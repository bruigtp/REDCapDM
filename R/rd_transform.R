#' Transformation of the Raw Data
#'
#' This function transforms the raw REDCap data read by the `redcap_data` function. It returns the transformed data and dictionary, along with a summary of the results of each step.
#' @param ... Output of the `redcap_data` function, which is a list containing the data frames of the data, dictionary and event_form (if needed) of the REDCap project.
#' @param data Data frame containing the data read from REDCap. If the list is specified, this argument is not necessary.
#' @param dic Data frame  containing the dictionary read from REDCap. If the list is specified, this argument is not necessary.
#' @param event_form Data frame containing the correspondence of each event with each form. If the list is specified, this argument is not necessary.
#' @param checkbox_labels Character vector with the names for the two options of every checkbox variable. Default is `c('No', 'Yes')`.
#' @param checkbox_na Logical indicating if checkboxes values with branching logic should be set to missing only when the branching logic is missing (`FALSE`), or also when the branching logic isn't satisfied (`TRUE`). The default is `FALSE`.
#' @param exclude_recalc Character vector with the names of variables that should not be recalculated. Useful for projects with time-consuming recalculations of certain calculated fields.
#' @param exclude_to_factor Character vector with the names of variables that should not be transformed to factors.
#' @param delete_vars Character vector specifying the variables to exclude.
#' @param delete_pattern Character vector specifying the regex pattern for variables to be excluded. By default, variables ending with `_complete` and `_timestamp` will be removed.
#' @param final_format Character string indicating the final format of the data. Options are `raw`, `by_event` or `by_form`. `raw` (default) returns the transformed data in its original structure, `by_event` returns it as a nested data frame by event, and `by_form` returns it as a nested data frame by form.
#' @param which_event Character string indicating a specific event to return if the final format is  `by_event`.
#' @param which_form Character string indicating a specific form to return if the final format is `by_form`.
#' @param wide Logical indicating if the data split by form (if selected) should be in a wide format (`TRUE`) or a long format (`FALSE`).
#' @return A list with the transformed dataset, dictionary, event_form, and the results of each transformation step.
#'
#' @examples
#' # Basic transformation
#' rd_transform(covican)
#'
#' # For customization of checkbox labels (example)
#' rd_transform(covican,
#'              checkbox_labels = c("Not present", "Present"))
#'
#' @export

rd_transform <- function(..., data = NULL, dic = NULL, event_form = NULL, checkbox_labels = c("No", "Yes"), checkbox_na = FALSE, exclude_recalc = NULL, exclude_to_factor = NULL, delete_vars = NULL, delete_pattern = c("_complete", "_timestamp"), final_format = "raw", which_event = NULL, which_form = NULL, wide = NULL){

  project <- c(...)

  results <- NULL
  ind <- 1

  if(!is.null(project)){
    if(!is.null(data)){
      warning("Data has been specified twice so the function will not use the information in the data argument.")
    }

    if(!is.null(dic)){
      warning("Dictionary has been specified twice so the function will not use the information in the dic argument.")
    }

    data <- project$data
    dic <- project$dictionary
    dic_ori <- dic

    if("event_form" %in% names(project)){
      if(!is.null(event_form)){
        warning("The event-form has been specified twice so the function will not use the information in the event_form argument.")
      }
      event_form <- project$event_form
    }
  }

  if(is.null(data) | is.null(dic)){
    stop("No data/dictionary was provided")
  }


  #Check if the project is longitudinal (has more than one event) or not:
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  if(final_format == "by_event" & is.null(event_form)){
    stop("To split the data by event the event_form has to be provided", call. = FALSE)
  }

  #If the project is not longitudinal 'by_form' can be used without event_form:
  if(final_format == "by_form" & is.null(event_form) & longitudinal){
    stop("To split the data by form the event_form has to be provided in a longitudinal project", call. = FALSE)
  }

  if(!is.null(which_event) & final_format != "by_event"){
    stop("Which event has been specified but the final format is not to split the data by event", call. = FALSE)
  }

  if(!is.null(which_form) & final_format != "by_form"){
    stop("Which form has been specified but the final format is not to split the data by form", call. = FALSE)
  }

  if(!is.null(wide) & final_format != "by_form"){
    stop("The argument wide has been specified but the final format is not to split the data by form", call. = FALSE)
  }

  if(!final_format %in% c("raw", "by_event", "by_form")){
    stop("final_format argument has to be one of the following: 'raw', 'by_event', 'by_form'", call. = FALSE)
  }

  #If the project is longitudinal and the event hasn't been specified:
  if(longitudinal & is.null(event_form)){
    warning("The project contains more than one event. For a complete transformation is recommended to include the event-form correspondence.")
  }

  #Check if the project has repeated instruments
  if("redcap_repeat_instrument" %in% names(data)) {
    repeat_instrument <- dplyr::case_when(
      any(!is.na(data$redcap_repeat_instrument)) ~ TRUE,
      TRUE ~ FALSE
    )
  } else {
    repeat_instrument <- FALSE
  }

  message("Transformation in progress...")

  labels <- purrr::map_chr(data, function(x) {
    lab <- attr(x, "label")
    if (!is.null(lab)) {
      lab
    } else {
      ""
    }
  })

  #Change the labelled class of each column but don't remove the label:
  data <- data %>%
    dplyr::mutate_all(function(x){
      class(x) <- setdiff(class(x), "labelled")
      x
    })

  # Delete selected variables
  results <- c(results, stringr::str_glue("{ind}. Removing selected variables\n"))
  ind <- ind + 1

  if(!is.null(delete_vars)){

    for(i in 1:length(delete_vars)) {
      data <- data %>%
        dplyr::select(!delete_vars[i])

      if (paste0(delete_vars[i], ".factor") %in% names(data)) {
        data <- data %>%
          dplyr::select(!paste0(delete_vars[i], ".factor"))
      }

      dic <- dic %>%
        dplyr::filter(.data$field_name != delete_vars[i])

    }

  }

  # Delete variables that contain specific patterns
  results <- c(results, stringr::str_glue("\n\n{ind}. Deleting variables that contain some patterns\n"))
  ind <- ind + 1

  if(!is.null(delete_pattern)){

    for(i in 1:length(delete_pattern)){

      if(delete_pattern[i] == "_complete"){

        data <- data %>%
          dplyr::select(!tidyselect::ends_with(c("_complete", "_complete.factor")))

        dic <- dic %>%
          dplyr::filter(!grepl("_complete$", .data$field_name))

      }else if(delete_pattern[i] == "_timestamp"){

        data <- data %>%
          dplyr::select(!tidyselect::ends_with(c("_timestamp", "timestamp.factor")))

        dic <- dic %>%
          dplyr::filter(!grepl("_timestamp$", .data$field_name))

      }else{

        data <- data %>%
          dplyr::select(!tidyselect::contains(delete_pattern[i]))

        dic <- dic %>%
          dplyr::filter(!grepl(delete_pattern[i], .data$field_name))
      }

    }

  }


  #Change the format of dates
  #Identify dates that have the tag "date_"/"datetime_"/"datetime_seconds_" in redcap. It will have always the format "Y-M-D" in any case
  var_date <- dic %>%
    dplyr::filter(grepl("^date_", .data$text_validation_type_or_show_slider_number)) %>%
    dplyr::pull(.data$field_name)

  var_date_valid <- dplyr::select(data, dplyr::all_of(var_date)) |> 
    purrr::keep(~ inherits(.x, "Date")) |> 
    names()

  var_date <- setdiff(var_date, var_date_valid)

  var_datetime <- dic %>%
    dplyr::filter(grepl("^datetime_", .data$text_validation_type_or_show_slider_number)) %>%
    dplyr::pull(.data$field_name)

  var_datetime_valid <- dplyr::select(data, dplyr::all_of(var_datetime)) |> 
    purrr::keep(~ inherits(.x, "POSIXct")) |> 
    names()

  var_datetime <- setdiff(var_datetime, var_datetime_valid)

  data <- data %>%
    dplyr::mutate_at(var_date, as.Date) %>%
    dplyr::mutate_at(var_datetime, function(x) {
      x <- dplyr::case_when(
        x == "" ~ NA,
        TRUE ~ x
      )
      as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
    })

    dic <- dic %>%
      dplyr::mutate(branching_logic_show_field_only_if = dplyr::case_when(is.na(branching_logic_show_field_only_if) ~ "",
                                                                          TRUE ~ branching_logic_show_field_only_if))

  if (!repeat_instrument) {
    #Recalculate calculated fields (previous to transforming factors and other preprocessing)
    #It wil create duplicate variables of each calculated field with "_recalc" in the end and the recalculated value

    results <- c(results, stringr::str_glue("\n\n{ind}. Recalculating calculated fields and saving them as '[field_name]_recalc'\n"))
    ind <- ind + 1

    #If the project is longitudinal and the event hasn't been specified no recalculation is possible
    if(longitudinal & is.null(event_form)){

      results <- c(results, "\nNo recalculation is possible as the project has more than one event and the event-form correspondence has not been specified\n")

    }else{

      recalc <- recalculate(data, dic, event_form, exclude_recalc)

      data <- recalc$data
      dic <- recalc$dic

      results <- c(results, recalc$results)

    }
  }


  if (!repeat_instrument) {
    if(checkbox_na){
      results <- c(results, stringr::str_glue("\n\n{ind}. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options. For checkboxes that have a branching logic, when the logic isn't satisfied or it's missing their values will be set to missing\n\n"))
    }else{
      results <- c(results, stringr::str_glue("\n\n{ind}. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options. For checkboxes that have a branching logic, when the logic is missing their values will be set to missing\n\n"))
    }
  } else {
    results <- c(results, stringr::str_glue("\n\n{ind}. Transforming checkboxes: changing their values to No/Yes and changing their names to the names of its options.\n"))
  }

  ind <- ind + 1

  #Identify checkbox variables:
  var_check<-names(data)[grep("___",names(data))]

  #Remove .factor:
  var_check_factors <- var_check[grep(".factor$",var_check)]

  if (length(var_check_factors) > 0) {
    data <- data %>%
      dplyr::select(-tidyselect::all_of(var_check_factors))

    var_check <- var_check[!grepl(".factor$",var_check)]
  } else {
    if (length(var_check) > 0){
      data <- data %>%
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(var_check),
          ~ dplyr::case_when(.x == "Unchecked" ~ 0,
                             .x == "Checked" ~ 1,
                             TRUE ~ NA)
        ))
    }
  }

  #If there is some checkbox:
  if(length(var_check) > 0){

    #If the event_form is not provided and the project is longitudinal
    if(is.null(event_form) & longitudinal) {

        results <- c(results, "\nBranching logic evaluation is not possible as the project has more than one event and the event-form correspondence has not been specified\n")

    } else {

      if (!repeat_instrument) {

        #Transform missings of checkboxes with branching logic:
        trans <- transform_checkboxes(data = data, dic = dic, event_form = event_form, checkbox_na = checkbox_na)

        data <- trans$data

        results <- c(results, trans$results)
      }

    }

    #Transform them to No/Yes:

    data <- data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(var_check),
        ~ factor(.x, levels = 0:1, labels = checkbox_labels)
      ))

    # Reapply the original labels after transforming the checkboxes to factors
    data <- data |>
      labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)

    #Change the variable names and their branching logic:

    data_dic <- checkbox_names(data, dic, labels, checkbox_labels)

    data <- data_dic$data
    dic <- data_dic$dic


  }else{

    results <- c(results, "\nNo checkboxes are found in the data\n")

  }

  #Replace original variables with their factor version except for redcap_event_name and redcap_data_access_group
  #If we dont want to convert another additional variable to factor we can specify it with the exclude argument:

  factors <- names(data)[grep("\\.factor$",names(data))]

  if (length(factors) > 0) {
    results <- c(results, stringr::str_glue("\n\n{ind}. Replacing original variables for their factor version"))
    ind <- ind + 1

    data_dic <- to_factor(data, dic, exclude = exclude_to_factor) # This step also transforms the branching logic of the factors

    data <- data_dic$data
    dic <- data_dic$dic
  }

  #Fix variables that instead of missing have an empty field (text variables, etc.):
  data <- data %>%
    #Fix characters:
    dplyr::mutate_if(is.character,  ~ gsub("^$", NA, .x)) %>%
    #Fix factors:
    dplyr::mutate_if(is.factor,function(x){levels(x)[levels(x)==""] <- NA; x})

  if (!repeat_instrument) {
    # Transform the branching logic from the dictionary which is in REDCap logic (raw) into R logic
    results <- c(results, stringr::str_glue("\n\n{ind}. Converting every branching logic in the dictionary into R logic"))
    ind <- ind + 1

    pos <- which(!dic$branching_logic_show_field_only_if %in% "")
    logics <- NULL

    for (i in pos) {

      evaluation <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = dic$branching_logic_show_field_only_if[i], var = dic$field_name[i])$rlogic, silent = T)

      if (!inherits(evaluation, "try-error")) {

        dic$branching_logic_show_field_only_if[i] <- rd_rlogic(data = data, dic = dic, event_form = event_form, logic = dic$branching_logic_show_field_only_if[i], var = dic$field_name[i])$rlogic

      } else {

        logics <- rbind(logics, dic$field_name[i])

      }
    }

    if (!is.null(logics)) {

      tabla <- tibble::tibble("Variables" = logics)
      results <- c(results, "\n", knitr::kable(tabla, "pipe", align = c("ccc"), caption = "Variables with unconverted branching logic"))

    }
  }


  #Arrange our dataset by record_id and event (will keep the same order of events as in redcap)
  if(longitudinal) {
    if("redcap_event_name.factor" %in% names(data)) {
      data <- data %>%
        dplyr::arrange(factor(.data$record_id, levels = unique(.data$record_id)), .data$redcap_event_name.factor)
    } else {
      data <- data %>%
        dplyr::arrange(factor(.data$record_id, levels = unique(.data$record_id)), .data$redcap_event_name)
    }
  }
  
  # Reapply labels to the modified dataset
  data <- data |>
    labelled::set_variable_labels(.labels = labels |> as.list(), .strict = FALSE)


  #If an event_form is specified or if the project has only one event and by_form has been specified
  if(!is.null(event_form) | (final_format == "by_form" & !longitudinal)){

    if(!is.null(event_form)){
      var_noevent <- dic$field_name[! dic$form_name %in% event_form$form]

      if(length(var_noevent) > 0){

        results <- c(results, stringr::str_glue("\n\n{ind}. Erasing variables from forms that are not linked to any event"))
        ind <- ind + 1

        var_noevent <- var_noevent[var_noevent %in% names(data)]
        data <- data %>%
          dplyr::select(-var_noevent)
        dic <- dic %>%
          dplyr::filter(! .data$field_name %in% var_noevent)
      }

    }

    #Final arrangment

    if(final_format == "by_event"){

      results <- c(results,stringr::str_glue("\n\n{ind}. Final arrangment of the data by event"))
      ind <- ind + 1

      if(is.null(which_event)){

        data <- split_event(data, dic, event_form)

      }else{

        data <- split_event(data,dic,event_form,which=which_event)

      }

    }else if(final_format == "by_form"){

      results <- c(results, stringr::str_glue("{ind}. Final arrangment of the data by form"))
      ind <- ind + 1

      if(is.null(wide)){
        wide <- FALSE
      }

      if(is.null(which_form)){

        if(longitudinal){
          data <- split_form(data, dic, event_form, which = NULL, wide)
        }else{
          data <- split_form(data, dic, which = NULL, wide)
        }

      }else{
        if(longitudinal){
          data <- split_form(data, dic, event_form, which=which_form, wide)
        }else{
          data <- split_form(data, dic, which=which_form, wide)
        }
      }

    }

    if(!is.null(event_form)) {
      list(
        data = data,
        dictionary = dic,
        event_form = event_form,
        results = stringr::str_glue("{results}")
      )
    } else {
      list(
        data = data,
        dictionary = dic,
        results = stringr::str_glue("{results}")
      )
    }


  }else {

    if(!is.null(event_form)) {
      list(
        data = data,
        dictionary = dic,
        event_form = event_form,
        results = stringr::str_glue("{results}")
      )
    } else {
      list(
        data = data,
        dictionary = dic,
        results = stringr::str_glue("{results}")
      )
    }

  }


}
