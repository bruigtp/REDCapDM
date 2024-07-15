# Internal Functions for rd_transform Pre-processing
#'
#' Recalculate REDCap Calculated Fields
#' @description
#' This function recalculates each calculated field if the logic can be transcribed to R. Note that calculated fields containing smart-variables or variables from other events cannot be transcribed.
#'
#' The function returns the dataset and dictionary with the recalculated variables appended (named as the original field plus `_recalc`), along with a summary table of the recalculation results.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame  containing the correspondence of each event with each form.
#' @param exclude_recalc Character vector with the names of the variables that should not be recalculated. Useful for projects with time-consuming recalculations for certain calculated fields.
#' @importFrom rlang :=
#'
############Calculated functions############
recalculate <- function(data, dic, event_form = NULL, exclude_recalc = NULL){

  #Redefine rounding function (the round original function is troublesome for some special cases)
  round = function(x, digits) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^digits
    z*posneg
  }

  #Calculate for each calculated field the transcribed logic and if possible to transcribe recalculate it
  calc <- tibble::tibble(dic) %>%
    dplyr::filter(.data$field_type == "calc", !.data$field_name %in% exclude_recalc) %>%
    dplyr::mutate(
      calc = purrr::map(.data$field_name, function(x) {
        val <- data[, x]
        if(is.numeric(val)){
          as.numeric(val)
        } else {
          val
        }
      }),
      rlogic = purrr::map2(.data$choices_calculations_or_slider_labels, .data$field_name, function(x, y) {
        rlogic <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = x, var = y), silent = TRUE)
        if (!inherits(rlogic, "try-error")) {
          rlogic
        } else{
          NULL
        }
      }),
      trans = purrr::map_chr(.data$rlogic, function(x){
        if(!is.null(x)){
          x$rlogic
        }else{
          NA
        }
      }),
      recalc = purrr::map(.data$rlogic, function(x){
        if(!is.null(x)){
          x$eval
        }else{
          NULL
        }
      }),
      is_equal = purrr::map2_lgl(.data$calc, .data$recalc, function(x, y){
        if(!is.null(y)) {
          if(is.numeric(x) & is.numeric(y)){
            identical(round(x, 3), round(y, 3))
          }else if(all(is.na(x)) & all(is.na(y))) {
            TRUE
          } else {
            identical(x, y)
          }
        } else {
          NA
        }
      })
    ) %>%
    dplyr::select(-"rlogic")

  #Add this recalculated variables to data and dictionary and return both datasets

  calc_change <- calc %>%
    dplyr::filter(!is.na(.data$trans))

  if(nrow(calc_change) > 0){

    for(i in 1:nrow(calc_change)){

      name <- stringr::str_glue("{calc_change$field_name[i]}_recalc")

      data <- data %>%
        tibble::add_column("{name}" := calc_change$recalc[[i]], .after = as.character(calc_change$field_name[i]))

      add_row <- dic %>%
        dplyr::filter(.data$field_name == calc_change$field_name[i]) %>%
        dplyr::mutate(
          field_name = stringr::str_glue("{field_name}_recalc"),
          field_label = stringr::str_glue("{field_label} (Recalculate)")
        )

      dic <- rbind(dic, add_row)

    }

  }

  #Summary of the results

  report1 <- calc %>%
    dplyr::mutate(n = 1) %>%
    dplyr::summarise(
      trans = sum(!is.na(.data$trans)),
      N = sum(.data$n),
      no_trans = .data$N - .data$trans,
      no_equal = sum(!.data$is_equal, na.rm = TRUE),
    ) %>%
    dplyr::mutate(text1 = stringr::str_glue("{no_trans} ({round(no_trans*100/N, 2)}%)"),
                  text2 = stringr::str_glue("{no_equal} ({round(no_equal*100/trans, 2)}%)")
    ) %>%
    dplyr::select("Total calculated fields" = "N", "Non-transcribed fields" = "text1", "Recalculated different fields" = "text2")

  results <- knitr::kable(report1, "pipe", align = "ccc")

  report2 <- calc %>%
    dplyr::mutate(trans2 = ifelse(!is.na(.data$trans), "Yes", "No")) %>%
    dplyr::arrange(.data$trans2, .data$is_equal) %>%
    dplyr::select("field_name", "Transcribed?" = "trans2", "Is equal?" = "is_equal")

  results <- c(results, "\n", knitr::kable(report2, "pipe", align = "ccc"))

  list(
    data = data,
    dic = dic,
    results = results
  )

}


############Checkbox functions############

#' Transformation of Checkboxes with Branching Logic
#'
#' @description
#' This function inspects all the checkboxes in the study to determine if they have a branching logic. If a branching logic is present and its result is missing, the function will input a missing value into the checkbox. If `checkbox_na` is `TRUE`, the function will additionally input a missing value when the branching logic isn't satisfied, not just when it is missing. If a branching logic cannot be found or the logic cannot be transcribed due to the presence of smart variables, the variable is added to a list of reviewable variables that will be printed.
#'
#' The function returns the dataset with the transformed checkboxes and a table summarizing the results.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame  containing the correspondence of each event with each form.
#' @param checkbox_na Logical indicating if values of checkboxes with branching logic should be set to missing only when the branching logic is missing (`FALSE`), or also when the branching logic is not satisfied (`TRUE`). The default is `FALSE`.

transform_checkboxes <- function(data, dic, event_form = NULL, checkbox_na = FALSE){

  vars <- dic$field_name[dic$field_type=="checkbox"]
  results <- results1 <- results2 <- NULL
  caption <- "Checkbox variables advisable to be reviewed"

  if(length(vars) > 0){

    review <- NULL
    review2 <- NULL
    for(i in 1:length(vars)){

      #Identify all the variables in the database that belong to this checkbox (one for each one of the options)
      vars_data <- names(data)[grep(stringr::str_glue("{vars[i]}___"),names(data))]

      #Get the branching logic
      logic <- dic$branching_logic_show_field_only_if[dic$field_name==vars[i]]

      #If there is one
      if(!is.na(logic) & !logic %in% ""){

        #Translate the REDCap logic to r language using rd_rlogic function

        rlogic <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = logic, var = vars[i]), silent = TRUE)

        if(!inherits(rlogic, "try-error")){

          #Evaluate the logic
          rlogic_eval <- rlogic$eval

          #It will be missing when the logic isn't satisfied
          if(checkbox_na){
            for(j in 1:length(vars_data)){
              data[,vars_data[j]] <- ifelse(rlogic_eval,as.character(data[,vars_data[j]]),NA)
            }
          #It will be missing when the logic is missing
          }else{
            for(j in 1:length(vars_data)){
              data[,vars_data[j]] <- ifelse(!is.na(rlogic_eval),as.character(data[,vars_data[j]]),NA)
            }
          }


        }else{
          review2 <- c(review2, vars[i])
        }

      }else{

        #If there isn't a branching logic we put the variable to be reviewed
        review <- c(review,vars[i])

      }

    }

    #Summary with the results
    if(!is.null(review)){
      results1 <- tibble::tibble("Variables without any branching logic" = review)
      results <- knitr::kable(results1, "pipe", align = c("ccc"), caption = caption)
      if(!is.null(review2)){
        results <- c(results, "\n")
        caption <- NULL
      }
    }

    if(!is.null(review2)){
      results2 <- tibble::tibble("Variables with a logic that can't be transcribed" = review2)
      results <- c(results, knitr::kable(results2, "pipe", align = c("ccc"), caption = caption))
    }

  }else{
    warning("There isn't any checkbox variable in the dataset", call. = FALSE)
  }

  list(data = data,
       results = results)
}

#' Change Checkboxes Names to Option Names
#' @description
#' This function updates the names of checkboxes in the dataset and dictionary to reflect the names of their options.
#' @param data Dataset containing the REDCap data.
#' @param dic Dataset containing the REDCap dictionary.
#' @param labels Named character vector with the names of the variables in the data and their corresponding REDCap labels.
#' @param checkbox_labels Character vector specifying the names for the two options of each checkbox variable. The default is `c('No', 'Yes')`.

checkbox_names <- function(data, dic, labels, checkbox_labels = c("No", "Yes")){

  correspondence <- NULL

  #Identify checkbox variables:
  var_check <- names(data)[grep("___",names(data))]

  names_trim <- unique(gsub("___.*$", "", var_check))

  #Add to dictionary all variables from checkbox found in the data and remove the original one. Also, change the name both in the data and in the dictionary
  for(i in 1:length(names_trim)){

    svar_check <- grep(stringr::str_glue("^{names_trim[i]}___"), var_check, value = TRUE)

    label <- labels[svar_check]
    label <- gsub(".*choice=","",label)
    label <- gsub("\\)","",label)

    #Add rows with the name of all the variables for all the options
    new_row <- dic %>%
      dplyr::filter(.data$field_name==names_trim[i])

    #We have to repeat the original row n times (the length of svar_check)
    new_row <-  purrr::map_dfr(seq_len(length(svar_check)), ~new_row) %>%
      dplyr::mutate(
        field_name=svar_check,
        field_label=label,
        choices_calculations_or_slider_labels=stringr::str_glue("0, {checkbox_labels[1]} | 1, {checkbox_labels[2]}"))

    dic <- dic %>%
      tibble::add_row(new_row, .before = which(dic$field_name==names_trim[i])) %>%
      #Remove the original checkbox variable that was present in the beginning
      dplyr::filter(!.data$field_name%in%names_trim[i])

    label_name <- purrr::map_chr(label, ~janitor::make_clean_names(.x))
    label_name <- gsub("^x(\\d)","\\1",label_name)

    #Replace the original number corresponding on the option to the name of the choice
    out <- stringr::str_glue("{names_trim[i]}_{label_name}")

    #Trim name if it has more than 60 characters (if the option name is very large)
    out <- strtrim(out, 60)

    # We save the correspondence between the old name and the new one
    x <- cbind(gsub("___(.+)", "\\(\\1\\)", svar_check), out)
    correspondence <- rbind(correspondence, x)

    for(j in 1:length(out)) {
      out0 <- out[j]
      #Make unique if the name was already present in the names of the data
      out[j] <- utils::tail(make.unique(c(names(data), out[j])), 1)
      if(out[j] != out0) {
        warning(stringr::str_glue("The new name after the transformation of the checkox ({out0}) was already present in the data so it will be transformed to {out[j]}"))
      }
      #Change the name in the data and in the dictionary:
      names(data) <- dplyr::case_when(
        names(data) == svar_check[j] ~ out[j],
        TRUE ~ names(data)
      )

      dic <- dic %>%
        dplyr::mutate(
          field_name = dplyr::case_when(
            field_name == svar_check[j] ~ out[j],
            TRUE ~ field_name
          )
        )
    }
  }

  # Transforming the branching logic that contain checkboxes
  correspondence <- as.data.frame(correspondence)

  cats <- dic %>%
    dplyr::select("field_name", "choices_calculations_or_slider_labels") %>%
    dplyr::filter(.data$field_name %in% correspondence$out)

  cats <- cats %>%
    dplyr::mutate(choices_calculations_or_slider_labels = strsplit(.data$choices_calculations_or_slider_labels, "\\|")) %>%
    tidyr::unnest(.data$choices_calculations_or_slider_labels)

  cats <- cats %>%
    tidyr::separate(.data$choices_calculations_or_slider_labels, c("num", "cat"), ", ", extra = "merge") %>%
    dplyr::filter(.data$cat != "") %>%
    dplyr::mutate(num = trimws(.data$num), cat = trimws(.data$cat))

  cats <- merge(cats, correspondence, by.x = "field_name", by.y = "out")

  cats <- cats %>%
    dplyr::mutate(factor = paste0("[", .data$field_name, "]='", .data$cat, "'"),
                  V1 = stringi::stri_replace_all_fixed(cats$V1, c("(", ")"), c("\\(", "\\)"), vectorize_all = F),
                  redcap = paste0("\\[", .data$V1, "\\] ?=? ?'?", .data$num, "'?"),
                  redcap2 = paste0("\\[", .data$V1, "\\] ?<?>? ?'?", .data$num, "'?")) %>%
    dplyr::select(-"V1") %>%
    dplyr::arrange(dplyr::desc(.data$redcap))

  replace <- cats$factor
  names(replace) <- cats$redcap

  replace2 <- cats$factor
  names(replace2) <- cats$redcap2

  dic <- dic %>%
    dplyr::mutate(branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace),
                  branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace2))

  out <- list(
    data=data,
    dic=dic
  )

  out

}

############Structural functions##################

#' Creation of a Data Frame with Variables from All Forms of a Specified Event
#' @description
#' This function generates a nested dataset filtered by each event, containing only the variables associated with each event. It uses the provided data, dictionary, and event-form mapping. You can choose to return data for a specific event.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame containing the correspondence of each event with each form.
#' @param which Character string specifying an event if only data for that event is desired.

split_event <- function(data,dic,event_form,which=NULL){

  #We create event-variable correspondence from the variables in the dictionary:
  var_event <- event_form %>%
    dplyr::select("form_name"="form","redcap_event_name"="unique_event_name") %>%
    dplyr::right_join(dic[, c("form_name","field_name","field_type","branching_logic_show_field_only_if")], by = "form_name", multiple = "all") %>%
    #Remove variables that are not in the database (the descriptive type)
    dplyr::filter(.data$field_type!="descriptive", .data$field_name != "record_id") %>%
    tibble::as_tibble() %>%
    dplyr::select("redcap_event_name", "field_name")

  #Let's add the basic variables from redcap that are found in the data but not in the dictionary:
  basic_redcap_vars <- c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance","redcap_data_access_group","redcap_event_name.factor", "redcap_data_access_group.factor", "redcap_survey_identifier")

  #It can happen that one of these variables are not in the database for some projects
  basic_redcap_vars <- basic_redcap_vars[basic_redcap_vars %in% names(data)]

  var_event_add <- data.frame(redcap_event_name = NA, field_name = basic_redcap_vars)

  var_event <- rbind(var_event_add, var_event)

  #Now, let's see if there are more variables found in the var_event form than in the data, or the opposite:

  vars_more <- var_event$field_name[!var_event$field_name%in%names(data)]

  if(length(vars_more)>0){
    # print(vars_more)
    stop("There are more variables in the dictionary than in the data base so it's not possible to split by event. Transformation stops.", call. = FALSE)
  }

  vars_less <- names(data)[!names(data)%in%var_event$field_name]



  if(length(vars_less)>0){

    if (any(grepl("_complete$|_timestamp$", vars_less))) {
      mss <- dplyr::case_when(
        any(grepl("_complete$", vars_less)) & any(grepl("_timestamp$", vars_less)) ~ "c('_complete', '_timestamp')",
        any(grepl("_complete$", vars_less)) ~ "'_complete'",
        any(grepl("_timestamp$", vars_less)) ~ "'_timestamp'"
      )

      stop(stringr::str_glue("Transformation stops. Please use the argument `delete_pattern = {mss}` to delete the default variables created by REDCap and continue the transformation."), call. = FALSE)

    } else {
      # print(vars_less)
      stop("There are more variables in the data base than in the dictionary so it's not possible to split by event. Transformation stops.", call. = FALSE)
    }
  }

  #Let's create a database for every event filtering variables found in every event

  list_events <- stats::na.exclude(unique(var_event$redcap_event_name))

  ndata <- tibble::tibble("events"=list_events) %>%
    dplyr::mutate(
      vars = purrr::map(
        .data$events,
        ~ var_event %>%
          dplyr::filter(.data$redcap_event_name == .x) %>%
          dplyr::pull("field_name")
      ),
      df = purrr::map2(
        .data$events,
        .data$vars,
        ~ data %>%
          dplyr::filter(.data$redcap_event_name == .x) %>%
          dplyr::select(tidyselect::all_of(c(basic_redcap_vars, .y)))
      )
    )


  if(!is.null(which)){
    ndata$df[[which(ndata$events==which)]]
  }else{
    ndata
  }


}

#' Creation of a Data Frame with Variables from a Specified Form
#' @description
#' This function generates a nested dataset containing only the variables associated with each form, using the provided data, dictionary, and event-form mapping. You can choose to return data for a specific form.
#' @param data Data frame containing data from REDCap.
#' @param dic Data frame  containing the dictionary read from REDCap.
#' @param event_form Data frame  containing the correspondence of each event with each form.
#' @param which Character string specifying a form if only data for that form is desired.
#' @param wide Logical indicating if the dataset should be returned in a wide format (`TRUE`) or long format (`FALSE`).

split_form <- function(data, dic, event_form = NULL, which = NULL, wide=FALSE){

  #Check if the project is longitudinal or not:
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  if(longitudinal & is.null(event_form)){
    stop("To split the data by form the event_form has to be provided in a longitudinal project", call. = FALSE)
  }

  #Find basic REDCap variables found in the database
  basic_redcap_vars <- c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance","redcap_data_access_group","redcap_event_name.factor", "redcap_data_access_group.factor", "redcap_survey_identifier")

  basic_redcap_vars <- basic_redcap_vars[basic_redcap_vars%in%names(data)]

  #Previous to begin with the transformation let's find if there're the same variables in the data base than in the dictionary
  #But first we have to remove from the dictionary the descriptive variables that are found in the dictionary but not in the data
  dic <- dic %>%
    dplyr::filter(.data$field_type!="descriptive")

  vars_more <- dic$field_name[!dic$field_name%in%names(data)]

  if(length(vars_more)>0){
    stop("There're more variables in the dictionary than in the data base. Transformation stops", call. = FALSE)
  }

  vars_less <- names(data)[!names(data)%in%dic$field_name]
  #Remove the REDCap basic variables that are found in the database but not in the dictionary
  vars_less <- vars_less[!vars_less %in% basic_redcap_vars]

  if(length(vars_less)>0){

    if (any(grepl("_complete$|_timestamp$", vars_less))) {
      mss <- dplyr::case_when(
        any(grepl("_complete$", vars_less)) & any(grepl("_timestamp$", vars_less)) ~ "c('_complete', '_timestamp')",
        any(grepl("_complete$", vars_less)) ~ "'_complete'",
        any(grepl("_timestamp$", vars_less)) ~ "'_timestamp'"
      )

      stop(stringr::str_glue("Transformation stops. Please use the argument `delete_pattern = {mss}` to delete the default variables created by REDCap and continue the transformation."), call. = FALSE)

    } else {
      # print(vars_less)
      stop("There are more variables in the data base than in the dictionary so it's not possible to split by form. Transformation stops.", call. = FALSE)
    }
  }

  form <- unique(dic$form_name)

  if(longitudinal){
    ndata <- tibble::tibble("form"=form) %>%
      dplyr::mutate(
        events = purrr::map(.data$form, ~ event_form$unique_event_name[event_form$form == .x]),
        vars = purrr::map(.data$form, ~ dic$field_name[dic$form_name == .x])
      ) %>%
      #Collect variables from every event
      dplyr::mutate(df = purrr::map2(
        .data$events,
        .data$vars,
        ~ data %>%
          dplyr::filter(redcap_event_name %in% .x) %>%
          dplyr::select(tidyselect::all_of(unique(
            c(basic_redcap_vars, .y)
          )))
      ))
  }else{
    ndata <- tibble::tibble("form"=form) %>%
      dplyr::mutate(vars = purrr::map(.data$form, ~dic$field_name[dic$form_name == .x]),
                    #Add to vars the basic REDCap variables not found in the dictionary:
                    vars = purrr::map(.data$vars, ~unique(c(basic_redcap_vars, .x)))
      ) %>%
      #Collect variables from every event
      dplyr::mutate(df = purrr::map(.data$vars,  ~ data %>%
                                      dplyr::select(tidyselect::all_of(unique(c(basic_redcap_vars, .x))))))
  }


  if(wide){

    #We will add it with the order that repeated measures happen for every patient. There will be a row for each record-id.

    ndata <- ndata %>%
      dplyr::mutate(
        df = purrr::map2(.data$vars, .data$df, ~ .y %>%
                         dplyr::select(tidyselect::all_of(c("record_id", .x))) %>%
                         dplyr::group_by(.data$record_id) %>%
                         dplyr::mutate(id = 1:length(.data$record_id)) %>%
                         dplyr::ungroup() %>%
                         tidyr::pivot_wider(names_from = "id", values_from = -c("record_id", "id"))
             )
      )

  }

  if(!is.null(which)){

    ndata$df[[which(ndata$form==which)]]

  }else{

    ndata

  }


}

###############Other functions###############

#' Convert Variables to Factors
#' @description
#' This function converts all variables in the dataset to factors, except those specified in the `exclude` parameter.
#' @param data Data frame containing the REDCap data.
#' @param dic Data frame containing the REDCap dictionary.
#' @param exclude Character vector specifying the names of variables that should not be converted to factors. If `NULL`, all variables will be converted.

to_factor <- function(data, dic, exclude = NULL){

  #We need redcap_event_name to have the original values so we exclude of the conversion the variable redcap_event_name.factor. Also for redcap_data_access_group if present

  keep <- c("redcap_event_name.factor", "redcap_data_access_group.factor")
  keep_factors <- data %>%
    dplyr::select(keep[keep %in% names(data)])

  data$redcap_event_name.factor <- NULL
  data$redcap_data_access_group.factor <- NULL

  factors <- names(data)[grep("\\.factor$",names(data))]
  factors <- gsub("\\.factor$","",factors)

  #Exclude those variables that we don't want to convert to factors
  factors <- factors[!factors %in% exclude]

  data <- data %>%
    #Assign to the non factor variable the factor one and remove the later
    dplyr::mutate(dplyr::across(tidyselect::all_of(factors), ~ get(stringr::str_glue("{dplyr::cur_column()}.factor")))) %>%
    dplyr::select(-tidyselect::ends_with(".factor")) %>%
    tibble::add_column("redcap_event_name.factor" = keep_factors$redcap_event_name.factor, .after = "redcap_event_name")

  if (length(factors) > 0) {

    # Transform branching logics in the dictionary for variables that are currently factors
    cat_factors <- dic %>%
      dplyr::select("field_name", "choices_calculations_or_slider_labels") %>%
      dplyr::filter(.data$field_name %in% factors)

    cat_factors <- cat_factors %>%
      dplyr::mutate(
        choices_calculations_or_slider_labels = strsplit(.data$choices_calculations_or_slider_labels, "\\|")
      ) %>%
      tidyr::unnest(.data$choices_calculations_or_slider_labels)

    cat_factors <- cat_factors %>%
      tidyr::separate(.data$choices_calculations_or_slider_labels,
                      c("num", "cat"),
                      ", ",
                      extra = "merge") %>%
      dplyr::filter(.data$cat != "") %>%
      dplyr::mutate(num = trimws(.data$num), cat = trimws(.data$cat))

    cat_factors <- cat_factors %>%
      dplyr::mutate(
        redcap = paste0("\\[", .data$field_name, "\\] ?=? ?'?", .data$num, "'?"),
        redcap2 = paste0("\\[", .data$field_name, "\\] ?<?>? ?'?", .data$num, "'?"),
        factor = paste0("[", .data$field_name, "]='", .data$cat, "'"),
        factor2 = paste0("[", .data$field_name, "]<>'", .data$cat, "'"),
      ) %>%
      dplyr::arrange(.data$field_name, dplyr::desc(.data$num))

    replace <- cat_factors$factor
    names(replace) <- cat_factors$redcap

    replace2 <- cat_factors$factor2
    names(replace2) <- cat_factors$redcap2

    dic <- dic %>%
      dplyr::mutate(branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace),
                    branching_logic_show_field_only_if = stringr::str_replace_all(.data$branching_logic_show_field_only_if, replace2))

  }



  if("redcap_data_access_group" %in% names(data)){
    list(data = data %>%
                    tibble::add_column("redcap_data_access_group.factor" = keep_factors$redcap_data_access_group.factor, .after = "redcap_data_access_group"),
         dic = dic)
  }else{
    list(data = data, dic = dic)
  }
}

#' Fill Rows with Values from One Event
#' @description
#' This function fills all rows in the dataset with the value of a particular variable in a specified event. It is an auxiliary function used in the `rd_rlogic` function.
#' @param which_event String specifying the name of the event.
#' @param which_var String specifying the name of the variable.
#' @param data Dataset containing the REDCap data.

fill_data <- function(which_event, which_var, data){

  if(which_event %in% data$redcap_event_name){

    fill_values <- data %>%
      dplyr::select("record_id", "redcap_event_name", tidyselect::all_of(which_var)) %>%
      dplyr::rename(var = which_var) %>%
      dplyr::group_by(.data$record_id) %>%
      dplyr::mutate(
        var = dplyr::case_when(
          .data$redcap_event_name != which_event ~ NA,
          TRUE ~ .data$var
        ),
        #Only the first value if the event is repeated
        var = stats::na.exclude(unique(.data$var))[1]
      ) %>%
      tidyr::fill("var", .direction = "downup") %>%
      dplyr::pull("var")

    data[,which_var] <- fill_values

    data

  }else{

    stop("The logic can't be evaluated after the translation")

  }

}

