#' Read REDCap data
#'
#' @description
#' This function allows users to read datasets from a REDCap project into R for analysis, either by exporting the data or via an API connection.
#'
#' The REDCap API is an interface that allows communication with REDCap and the server without going through the interactive REDCap interface.
#'
#' [Important] In order to read the exported data from REDCap, please follow these steps:
#'
#' * * Use REDCap's 'Export Data' function
#' * * Select the 'R Statistical Software' format.
#' * * REDCap will then generate two files:
#' * * * * A CSV file containing all the observations of the REDCap project.
#' * * * * An R file containing the necessary R code to complete each variable's information and import them.
#' * * These files, along with the dictionary and event-mapping, must be located in the same directory.
#'
#' @note If you will give further use to the package, we advise you to use the argument `dic_path` to read your dictionary, as all other functions need it in order to run properly.
#'
#' @param data_path Character string with the path of the R file from which the dataset will be read (.
#' @param dic_path Character string with the path of the dictionary.
#' @param event_path Character string with the pathname of the file containing the correspondence between each event and each form (it can be downloaded through the `Designate Instruments for My Events` tab inside the `Project Setup` section of REDCap)
#' @param uri The URI (Uniform Resource Identification) of the REDCap project.
#' @param token Character vector containing the generated token.
#' @param filter_field Character vector with the fields of the REDCap project desired to be imported into R (import via API connection only).
#' @return List containing the dataset and the dictionary of the REDCap project. If the event_path is specified, it will also contain a third element with the correspondence of the events & forms of the project.
#'
#'
#' @examples
#' \dontrun{
#' # Exported files from REDCap
#'
#' dataset <- redcap_data(data_path = "C:/Users/username/example.r",
#'                        dic_path = "C:/Users/username/example_dictionary.csv",
#'                        event_path = "C:/Users/username/events.csv")
#'
#' # API connection
#'
#' dataset_api <- redcap_data(uri = "https://redcap.idibell.cat/api/",
#'                            token = "55E5C3D1E83213ADA2182A4BFDEA") # This token is fictitious
#'
#' }
#' @export

redcap_data<-function(data_path = NA, dic_path = NA, event_path = NA, uri = NA, token = NA, filter_field = NA)
  {
  oldwd <- getwd()
  on.exit(setwd(oldwd))

  # Warning: data_path, dic_path and another argument are specified.
  if(all(!c(data_path, dic_path) %in% NA) & any(!c(token, uri) %in% NA)){
    stop("Too many arguments, if you want to read exported data from REDCap use only the arguments data_path and dic_path", call. = FALSE)
  }

  # Warning: token, uri and another argument are specified.
  if(all(!c(token, uri) %in% NA) & any(!c(data_path, dic_path) %in% NA)){
    stop("Too many arguments, if you want to read data from REDCap through an API connection use only the arguments uri and token.", call. = FALSE)
  }

  # Warning: either uri or token is specified alone
  if ((!is.na(uri) & is.na(token)) | (is.na(uri) & !is.na(token))) {
    stop("If you want to read data from REDCap through an API connection, both 'uri' and 'token' arguments must be provided.", call. = FALSE)
  }

  # Read data, dictionary and event-form mapping in case of exported data.
  if(all(!c(data_path, dic_path) %in% NA) & all(c(token, uri) %in% NA)){

    # Evaluate the extension of the data_path
    extension_data <- tools::file_ext(data_path)

    if (!extension_data %in% c("R", "r")) {
      stop("Unsupported file format. Only R files are supported. Please specify the downloaded R file from REDCap within this argument.")
    }

    # Read data
    tmp_env <- new.env()
    file.lines <- scan(data_path, what = character(), skip = 2, sep = '\n', quiet = TRUE)
    file.lines.collapsed <- paste(file.lines, collapse = '\n')
    command <- paste0("dirname(parent.frame(2)$", "data_path", ")")
    setwd(eval(parse(text = command)))
    source(textConnection(file.lines.collapsed), local = tmp_env, encoding = "UTF-8")
    data <- get("data", envir = tmp_env)
    if (names(data)[1]!="record_id") {
      names(data)[1] <- "record_id"
    }

    # Read dictionary
    setwd(oldwd)

    # Evaluate the extension of the dictionary_path
    extension_dic <- tools::file_ext(dic_path)

    if (extension_dic == "xlsx") {

      # Read XLSX file
      dic <- openxlsx::read.xlsx(dic_path, colNames = F, detectDates = T)

    } else if (extension_dic == "csv") {

      # Read CSV file
      dic <- utils::read.csv(dic_path, encoding = "UTF-8", header = FALSE)

    } else {

      stop("Unsupported file format. Only XLSX and CSV are supported.")

    }

    # Changing names of the first column and first observation
    names(dic) <- dic[1,]
    dic <- dic[-1,]
    names(dic) <- janitor::make_clean_names(names(dic))
    names(dic)[1] <- "field_name"
    if (dic[1,1]!="record_id") {
      dic[1,1] <- "record_id"
    }

    # Remove descriptive variables from dictionary
    if ("descriptive" %in% dic$field_type) {
      dic <- dic %>% dplyr::filter(!.data$field_type %in% "descriptive")
    }


    # Indicator of longitudinal projects
    longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

    #Read event file
    if(!is.na(event_path)){

      setwd(oldwd)

      # Evaluate the extension
      extension <- tools::file_ext(event_path)

      if (extension == "xlsx") {

        # Read XLSX file
        event_form <- openxlsx::read.xlsx(event_path, detectDates = T)

      } else if (extension == "csv") {

        # Read CSV file
        event_form <- utils::read.csv(event_path, encoding = "UTF-8")

      } else {

        stop("Unsupported file format. Only XLSX and CSV are supported.")

      }

      data_def <- list(data = data, dictionary = dic, event_form = event_form)

    }else{

      #If no event is specified and the project is longitudinal
      if(longitudinal){
        warning("The project contains more than one event. You might want to load the event-form correspondence using the argument event_path.")
      }

      data_def <- list(data = data, dictionary = dic)
    }

  }

  # Read data, dictionary and event-form mapping in case of an API connection.
  if(all(!c(token, uri) %in% NA) & all(c(data_path, dic_path) %in% NA)){

    # Message
    message("Importing in progress...")

    # First read the labels

    ## Error SSL peer certificate (Github issue #6)

    tryCatch({
      if (all(filter_field %in% NA)) {

        labels <- suppressMessages(REDCapR::redcap_read(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label = "label", raw_or_label_headers = "label", export_data_access_groups = TRUE)$data)

      } else {

        labels <- suppressMessages(REDCapR::redcap_read(redcap_uri = uri, token = token, verbose = F, raw_or_label = "label", raw_or_label_headers = "label", export_data_access_groups = TRUE, fields = filter_field)$data)

      }
    },
    error = function(e) {
      if (grepl("SSL peer certificate", e$message)) {
        stop("Unable to establish a secure connection due to an SSL certificate problem.\nConsider adding the following line of code to bypass SSL certificate verification: httr::with_config(httr::config(ssl_verifypeer = FALSE), ... <- readcap_data(...)).\n", call. = F)
      } else {
        stop(e)
      }
    })

    if (nrow(labels) > 0) {
      names(labels)[1] <- "record_id"
    } else {
      stop("Data retrieval is currently unavailable. Please check the status of the REDCap server, confirm the existence of records within the project and verify that you have the necessary data export and API permissions to perform this operation.", call. = F)
    }

    # Save the factor version of the default variables of redcap
    redcap_names <- names(labels %>%
                            dplyr::select(dplyr::any_of(c("Event Name", "Repeat Instrument", "Data Access Group"))))

    default_names <- data.frame(fac = redcap_names) %>%
      dplyr::mutate(corres = dplyr::case_when(fac %in% "Event Name" ~ "redcap_event_name.factor",
                                              fac %in% "Repeat Instrument" ~ "redcap_repeat_instrument.factor",
                                              fac %in% "Data Access Group" ~ "redcap_data_access_group.factor"))

    rename_redcap <- default_names$fac
    names(rename_redcap) <- default_names$corres

    main_vars <- labels %>%
      dplyr::mutate_at(redcap_names[!redcap_names %in% "Repeat Instrument"], ~forcats::fct_inorder(.)) %>%
      dplyr::rename(dplyr::all_of(rename_redcap)) %>%
      dplyr::select("record_id", default_names$corres)

    # Remove the "...number" suffixes from the labels
    labels <- gsub("\\.{3}\\d+$", "", names(labels))


    # Message
    message("Almost done...")

    # Read data using the API connection

    if (all(filter_field %in% NA)) {
      data_api <- REDCapR::redcap_read_oneshot(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label = "raw", export_data_access_groups = TRUE)$data
    } else {
      data_api <- REDCapR::redcap_read_oneshot(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label = "raw", export_data_access_groups = TRUE, fields = filter_field)$data
    }

    if (nrow(data_api) > 0) {
      names(data_api)[1] <- "record_id"
    } else {
      stop("Observational data retrieval is currently unavailable. Please verify the status of the REDCap server or confirm the existence of records within the project.", call. = F)
    }

    # Read dictionary using the API connection
    dic_api <- REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = FALSE)$data

    names(dic_api)[1] <- "field_name"

    ## Making sure the names of both dictionaries(exported data and API connection) match
    names(dic_api)[names(dic_api) %in% c("select_choices_or_calculations", "branching_logic", "question_number")] <- c("choices_calculations_or_slider_labels", "branching_logic_show_field_only_if", "question_number_surveys_only")

    # Apply labels
    data_api <- as.data.frame(purrr::map2(data_api, labels, ~labelled::set_variable_labels(.x, .y, .strict = FALSE)))

    # Remove descriptive variables from dictionary
    if ("descriptive" %in% dic_api$field_type) {
      dic_api <- dic_api %>% dplyr::filter(!.data$field_type %in% "descriptive")
    }

    # If filter_field is described, filter the variables in the dictionary
    if (!all(filter_field %in% NA)) {
      dic_api <- dic_api %>% dplyr::filter(.data$field_name %in% filter_field)
    }

    # Identify checkboxes fields and convert them to factor using the dictionary as guide

    if (sum(dic_api$field_type %in% "checkbox") > 0) {

      var_check <- names(data_api)[grep("___", names(data_api))]

      data_api <- data_api %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(var_check), ~ factor(., levels = c("0", "1"), labels = c("Unchecked", "Checked")), .names = "{col}.factor"))

    }

    # Identify radio buttons and dropdown fields and convert them to factor using the dictionary as guide

    if (sum(dic_api$field_type %in% c("radio", "dropdown")) > 0) {

      var_radio <- dic_api %>%
        dplyr::filter(.data$field_type %in% c("radio", "dropdown")) %>%
        dplyr::select("field_name", "field_type", "choices_calculations_or_slider_labels")%>%
        dplyr::mutate(factor = purrr::map(.data$choices_calculations_or_slider_labels, ~stringr::str_split(.x, "\\|") %>% unlist %>% trimws),
                      levels = purrr::map(factor, ~gsub(",.*", "", .x)),
                      labels = purrr::map(factor, ~gsub("^[^,]*,\\s*", "", .x)))

      for (i in var_radio$field_name) {
        tryCatch(
          {
            data_api[[stringr::str_glue("{i}.factor")]] <- factor(data_api[[i]],
                                                                  levels = c(var_radio$levels[[which(var_radio$field_name %in% i)]]),
                                                                  labels = c(var_radio$labels[[which(var_radio$field_name %in% i)]]))
          },
          error = function(e) {
            warning(stringr::str_glue("The following variable could not be replicated in its factor version: {i}. Please manually create a factor version of this variable named '{i}.factor' to properly execute the rd_transform() function."), call. = F)
          }
        )
      }
    }

    # Join the main_vars to the imported data

    data_api <- data_api %>%
      dplyr::bind_cols(main_vars %>% dplyr::select(-"record_id"))

    # Indicator of longitudinal projects
    longitudinal <- ifelse("redcap_event_name" %in% names(data_api), TRUE, FALSE)

    # Read event file
    if(!is.na(event_path)){

      # Warning: event_path not necessary while using API connection
      warning("The event_path argument is not necessary as the event-form correspondence can be automatically read with the API connection")

      setwd(oldwd)

      # Evaluate the extension
      extension <- tools::file_ext(event_path)

      if (extension == "xlsx") {

        # Read XLSX file
        event_form <- openxlsx::read.xlsx(event_path, detectDates = T)

      } else if (extension == "csv") {

        # Read CSV file
        event_form <- utils::read.csv(event_path, encoding = "UTF-8")

      } else {

        stop("Unsupported file format. Only XLSX and CSV are supported.")

      }

      data_def <- list(data = data_api,
                       dictionary = dic_api,
                       event_form = event_form)
    } else {

      # If the event file is not specified, the function reads it using the API connection (in case of longitudinal projects)
      if(longitudinal){

        event_form <- as.data.frame(REDCapR::redcap_event_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data)

        data_def <- list(data = data_api,
                         dictionary = dic_api,
                         event_form = event_form)

      } else {

        data_def <- list(data = data_api,
                         dictionary = dic_api)

      }

    }

    # Message
    message("Done!")
  }

  # Specifying the "UTF-8" encoding to each character column of the data
  for (i in 1:length(data_def$data)) {
    if(is.character(data_def$data[, i])){
      suppressWarnings(data_def$data[, i] <- stringr::str_conv(data_def$data[, i], "UTF-8"))
    }
  }

  # Output
  return(data_def)

}
