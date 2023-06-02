#' Read REDCap data
#'
#' @description
#' This function allows users to read datasets from a REDCap project into R for analysis, either via export of the data or via an API connection.
#'
#' The REDCap API is an interface that allows communication with REDCap and server without going through the interactive REDCap interface.
#'
#' @note If you will give further use to the package, we advise you to use the argument 'dic_path' to read your dictionary, as all other functions need it in order to run properly.
#'
#' @param data_path Character string with the pathname of the R file to read the dataset from.
#' @param dic_path Character string with the pathname of the dictionary.
#' @param event_path Character string with the pathname of the file containing the correspondence between each event and each form (it can be downloaded through the `Designate Instruments for My Events` tab inside the `Project Setup` section of REDCap)
#' @param uri The URI (Uniform Resource Identification) of the REDCap project.
#' @param token Character vector with the generated token.
#' @return List containing the dataset and the dictionary of the REDCap project. If the event_path is specified, it will also contain a third element with the correspondence of the events & forms of the project.
#'
#' @note To read exported data, you must first use REDCap's 'Export Data' function and select the 'R Statistical Software' format. It will then generate a CSV file with all the observations and an R file with the necessary code to complete each variable's information.
#'
#' @export

redcap_data<-function(data_path = NA, dic_path = NA, event_path = NA, uri = NA, token = NA)
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

  # Read data, dictionary and event-form mapping in case of exported data.
  if(all(!c(data_path, dic_path) %in% NA) & all(c(token, uri) %in% NA)){

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
    dic <- utils::read.csv(paste(dic_path), encoding = "UTF-8", header = FALSE)
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
      event_form <- utils::read.csv(paste(event_path), encoding = "UTF-8")
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

    # First read the labels
    labels <- suppressMessages(REDCapR::redcap_read(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label_headers = "label")$data)

    labels <- gsub("\\)(.*\\))", "\\1", gsub("(\\(.*)\\(", "\\1", names(labels)))

    # Read data using the API connection
    data_api <- REDCapR::redcap_read_oneshot(redcap_uri = uri, token = token, verbose = FALSE, raw_or_label = "label")$data

    if (nrow(data_api) > 0) {
      names(data_api)[1] <- "record_id"
    } else {
      stop("No observational data is available for reading. Please ensure that you add records to your REDCap project.", call. = F)
    }

    # Apply labels
    data_api <- as.data.frame(purrr::map2(data_api, labels, ~labelled::set_variable_labels(.x, .y)))

    # Read dictionary using the API connection
    dic_api <- REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = FALSE)$data

    ## Making sure the names of both dictionaries(exported data and API connection) match
    names(dic_api)[names(dic_api) %in% c("select_choices_or_calculations", "branching_logic", "question_number")] <- c("choices_calculations_or_slider_labels", "branching_logic_show_field_only_if", "question_number_surveys_only")

    # Remove descriptive variables from dictionary
    if ("descriptive" %in% dic_api$field_type) {
      dic_api <- dic_api %>% dplyr::filter(!.data$field_type %in% "descriptive")
    }

    # Indicator of longitudinal projects
    longitudinal <- ifelse("redcap_event_name" %in% names(data_api), TRUE, FALSE)

    # Read event file
    if(!is.na(event_path)){

      # Warning: event_path not necessary while using API connection
      warning("The event_path argument is not necessary as the event-form correspondence can be automatically read with the API connection")

      setwd(oldwd)
      event_form <- utils::read.csv(paste(event_path), encoding = "UTF-8")
      data_def <- list(data = data_api,
                       dictionary = dic_api,
                       event_form = event_form)
    } else {

      # If the event file is not specified, the function reads it using the API connection (in case of longitudinal projects)
      if(longitudinal){

        event_form <- as.data.frame(REDCapR::redcap_event_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data)

        data_def <- list(data = data_api[, !(grepl("_complete", names(data_api)))],
                         dictionary = dic_api,
                         event_form = event_form)

      } else {

        data_def <- list(data = data_api,
                         dictionary = dic_api)

      }

    }
  }

  # Specifying the "UTF-8" encoding to each character column of the data
  for (i in 1:length(data_def$data)) {
    if(is.character(data_def$data[, i])){
      suppressWarnings(data_def$data[, i] <- stringr::str_conv(data_def$data[, i], "UTF-8"))
    }
  }

  # Apply labels (just in API cases)
  if(all(!c(token, uri) %in% NA) & all(c(data_path, dic_path) %in% NA)){
    data_def$data <- as.data.frame(purrr::map2(data_def$data, labels, ~ labelled::set_variable_labels(.x, .y)))
  }

  # Output
  return(data_def)

}
