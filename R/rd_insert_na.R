#' Insert missing using a filter
#'
#' Function that allows you to manually input a missing to some variables (`vars`) when some filters (`filter`) are satisfied. Useful for checkboxes without a gatekeeper question in the branching logic. Take in account that the variable will be transformed only in the events where both the variable and the filter evaluation are present, so they need to have at least one event in common.
#' @param ... List containing the data and the dictionary and the event if it's needed. Can be the output of the function `redcap_data`.
#' @param data Data frame containing data from REDCap. If the list is specified this argument is not needed.
#' @param dic Data frame  containing the dictionary read from REDCap. If the list is specified this argument is not needed.
#' @param event_form Data frame  containing the correspondence of each event with each form. If the list is specified this argument is not needed.
#' @param vars Character vector containing the names of those variables to transform.
#' @param filter Character vector containing the logic to be directly evaluated. When each logic is TRUE the corresponding variable specified in `vars` will be put to missing.
#' @return transformed data with the specified variables converted.
#' @examples
#' table(is.na(covican$data$potassium))
#' data <- rd_insert_na(covican,
#'              vars = "potassium",
#'              filter = "age < 65")
#' table(data$potassium)
#' @export
#' @importFrom rlang .data

rd_insert_na <- function(..., data = NULL, dic = NULL, event_form = NULL, vars, filter){

  project <- c(...)

  if(!is.null(project)){
    if(!is.null(data)){
      warning("Data has been specified twice so the function will not use the information in the data argument.")
    }

    if(!is.null(dic)){
      warning("Dictionary has been specified twice so the function will not use the information in the dic argument.")
    }

    data <- project$data
    dic <- project$dictionary

    if("event_form" %in% names(project)){
      if(!is.null(event_form)){
        warning("Event has been specified twice so the function will not use the information in the event argument.")
      }
      event_form <- project$event_form
    }
  }

  if(is.null(data) | is.null(dic)){
    stop("No data/dictionary was provided")
  }

  #Check if the project is longitudinal (has more than one event) or not:
  longitudinal <- ifelse("redcap_event_name" %in% names(data), TRUE, FALSE)

  if(is.null(event_form) & longitudinal){
    stop("There is more than one event in the data and the event-form correspondence hasn't been specified")
  }

  if(length(filter) != length(vars)){

    stop("The number of filter variables specified doesn't match with the number of variables specified", call. = FALSE)

  }else{

    for(i in 1:length(filter)){

      #For every filter & variable get the variables specified in the filter and their events (if there is more than one event)
      if(longitudinal){
        #First, let's get the variables in the filter:
        vars_filter <- trimws(unlist(stringr::str_split(filter[i], "[&|]")))
        vars_filter <- gsub("!?is.na\\(", "", vars_filter)
        vars_filter <- unlist(stringr::str_extract_all(vars_filter, "^\\w+"))

        #Get the events of these variables:
        event_filter <- tibble::tibble(vars_filter = vars_filter) %>%
          dplyr::mutate(form = purrr::map_chr(.data$vars_filter, ~dic %>%
                                                dplyr::filter(.data$field_name %in% .x) %>%
                                                dplyr::pull(.data$form_name)),
                        event = purrr::map(.data$form, ~event_form %>%
                                             dplyr::filter(.data$form %in% .x) %>%
                                             dplyr::pull(.data$unique_event_name)))

        #Get the events in common of all the filter variables:
        events <- Reduce(intersect,  event_filter$event)

        #If the filter variables have no events in common:
        if(length(events) == 0){
          stop("Variables included in the filter are in different events.")
        }

        #Now let's get the event of the variable to be transformed:
        form_var <- dic %>%
          dplyr::filter(.data$field_name == vars[i]) %>%
          dplyr::pull(.data$form_name)

        event_var <- event_form %>%
          dplyr::filter(.data$form == form_var) %>%
          dplyr::pull(.data$unique_event_name)

        match_events <- intersect(events, event_var)

        #If the filter variables are in different events with respect to the variable to be transformed it will give an error:
        if(length(match_events) == 0){
          stop("The variable to be transformed is in a different event than the filter to be evaluated.")
        }else{
          #If there is some event of the variable to be transformed that is not present in the filter it will give a warning:
          if (!all(event_var %in% match_events)) {
            warning(stringr::str_glue("The variable to be transformed ({vars[i]}) is present in more events than the events where the corresponding filter is evaluated. Only the rows of those events in common will be transformed ({match_events})."))
          }
        }
      }

      #Transform the data:
      id <- data %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(eval(parse(text = filter[i]))) %>%
        dplyr::pull(id)

      data[id, vars[i]] <-  NA

    }

    data

  }

}
