#' Translate REDCap Logic to R Logic
#'
#' This function allows you to convert REDCap logic into R logic. WARNING: Please note that if the REDCap logic involves smart variables, this function may not be able to transform it accurately.
#'
#' @param ... List containing the data, dictionary and event mapping (if applicable) of the REDCap project. This should be the output of the `redcap_data` function.
#' @param data Data frame containing data from REDCap. If the list is specified, this argument is not required.
#' @param dic Data frame  containing the dictionary read from REDCap. If the list is specified, this argument is not required.
#' @param event_form Data frame  containing the correspondence of each event with each form. If the list is specified, this argument is not required.
#' @param logic String containing logic in REDCap format.
#' @param var String with the name of the variable containing the logic.
#' @return List containing the logic in R format and its evaluation.
#' @examples
#' rd_rlogic(covican,
#'           logic = "if([exc_1]='1' or [inc_1]='0' or [inc_2]='0' or [inc_3]='0',1,0)",
#'           var = "screening_fail_crit")
#' @export

rd_rlogic <- function(..., data = NULL, dic = NULL, event_form = NULL, logic, var){

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
        warning("Event has been specified twice so the function will not use the information in the event_form argument.")
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

  rlogic <- logic

  #We look first if there is some checkbox evaluated in the logic
  if(grepl("\\)\\]",rlogic)){
    num_vars <- stringr::str_count(rlogic,"]")
    for(i in 1:num_vars){
      rlogic <- gsub("\\[(.+)\\((\\d+)\\)\\]","[\\1___\\2]",rlogic)
    }
  }

  #If we find [event-name][var] is the same as [var]
  #If we find [var][current-instance] is the same as [var]
  rlogic <- gsub("\\[event\\-name\\]\\[","[", rlogic)
  rlogic <- gsub("\\]\\[current-instance\\]", "]", rlogic)

  #Change event-name, user-dag-name, record-dag-name
  rlogic <- gsub("\\[event\\-name\\]","[redcap_event_name]", rlogic)
  rlogic <- gsub("\\[user\\-dag\\-name\\]","[redcap_data_access_group]", rlogic)
  rlogic <- gsub("\\[record\\-dag\\-name\\]","[redcap_data_access_group]", rlogic)

  #Get the variables that are being evaluated

  #Get all variables evaluated
  rlogic_var <- unlist(stringr::str_extract_all(rlogic, "\\[[\\w,\\-]+\\]"))

  #Check if each variable is present in the data or it's one of the events
  if(longitudinal){
    check_lgl <- purrr::map_lgl(rlogic_var,function(x){
      out <- gsub("^\\[","",x)
      out <- gsub("\\]$","",out)
      out%in%names(data) | out%in%data$redcap_event_name
    })
  }else{
    check_lgl <- purrr::map_lgl(rlogic_var,function(x){
      out <- gsub("^\\[","",x)
      out <- gsub("\\]$","",out)
      out%in%names(data)
    })
  }

  #If there are some that are not in the dataframe it will give an error

  if(any(!check_lgl)){

    stop("Redcap logic contains some redcap variable that it isn't found in the database (it can be a smart-variable)")

  }else{

    #If all the variables are contained in the data we can transcribe the redcap logic to r logic
    #When in redcap we had " now we have \". Change "" to '':
    rlogic <-  gsub('"', "'", rlogic)


    #Change the redcap functions into r functions
    rlogic <- gsub("if\\s?\\(", "ifelse(", rlogic)
    rlogic <- gsub("rounddown(.*),0\\)", "floor\\1)", rlogic)
    rlogic <- gsub("rounddown(.*),1\\)", "round\\1, 1)", rlogic)
    rlogic <- gsub("rounddown(.*),2\\)", "round\\1, 2)", rlogic)
    rlogic <- gsub("rounddown(.*),3\\)", "round\\1, 3)", rlogic)
    rlogic <- gsub("rounddown(.*)\\)", "floor\\1)", rlogic)
    rlogic <- gsub("datediff\\s?", "lubridate::time_length(lubridate::interval", rlogic)
    rlogic <- gsub("sum\\((.*?)\\)","rowSums(cbind(\\1))", rlogic)
    #Change dates (there can be dates specified in the logic) to date format
    if(grepl("'dmy'", rlogic)){
      rlogic <- gsub("'(\\d\\d-\\d\\d-\\d\\d\\d\\d)'", "lubridate::dmy('\\1')", rlogic)
    }else if(grepl("'mdy'", rlogic)){
      rlogic <- gsub("'(\\d\\d-\\d\\d-\\d\\d\\d\\d)'", "lubridate::mdy('\\1')", rlogic)
    }else if(grepl("'ymd'", rlogic)){
      rlogic <- gsub("'(\\d\\d\\d\\d-\\d\\d-\\d\\d)'", "lubridate::ymd('\\1')", rlogic)
    }else if(grepl("'ydm'", rlogic)){
      rlogic <- gsub("'(\\d\\d\\d\\d-\\d\\d-\\d\\d)'", "lubridate::ydm('\\1')", rlogic)
    }else if(grepl("'myd'", rlogic)){
      rlogic <- gsub("'(\\d\\d-\\d\\d\\d\\d-\\d\\d)'", "lubridate::myd('\\1')", rlogic)
    }else if(grepl("'dym'", rlogic)){
      rlogic <- gsub("'(\\d\\d-\\d\\d\\d\\d-\\d\\d)'", "lubridate::dym('\\1')", rlogic)
    }

    rlogic <- gsub("\\,\\s?true", "", rlogic)
    rlogic <- gsub("\\,\\s?'y'\\,\\s?'dmy'", "), 'year'", rlogic)
    rlogic <- gsub("\\,\\s?'d'\\,\\s?'dmy'", "), 'day'", rlogic)
    rlogic <- gsub("\\,\\s?'m'\\,\\s?'dmy'", "), 'month'", rlogic)

    rlogic <- gsub("\\,\\s?'y'\\)", "), 'year')", rlogic)
    rlogic <- gsub("\\,\\s?'d'\\)", "), 'day')", rlogic)
    rlogic <- gsub("\\,\\s?'m'\\)", "), 'month')", rlogic)



    #Change variables specification. If [][] we get the event with the first claudator. If not the event will be the same as the one of the calculated variable

    #Vector with all the [][] if found:
    var_event <- unlist(stringr::str_extract_all(rlogic, "\\[[\\w,\\-]+\\]\\[[\\w,\\-]+\\]"))

    if(length(var_event) > 0){

      #Separate them
      list_var_event <- purrr::map(var_event, function(x){
       x <- unlist(stringr::str_split(x, "\\]\\["))
       x <- gsub("\\[", "", x)
       x <- gsub("\\]", "", x)
      })

      #If there is one same variable evaluated in different events the logic can't be transcribed
      n_events <- data.frame(do.call(rbind, list_var_event))
      names(n_events) <- c("events", "vars")
      n_events <- n_events %>%
        dplyr::group_by(.data$vars) %>%
        dplyr::summarise(n = length(unique(.data$events)))

      if(any(n_events$n > 1)){
        stop("The logic can't be transcribed because the same variable is present in the logic specified for different events")
      }

      #Apply the previously defined function to get the value in the corresponding event and fill it for all the rows of data
      for(i in 1:length(list_var_event)){
        data <- fill_data(list_var_event[[i]][1], list_var_event[[i]][2], data)
      }

      rlogic <- gsub("\\[\\w+\\]\\[", "[", rlogic)

    }

    #Change variable specification from [] to data$

    #Change first [.] = '' for is.na(data$.) and [.] <>'' for !is.na(data$.)

    rlogic <- gsub("\\[(\\w+)\\]\\s?<>\\s?''", "!is.na(data$\\1)", rlogic)
    rlogic <- gsub("\\[(\\w+)\\]\\s?=\\s?''", "is.na(data$\\1)", rlogic)
    rlogic <- gsub("\\[(\\w+)\\]","data$\\1",rlogic)

    # #Inside the interval function there will be date variables so we have to wrap them with as.Date. Let's take the part we want to change:
    # if(grepl("interval\\(", rlogic)) {
    #   interval_str <- unlist(str_match_all(rlogic, "interval\\(.*?\\)"))
    #   interval_str2 <- map(interval_str, ~gsub("(data\\$\\w+)", "as.Date(\\1)", .x))
    #   for(i in 1:length(interval_str2)) {
    #     rlogic <- stringi::stri_replace_all_fixed(rlogic, interval_str[[i]], interval_str2[[i]])
    #   }
    # }

    #Change the redcap operators into r operators
    rlogic <- gsub("=","==",rlogic)
    rlogic <- gsub("<==","<=",rlogic)
    rlogic <- gsub(">==",">=",rlogic)
    rlogic <- gsub("<>","!=",rlogic)
    rlogic <- gsub(" and "," & ",rlogic)
    rlogic <- gsub(" or "," | ",rlogic)

    #Remove '' after one of these symbols appear: <, >, <=, >=
    rlogic <- gsub("\\s?<\\s?'([\\d\\.]+)'", " < \\1", rlogic, perl = TRUE)
    rlogic <- gsub("\\s?>\\s?'([\\d\\.]+)'", " > \\1", rlogic, perl = TRUE)
    rlogic <- gsub("\\s?<=\\s?'([\\d\\.]+)'", " <= \\1", rlogic, perl = TRUE)
    rlogic <- gsub("\\s?>=\\s?'([\\d\\.]+)'", " >= \\1", rlogic, perl = TRUE)

    #Transform '' for missing:
    rlogic <- gsub("''", "NA", rlogic)

    #Now that we have transcribed the logic we have to evaluate it in the event of the corresponding variable (if data contains more than one event):
    if(!is.null(event_form)){

      #Get the form where the variable is found through the dictionary:
      form_var <- dic %>%
        dplyr::filter(.data$field_name == var) %>%
        dplyr::pull(.data$form_name)

      #Get the event through the event-form mapping (it can be  more than one):
      event_var <- event_form %>%
        dplyr::filter(.data$form == form_var) %>%
        dplyr::pull(.data$unique_event_name)

    }

    #Redefine rounding function to match the one in redcap for rounding .5 decimals in the same way (2.5 ~ 3)
    round = function(x, digits) {
      posneg = sign(x)
      z = abs(x)*10^digits
      z = z + 0.5 + sqrt(.Machine$double.eps)
      z = trunc(z)
      z = z/10^digits
      z*posneg
    }

    #Calculate evaluating the logic
    rlogic_eval <- try(eval(parse(text = rlogic)), silent = TRUE)

    if(inherits(rlogic_eval, "try-error") | length(rlogic_eval) == 0){

      stop("The logic can't be evaluated after the translation")

    }else{

      #Only in the specified event (if data contains more than one event)!
      if(!is.null(event_form)){
        return(
        list(
          rlogic = rlogic,
          eval = data %>%
            dplyr::mutate(calc = rlogic_eval,
                          calc = ifelse(! .data$redcap_event_name %in% event_var, NA, .data$calc)) %>%
            dplyr::pull(.data$calc)
        )
        )
      }else{
        #If there is only one event:
        return(
          list(
            rlogic = rlogic,
            eval = rlogic_eval
          )
        )
      }


    }

  }

}
