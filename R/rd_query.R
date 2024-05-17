#' Identification of queries
#'
#' This function allows you to identify queries using a particular expression/filter.
#' It can be used to identify missing values or to identify values outside the lower and upper limits of a variable.
#'
#' @param ... List containing the data, dictionary and event mapping (if required) of the REDCap project. This should be the output of the `redcap_data` function.
#' @param data Data frame containing the data read from REDCap. If the list is given, this argument is not required.
#' @param dic Data frame containing the dictionary read from REDCap. If the list is given, this argument is not required.
#' @param event_form Data frame containing the correspondence of each event with each form. If the list is specified, this argument is not required.
#' @param variables Character vector containing the names of the database variables to be checked.
#' @param expression Character vector of expressions to apply to the selected variables.
#' @param negate Logical value indicating whether the defined expression should be negated. Default value is `FALSE`.
#' @param variables_names Character vector containing the description of each selected variable. By default, the function automatically takes the description of each variable from of the REDCap project dictionary.
#' @param query_name Description of the query. It can be the same for all variables, or you can define a different one for each variable. By default, this function defines it as `The value is [value] and it should not be [expression]'`.
#' @param instrument REDCap instrument to which the variables belong. It can be the same for all variables, or you can define a different one for each variable. By default, the function automatically selects the corresponding instrument of each variable from the REDCap project dictionary.
#' @param event The name of the REDCap event to analyze. If there are events in your REDCap project, you should use this argument to name the event to which the defined variables belong.
#' @param filter A filter to be applied to the dataset. For example, this argument can be used to apply the branching logic of a defined variable.
#' @param addTo Data frame corresponding to a previous query data frame to which you can add the new query data frame. By default, this function always creates a new data frame regardless previous reports.
#' @param report_title Character string specifying the title of the report.
#' @param report_zeros Logical. If `TRUE`, the function returns a report containing variables with zero queries.
#' @param by_dag Logical. If `TRUE`, both elements of the output will be grouped by the Data Access Groups (DAGs) of the REDCap project.
#' @param link List containing project information used to create a web link to each query.
#' @return A list with a data frame of 9 columns (10 columns, if the link argument is specified) meant to help the user identify each query and a table with the total number of queries per variable.
#'
#' @examples
#' # Missing values
#' example <- rd_query(covican,
#'                     variables = c("copd", "age"),
#'                     expression = c("is.na(x)", "x %in% NA"),
#'                     event = "baseline_visit_arm_1")
#' example
#'
#' # Expression
#' example <- rd_query(covican,
#'                     variables="age",
#'                     expression="x>20",
#'                     event="baseline_visit_arm_1")
#' example
#'
#' # Using the filter argument
#' example <- rd_query(covican,
#'                     variables = "potassium",
#'                     expression = "is.na(x)",
#'                     event = "baseline_visit_arm_1",
#'                     filter = "available_analytics=='1'")
#' example
#' @export
#' @importFrom rlang .data

rd_query <- function(..., variables = NA, expression = NA, negate = FALSE, event = NA, filter = NA, addTo = NA, variables_names = NA, query_name = NA, instrument = NA, report_title = NA, report_zeros = FALSE, by_dag = FALSE, link = list(), data = NULL, dic = NULL, event_form = NULL)
  {

    # If the entire list resulting from the 'redcap_data' function is used
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

      if("event_form" %in% names(project)) {
        if(!is.null(event_form)){
          warning("Event form has been specified twice so the function will not use the information in the event_form argument.")
        }
        event_form <- as.data.frame(project$event_form)
      }
    }

    # Making sure that the object data and dic are a data.frame
    data <- as.data.frame(data)
    dic <- as.data.frame(dic)

    # Creation of the structure of the queries
    queries <- as.data.frame(matrix(ncol = 10, nrow = 0))
    colnames(queries) <- c("Identifier", "DAG", "Event", "Instrument", "Field", "Repetition", "Description", "Query", "Code", "Link")

    # Creation of the structure for variables with zero queries
    excel_zero <- as.data.frame(matrix(ncol = 4, nrow = 0))
    colnames(excel_zero) <- c("DAG", "Variables", "Description", "Query")

    # Naming the first column of the REDCap's database as record_id
    if (all(!names(data) == "record_id")) {
      names(data)[1] <- "record_id"
    }

    # Error: one of the variables does not exist in the data
    if (any(!variables %in% names(data))) {
      stop("The data does not contain the specified variable(s). Please double-check the input variable name.")
    }

    # Warning: event not specified
    if (all(is.na(event)) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)) & is.null(event_form)) {
      warning("No event or event-form has been specified. Therefore, the function will automatically consider observations from all events in the dataset. Ensure that the selected variable(s) is(are) collected in all specified events. This will avoid overestimating the number of queries.", call. = F)
    }

    # If the argument event is not specified but there is the event form element in our data frame
    if (all(is.na(event)) & !is.null(event_form)) {
      var_form <- dic %>%
        dplyr::filter(.data$field_name %in% variables) %>%
        dplyr::pull(.data$form_name)

      event <- event_form %>%
        dplyr::filter(.data$form %in% var_form) %>%
        dplyr::pull(.data$unique_event_name)
    }

    # Applying a filter of the chosen events to the database
    if (all(!is.na(event))) {

      # Saving initial dataset
      data0 <- data

      # Error: there is no event name variable in the dataset but the argument has a value
      if (all(!c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        stop("The event argument is specified, but there is no event name variable in the dataset.")
      }

      # Error: one of the specified events does not exist in the database
      if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        if (any(!event %in% data$redcap_event_name) & any(!event %in% data$redcap_event_name.factor)) {
          stop("One of the events mentioned does not exist in the database, please verify the argument 'event'.")
        }
      }

      # Warning: there is more than one event specified
      if (length(event) > 1 & is.null(event_form)) {
        warning("More than one event has been specified. Ensure that the selected variable(s) are collected in all specified events. This will avoid overestimating the number of queries.", call. = F)
      }

      # Filtering by event_name.factor if the event is equal to the category in the factor version
      if ("redcap_event_name.factor" %in% names(data) & all(event %in% data$redcap_event_name.factor)) {
          data <- dplyr::filter(data, data$redcap_event_name.factor %in% event)
      }

      # Filtering by event_name if the event is equal to the category in the raw version
      if (all(event %in% data$redcap_event_name)) {
        data <- dplyr::filter(data, data$redcap_event_name %in% event)
      }

      # Warning: there are identifiers that do not present the specified event (it only appears when checking for missings - %in%NA)
      if (length(unique(data0$record_id)) != length(unique(data$record_id)) & any(grepl("%in%NA", gsub(" ", "", expression)))) {
        warning("There are certain identifiers without observations in one of the events. \nUse the function 'rd_event' to check which ones are missing.", call. = FALSE)
      }
    }

    # Error: Trying to split the report by DAGs but the REDCap project does not present DAGs
    if (by_dag %in% TRUE & all(!c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(data))) {
      stop("Trying to split the query reporting according to each DAG, but the project does not present DAGs. To continue, please specify by_dag = F.", call. = F)
    }

    # Warning: If the link argument is not fully completed
    if (!is.null(names(link))) {
      if (!all(c("domain", "redcap_version", "proj_id", "event_id") %in% names(link)) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        stop("There is not enough information in the link argument. \nTo create the link correctly, please provide the domain, redcap version, project ID and event ID.", call. = F)
      }
      if (!all(c("domain", "redcap_version", "proj_id") %in% names(link)) & !any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        stop("There is not enough information in the link argument. \nTo create the link correctly, please provide the domain, redcap version and project ID.", call. = F)
      }
    }

    # Adding the information of the event_id from the link argument to the data
    if (!is.null(link[["event_id"]])) {

      # The dataset has an event name variable
      if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {

        # The event argument is specified and the event_id is specified using the events in the raw version
        if (all(event %in% names(link[["event_id"]])) & all(event %in% data[, "redcap_event_name"])) {
          event_link <- data.frame("name" = names(link[["event_id"]]),
                                   "event_id" = as.numeric(link[["event_id"]]))
          data <- merge(data, event_link, by.x = "redcap_event_name", by.y = "name")
        }

        # The event argument is specified and the event_id is specified using the events in the factor version
        if (all(event %in% names(link[["event_id"]])) & all(event %in% data[, "redcap_event_name.factor"])) {
          event_link <- data.frame("name" = names(link[["event_id"]]),
                                   "event_id" = as.numeric(link[["event_id"]]))
          data <- merge(data, event_link, by.x = "redcap_event_name.factor", by.y = "name")
        }

        # The event argument is not specified and the event_id is specified using the events in the raw version
        if (all(is.na(event) & all(names(data[, "redcap_event_name"] %in% names(link[["event_id"]]))))) {
          event_link <- data.frame("name" = names(link[["event_id"]]),
                                   "event_id" = as.numeric(link[["event_id"]]))
          data <- merge(data, event_link, by.x = "redcap_event_name", by.y = "name")
        }

        # The event argument is not specified and the event_id is specified using the events in the factor version
        if (all(is.na(event) & all(names(data[, "redcap_event_name.factor"] %in% names(link[["event_id"]]))))) {
          event_link <- data.frame("name" = names(link[["event_id"]]),
                                   "event_id" = as.numeric(link[["event_id"]]))
          data <- merge(data, event_link, by.x = "redcap_event_name.factor", by.y = "name")
        }

      } else {

        # There is not an event name variable in the dataset
        if (length(link[["event_id"]]) == 1) {

          data[, "event_id"] <- as.numeric(link[["event_id"]])

        } else {

          # Error: More than one event_id is specified and the project is non-longitudinal
          stop("The project is non-longitudinal (has no events). Therefore, you only need to specify a single event ID.", call. = F)

          }
        }
    }

    # Warning: detecting more variables than expressions, so the function applies the same expression to all variables
    if (length(variables) > length(expression)) {

      expression <- rep(expression[1], length(variables))
      warning("Due to the greater number of variables compared to expressions, the same expression has been applied to all of them.", call. = FALSE)

    }

    # Filtering the data using the information of the argument 'filter'
    if (all(!is.na(filter)) & length(filter) == 1) {

        # Error: logic used in the filter is incorrect
        command <- paste0("data", "<-dplyr::filter(data,", filter, ")")

        evaluation <- try(eval(parse(text = command)), silent = TRUE)

        if(inherits(evaluation, "try-error")){

          stop("The logic used in the filter is incorrect. Please review the filter argument and make the necessary adjustments.")

        } else {

          eval(parse(text = command))

        }

        # Error: filter results in zero observations
        if (nrow(data) == 0) {
          warning("The filter applied is correct but does not match any observations. Please double-check that the filter is properly formulated.", call. = FALSE)
        }
    }

    # Variables to be checked for missing values which present a branching logic
    var_logic <- variables[which(variables %in% dic[!dic$branching_logic_show_field_only_if %in% c(NA, ""), "field_name"])]

    # We create three objects that will contain the branching logic that can't be converted to R logic
    compatible <- NULL
    logics <- NULL
    br_eval <- NULL

    # Branching logic detected (only in variables checked for missing values)
    if (length(var_logic) > 0) {

      # Data frame with variable, variable name and branching logic
      branch <- data.frame(
        var = dic$field_name[dic$field_name %in% gsub("___\\d*$", "", var_logic)],
        label = gsub("\\s+", "", gsub("<.*?>", "", dic$field_label[dic$field_name %in% gsub("___\\d*$", "", var_logic)])),
        branch = dic$branching_logic_show_field_only_if[dic$field_name %in% gsub("___\\d*$", "", var_logic)])

      # Saving the original data frame before the transformation
      branch0 <- branch

      # We convert the REDCap logic into R logic
      if ((!is.null(event_form) | all(!c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) & all(stringr::str_detect(branch$branch, paste(c("\\[", "\\]"), collapse = "|")))) {

        for (j in 1:nrow(branch)) {

          evaluation <- try(rd_rlogic(data = data, dic = dic, event_form = event_form, logic = branch$branch[j], var = branch$var[j])$rlogic, silent = T)

          if (!inherits(evaluation, "try-error")) {

            branch$branch[j] <- rd_rlogic(data = data, dic = dic, event_form = event_form, logic = branch$branch[j], var = branch$var[j])$rlogic

          } else {

            logics <- rbind(logics, branch$var[j])

          }
        }

        if (!is.null(logics) & nrow(data) > 0) {

          warning(c("The branching logic of the following variables could not be converted into R logic:", paste0("\n - ", unique(logics)), "\n Check the results element of the output(...$results) for details."), call. = F)

        }
      }

      # Arrange the data frames
      rownames(branch0) <- NULL
      names(branch0) <- c("Variable", "Label", "Branching logic")
      rownames(branch) <- NULL
      names(branch) <- c("Variable", "Label", "Branching logic")

      ## Warning about the variables with branching logic
      if (is.null(event_form) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
        warning("At least one of the variables that have been checked for missing values present a branching logic. \nCheck the results element of the output(...$results) for details.", call. = FALSE)
      }
    }

    # Apply an expression to the corresponding variable
    for (i in 1:length(expression)) {

      # If the chosen variable has a branching logic
      if (length(var_logic) > 0 & (!is.null(event_form) | all(!c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) & any(!variables[i] %in% logics)) {
        if (any(variables[i] == var_logic)) {

          logic <- branch %>%
            dplyr::filter(.data$Variable %in% variables[i]) %>%
            dplyr::pull(.data$`Branching logic`)

          branching <- NULL
          command <- paste0("branching", "<-dplyr::filter(data,", gsub(pattern = "data\\$", replacement = "data$", x = logic), ")")

          eval(parse(text = command))

          if(nrow(branching) > 0) {

            compatible <- rbind(compatible, variables[i])
            raw0 <- branching

          } else {

            br_eval <- rbind(br_eval, variables[i])
            raw0 <- data

          }
        } else {

          raw0 <- data

        }
      } else {

        raw0 <- data

      }

      # Applying the expression
      x <- variables[i]

      command <- paste0("raw", "<-dplyr::filter(raw0,", gsub("\\bx\\b", x, expression[i]), ")")

      # Evaluation of the command with the resulting expression
      eval(parse(text = command))

      # Transform the expression so it does not contain "x"
      expression[i] <- gsub("\\bx\\b", "", expression[i])

      # If the argument negate is TRUE, reverse the expression apllied
      if (negate == TRUE) {
        raw <- suppressMessages(dplyr::anti_join(raw0, raw))
      }

      # If there is more than one filter
      if (all(!filter %in% NA) & length(filter) > 1) {

        # If the number of filters is equal to the number of variables
        if (length(filter) == length(variables)) {

          # Apply each filter
          command <- paste0("definitive", "<-dplyr::filter(raw,", filter[i], ")")
          eval(parse(text = command))

          # Filtering results in zero observations
          if (nrow(data) == 0) {
            warning("One of the applied filters does not match any observations. Please check that it is correct.", call. = FALSE)
            }
          } else {
            # Error: number of filters is different from the number of variables
            stop("Multiple filters have been applied, but the number of filters does not match the number of variables. Please verify it.", call. = F)
          }

        } else {
          definitive <- raw
        }


      # Identification of queries, using the structure built before
      if (nrow(definitive) > 0) {
        x <- definitive[, c("record_id", grep("redcap", names(definitive), value = T), variables[i])]

        # Add event_id when the link argument is specified
        if (all(c("domain", "redcap_version", "proj_id", "event_id") %in% names(link))) {
          x[, "event_id"] <- definitive[, "event_id"]
        }

        # Filling the structure data frame with data
        excel <- data.frame(
          Identifier = x[, "record_id"],
          DAG = if (any(c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(x))) {
            if ("redcap_data_access_group.factor" %in% names(x)) {
              as.character(x[, "redcap_data_access_group.factor"])
            } else {
              as.character(x[, "redcap_data_access_group"])
            }
          } else {
            "-"
          },
          Event = if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(x))) {
            if ("redcap_event_name.factor" %in% names(x)) {
              as.character(x[, "redcap_event_name.factor"])
            } else {
              as.character(x[, "redcap_event_name"])
              }
          } else {
            "-"
          },
          Instrument = if (all(is.na(instrument))) {
            if (gsub("___.*$", "", variables[i]) %in% dic$field_name) {
              gsub("_", " ", stringr::str_to_sentence(dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "form_name"]))
            } else{
              "-"
            }
          } else {
            if (length(instrument) == 1) {
              instrument
            } else {
              if (length(instrument) == length(variables) & length(instrument) > 1) {
                instrument[i]
              } else {
                if (length(instrument) != length(variables) & length(instrument) > 1) {
                  stop("Multiple instruments specified, but the number of instruments is different from the number of variables. Please match each variable to each instrument.", call. = F)
                }
              }
            }
          },
          Field = variables[i],
          Repetition = if (any(c("redcap_repeat_instrument", "redcap_repeat_instance") %in% names(x))) {
            if ("redcap_repeat_instrument" %in% names(x) & all(is.na(x[, "redcap_repeat_instrument"]))) {
              if ("redcap_repeat_instance" %in% names(x) & all(is.na(x[, "redcap_repeat_instance"]))) {
                "-"
              } else {
                paste0(x[, "redcap_repeat_instance"])
              }
            } else {
              paste0(x[, "redcap_repeat_instrument"], "-", x[, "redcap_repeat_instance"])
            }
          } else {
            "-"
          },
          Description = if (all(is.na(variables_names))) {
            if (gsub("___.*$", "", variables[i]) %in% dic$field_name) {
              trimws(gsub("<.*?>", "", as.character(dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "field_label"])))
            } else{
              "-"
            }
          } else {
            if (length(variables_names) == 1) {
              variables_names
            } else {
              if (length(variables_names) == length(variables) & length(variables_names) > 1) {
                variables_names[i]
              } else {
                if (length(variables_names) != length(variables) & length(variables_names) > 1) {
                  stop("Multiple variables names specified, but the number of names is different from the number of variables. Please match each variable to each name.", call. = F)
                }
              }
            }
          },
          Query = if (all(!is.na(query_name))) {
            if (length(query_name) %in% 1) {
              query_name
            } else {
              if (length(query_name) %in% length(variables) & length(query_name) > 1) {
                query_name[i]
              } else {
                if (!length(query_name) %in% length(variables) & length(query_name) > 1) {
                  stop("Multiple query names specified, but the number of query names is different from the number of variables. Please match each variable to each query name.", call. = F)
                }
              }
            }
          } else {
            if (negate == TRUE) {
              trimws(gsub("  ", " ", paste("The value is", x[, variables[i]], "and it should be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern=c("<", ">", "<=", ">=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement=c(" less than ", " greater than ", " less than or equal to ", " greater than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize=FALSE))))
            } else {
              trimws(gsub("  ", " ", paste("The value is", x[, variables[i]], "and it should not be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern=c("<", ">", "<=", ">=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement=c(" less than ", "greater than ", " less than or equal to ", " greater than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize=FALSE))))
            }
          },
          Code = "",
          stringsAsFactors = FALSE
        )

        # Add link to each query
        if (all(c("domain", "redcap_version", "proj_id", "event_id") %in% names(link)) & any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
            excel[, "Link"] <- paste0("https://", link[["domain"]], "/redcap_v", link[["redcap_version"]], "/DataEntry/index.php?pid=", link[["proj_id"]], "&event_id=", x[, "event_id"], "&page=", dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "form_name"], "&id=", x[, "record_id"])
        }

        if (all(c("domain", "redcap_version", "proj_id") %in% names(link)) & !any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
          excel[, "Link"] <- paste0("https://", link[["domain"]], "/redcap_v", link[["redcap_version"]], "/DataEntry/index.php?pid=", link[["proj_id"]], "&page=", dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "form_name"], "&id=", x[, "record_id"])
        }

        # Adding each identified query to the queries data frame
        queries <- rbind(queries, excel)

      } else {

        # If there are no queries to be identified, we still need this information to build the report
        excel <- data.frame(
          DAG = if (any(c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(data)) & nrow(data) > 0) {
            if("redcap_data_access_group.factor" %in% names(data)) {
              if (all(is.na(event))) {
                unique(as.character(data[, "redcap_data_access_group.factor"]))
              } else {
                unique(as.character(data[data$redcap_event_name %in% event | data$redcap_event_name.factor %in% event, "redcap_data_access_group.factor"]))
              }
            } else {
              if (all(is.na(event))) {
                unique(as.character(data[, "redcap_data_access_group"]))
              } else {
                unique(as.character(data[data$redcap_event_name %in% event | data$redcap_event_name.factor %in% event, "redcap_data_access_group"]))
              }
            }
          } else {
            "-"
          },
          Variables = variables[i],
          Description = if (all(is.na(variables_names))) {
            if (gsub("___.*$", "", variables[i]) %in% dic$field_name) {
              trimws(gsub("<.*?>", "", dic[dic[, "field_name"] %in% gsub("___.*$", "", variables[i]), "field_label"]))
            } else{
              "-"
            }
          } else {
            if (length(variables_names) == 1) {
              variables_names
            } else {
              if (length(variables_names) == length(variables) & length(variables_names) > 1) {
                variables_names[i]
              } else {
                if (length(variables_names) != length(variables) & length(variables_names) > 1) {
                  stop("Multiple variables names specified, but the number of names is different from the number of variables. Please match each variable to each name.", call. = F)
                }
              }
            }
          },
          Event = if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)) & nrow(data) > 0) {
            if ("redcap_event_name.factor" %in% names(data)) {
              unique(as.character(data[, "redcap_event_name.factor"]))
            } else {
              unique(as.character(data[, "redcap_event_name"]))
            }
          } else {
            "-"
          },
          Query = if (all(!is.na(query_name))) {
            if (length(query_name) == 1) {
              query_name
            } else {
              if (length(query_name) == length(variables) & length(query_name) > 1) {
                query_name[i]
              } else {
                if (length(query_name) != length(variables) & length(query_name) > 1) {
                  stop("Multiple query names specified, but the number of query names is different from the number of variables. Please match each variable to each query name.", call. = F)
                }
              }
            }
          } else {
            if (negate == TRUE) {
              gsub("  ", " ", paste("The value should be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern=c("<", ">", ">=", "<=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement=c(" less than ", " greater than ", " greater than or equal to ", " less than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize=FALSE)))
            } else {
              gsub("  ", " ", paste("The value should not be", stringi::stri_replace_all_regex(gsub(" ", "", expression[i]), pattern=c("<", ">", ">=", "<=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement=c(" less than ", " greater than ", " greater than or equal to ", " less than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize=FALSE)))
            }
          },
          stringsAsFactors = F
        )

        # Adding each variable with zero queries to the data frame
        excel_zero <- rbind(excel_zero, excel)
      }
    }

    ## Warnings about the variables with branching logic
    if (!is.null(br_eval) & nrow(data) > 0) {
      warning("The branching logic of the following variables can not be applied automatically:", paste0("\n- ", unique(br_eval)), "\nCheck the results element of the output(...$results) for details.", call. = FALSE)
    }

    if (!is.null(compatible)) {
      if (length(compatible) > 1) {
        sentence <- c("The branching logic of the following variables were applied automatically: ", paste0("\n- ", unique(compatible)))
      } else {
        sentence <- paste0("The branching logic of the following variable was applied automatically: ", unique(compatible))
      }
      warning(sentence, call. = F)
    }


    # If the argument 'addTo' is specified, combine the queries generated with a previous data frame of queries
    if (all(!is.na(addTo))) {

      if ("Link" %in% names(addTo$queries)) {
        col_names <- c(names(queries), "Link")
      } else {
        col_names <- names(queries)
      }

      queries <- merge(queries,
                       addTo$queries,
                       by = intersect(names(addTo$queries), names(queries)),
                       all = TRUE)
      queries <- queries %>%
        dplyr::select(dplyr::all_of(col_names))
    }

    # Classify each query with it's own code
    if (nrow(queries) > 0) {

      # First we sort the data frame by record_id
      if (all(grepl("-", queries$Identifier))) {
        queries <- queries %>%
                      tidyr::separate("Identifier", c("center", "id"), sep = "([-])", remove = FALSE)
        queries[, "center"] <- as.numeric(queries[, "center"])
        queries[, "id"] <- as.numeric(queries[, "id"])
        queries <- queries[order(queries[, "center"], queries[, "id"]), ]
        rownames(queries) <- NULL
        queries <- queries %>% dplyr::select(-"center", -"id")
      } else {
        queries$Identifier <- as.numeric(queries$Identifier)
        queries <- queries[order(queries$Identifier), ]
      }

      # Add the code to each query and eliminate duplicated ones
      queries <- unique(queries %>%
                          dplyr::select(-"Code"))
      queries <- data.frame(queries %>%
                              dplyr::group_by(.data$Identifier) %>%
                              dplyr::mutate(cod = 1:dplyr::n()))
      queries$Code <- paste0(as.character(queries$Identifier), "-", queries$cod)
      queries <- queries %>% dplyr::select(-"cod")

      # Reorder the columns if the link argument was specified
      if ("Link" %in% names(queries)) {
        queries <- queries %>%
                      dplyr::select("Identifier":"Query", "Code", "Link")
      }

      # Build the report
      report <- data.frame("dag" = queries$DAG,
                           "var" = queries$Field,
                           "descr" = queries$Description,
                           "event" = queries$Event,
                           "query_descr" = gsub(" is .* it", "", queries$Query))

      # If there is no previous report or variables names specified we convert the variables and their description to factors using the dictionary
      if (all(addTo %in% NA & variables_names %in% NA)) {
        report$var <- factor(report$var,
                             levels = c(unique(variables)))
        report$descr <- factor(report$descr,
                               levels = c(unique(trimws(gsub("<.*?>", "", dic$field_label[dic$field_name %in% gsub("___.*$", "", variables)])))))

        # If our project has events, we also convert them to factors
        if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
          report$event <- factor(report$event,
                                 levels = if("redcap_event_name.factor" %in% names(data)) {
                                   unique(as.character(data$redcap_event_name.factor))
                                 } else {
                                   unique(as.character(data$redcap_event_name))
                                 })
        }
      }

      # If there is no previous report but the variables names argument is specified we convert the variables and their description to factors using the selected names
      if (all(addTo %in% NA & !is.na(variables_names))) {
        report$var <- factor(report$var,
                             levels = c(unique(variables)))
        report$descr <- factor(report$descr,
                               levels = c(unique(variables_names)))

        # If our project has events, we also convert them to factors
        if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data))) {
          report$event <- factor(report$event,
                                 levels = if("redcap_event_name.factor" %in% names(data)) {
                                   unique(as.character(data$redcap_event_name.factor))
                                 } else {
                                   unique(as.character(data$redcap_event_name))
                                 })
        }

      }


      # Report of all variables (including the ones with zero queries)
      if (report_zeros == TRUE) {

        # Total number of queries by variable, event and DAG
        report <- report %>%
                    dplyr::group_by(.data$dag, .data$var, .data$event, .data$query_descr, .drop = FALSE) %>%
                    dplyr::summarise("total" = dplyr::n(), .groups = "keep")

        # Adding the description and query description to the variables with 0 queries
        if (any(report$total %in% 0)) {

          # Add the query description
          report$var_event <- paste0(report$var, "_", report$event)
          complete_vars <- unique(as.character(report$var_event[report$total != 0]))
          zero_vars <- unique(as.character(report$var_event[!report$var_event %in% complete_vars]))
          report <- report %>%
                      dplyr::filter((.data$var_event %in% complete_vars & .data$total != 0) | .data$var_event %in% zero_vars)

          for (i in zero_vars) {
            report$query_descr[report$var_event %in% i] <- paste0("The value should not be ",
                                                                  stringi::stri_replace_all_regex(gsub(" ", "", expression[which(variables %in% report$var[which(report$var_event %in% i)])]), pattern=c("<", ">", ">=", "<=", "&", "\\|", "==", "!=", "%in%NA", "%in%", "%nin%", "is.na\\(\\)"), replacement=c(" less than ", " greater than ", " greater than or equal to ", " less than or equal to ", " and ", " or ", " equal to ", " not equal to ", " missing ", " equal to ", " not equal to ", " missing "), vectorize=FALSE))
          }

          report <- report %>% dplyr::select(- "var_event")

          # Add the variables description
          report$descr <- purrr::map_chr(gsub("___.*$","",report$var), function(x){
            if (x %in% dic$field_name){

              # Truncate description name if it exceeds 50 characters
              name <- gsub("<.*?>", "", dic$field_label[dic$field_name %in% x])
              stringr::str_trunc(name, 50)

            } else {
              "-"
            }})
        }
      } else {

        # Report of the variables with identified queries (eliminating the ones with zero queries)
        report <- report %>%
            dplyr::group_by(.data$var, .data$descr, .data$event, .data$query_descr, .data$dag, .drop = TRUE) %>%
            dplyr::summarise("total" = dplyr::n(), .groups = "keep") %>%
            dplyr::filter("total" != 0)

      }

      # Truncate variable name if it exceeds 26 characters
      report$descr <- as.character(report$descr)
      report$descr <- stringr::str_trunc(report$descr, 50)

      # Truncate variable name if it exceeds 26 characters
      report$var <- as.character(report$var)
      report$var <- stringr::str_trunc(report$var, 26)

      # Sorting by the total number of queries
      report <- as.data.frame(report)
      report <- report[order(as.numeric(report$total), decreasing = TRUE), ]

      # Arrange the report
      report <- report %>% dplyr::select("dag", "var", "descr", "event", "query_descr", "total")
      names(report) <- c("DAG", "Variables", "Description", "Event", "Query", "Total")
      report[, "Query"] <- gsub("&", " & ", gsub("\\|", " \\| ", report[, "Query"]))
      rownames(report) <- NULL

    } else {
      # If there is none query, the function still creates a report containing all selected variables.

      # Message: if there is none query to be identified
      if (nrow(data) > 0) {
        message("There is no query to be identified in the dataset.")
      }

      report <- excel_zero
      report$Total <- 0
      rownames(report) <- NULL
    }

    # Before starting we check if there is more than one report_title and if it isn't the case we stabilish the caption for the report_title
    if (all(is.na(report_title))) {
      report_title <- "Report of queries"
    } else {
      if (length(report_title) > 1) {
        stop("There is more than one title for the report, please choose only one.", call. = FALSE)
      }
    }

    # Adding information about the variables with branching logic to the report
    if (length(var_logic) > 0 & (!is.null(logics) | is.null(event_form)) & (!is.null(br_eval) | any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)))) {
      branch0$Variable <- stringr::str_trunc(branch0$Variable, 26)
      report <- merge(report,
                      branch0 %>% dplyr::select("Variable", "Branching logic"),
                      by.x = "Variables", by.y = "Variable", all.x = TRUE)
      report[, "Branching logic"] <- stringr::str_replace_na(report[, "Branching logic"], "-")
    }

    # Reorder the report
    report <- report %>%
                dplyr::arrange(dplyr::desc(.data$Total))

    # Report in case the by_dag argument is true
    if (by_dag %in% TRUE) {
      report[is.na(report)] <- "-"
      report_dag <- split(report, f = report$DAG)

      # Build structure of each report in order to appear in the RStudio Viewer tab
      for (i in 1:length(report_dag)) {
        report_dag[[i]] <- report_dag[[i]] %>%
          dplyr::select("DAG", names(report))
        report_dag[[i]] <- knitr::kable(report_dag[[i]],
                                        align = "ccccc",
                                        row.names = FALSE,
                                        caption = report_title,
                                        format = "html",
                                        longtable = TRUE)
        report_dag[[i]] <- kableExtra::kable_styling(report_dag[[i]],
                                                     bootstrap_options = c("striped", "condensed"),
                                                     full_width = FALSE)
        report_dag[[i]] <- kableExtra::row_spec(report_dag[[i]], 0,
                                                italic = FALSE,
                                                extra_css = "border-bottom: 1px solid grey")
      }

      # Definitive output
      def <- list(queries = split(queries, f = queries$DAG),
                  results = report_dag)

    } else {

      # If by_dag argument is false
      # If there is a branching logic, we cannot lose the branching logic column
      if (length(var_logic) > 0 & (!is.null(logics) | is.null(event_form)) & (!is.null(br_eval) | any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data)))) {

        report <- report %>%
          dplyr::select(-"DAG") %>%
          dplyr::group_by(.data$Variables, .data$Description, .data$Event, .data$Query, .data$`Branching logic`, .drop = FALSE) %>%
          dplyr::summarise("Total" = sum(.data$Total), .groups = "keep") %>%
          dplyr::select("Variables", "Description", "Event", "Query", "Total", "Branching logic")

      } else {

        report <- report %>%
          dplyr::select(-"DAG") %>%
          dplyr::group_by(.data$Variables, .data$Description, .data$Event, .data$Query, .drop = FALSE) %>%
          dplyr::summarise("Total" = sum(.data$Total), .groups = "keep")

      }

      if (any(!report$Total %in% 0)) {
        if (report_zeros == TRUE) {

          report <- report %>%
            dplyr::arrange(dplyr::desc(.data$Total))

          report$Query[report$Total %in% 0]

        } else {

          report <- report %>%
            dplyr::arrange(dplyr::desc(.data$Total)) %>%
            dplyr::filter(.data$Total != 0)

        }
      }

      report[is.na(report)] <- "-"

      # Build structure of each report in order to appear in the RStudio Viewer tab
      result <- knitr::kable(report, "pipe",
                             align = "ccccc",
                             caption = report_title)
      viewer <- knitr::kable(report,
                             align = "ccccc",
                             row.names = FALSE,
                             caption = report_title,
                             format = "html",
                             longtable = TRUE)
      viewer <- kableExtra::kable_styling(viewer,
                                          bootstrap_options = c("striped", "condensed"),
                                          full_width = FALSE)
      viewer <- kableExtra::row_spec(viewer, 0,
                                     italic = FALSE,
                                     extra_css = "border-bottom: 1px solid grey")

      # Definitive output
      def <- list(queries = dplyr::tibble(queries),
                  results = viewer)
    }

    # Return the final product
    def
  }


