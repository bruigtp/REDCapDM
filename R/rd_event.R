#' Identification of missing event(s)
#'
#' When working with a longitudinal REDCap project, the exported data has a structure where each row represents one event per record. However, by default REDCap does not export events for which there is no information available.
#' This function allows you to identify which records do not contain information about a particular event.
#'
#' @param ... List containing the data, dictionary and event mapping (if required) of the REDCap project. This should be the output of the `redcap_data` function.
#' @param data Data frame containing the data read from REDCap. If the list is specified, this argument is not required.
#' @param dic Data frame containing the dictionary read from REDCap. If the list is specified, this argument is not required.
#' @param event Character vector with the name of the REDCap event(s) to be analyzed.
#' @param filter A filter to be applied to the dataset. This argument can be used to identify the missing events on a subset of the dataset.
#' @param query_name Description of the query. It can be the same for all variables, or you can define a different one for each variable. By default, the function defines it as `The event [event] is missing' for each event`.
#' @param addTo Data frame corresponding to a previous query data frame to which you can add the new query data frame. By default, the function always generates a new data frame without taking into account previous reports.
#' @param report_title Character string specifying the title of the report.
#' @param report_zeros Logical. If `TRUE`, the function returns a report containing variables with zero queries.
#' @param link List containing project information used to create a web link to each missing event.
#' @return A list with a data frame of 9 columns (10 columns, if the link argument is specified) meant to help the user identify each missing event and a table with the total number of missing events per event analyzed.
#'
#' @examples
#' example <- rd_event(covican,
#'                     event = "follow_up_visit_da_arm_1")
#' example
#' @export
#' @importFrom rlang .data

rd_event <- function(..., data = NULL, dic = NULL, event, filter = NA, query_name = NA, addTo = NA, report_title = NA, report_zeros = FALSE, link = list())
  {
    # If the entire list resulting from the 'redcap_data' function is used
    project <- c(...)

    if(!is.null(project)){

      if (!is.null(data)) {

        warning("Data has been specified twice so the function will not use the information in the data argument.")

      }

      if (!is.null(dic)) {

        warning("Dictionary has been specified twice so the function will not use the information in the dic argument.")

      }

      data <- project$data
      dic <- project$dictionary
    }

    # Making sure that the object data is a data.frame
    data <- as.data.frame(data)

    # Creation of the structure of the queries
    queries <- as.data.frame(matrix(ncol = 10, nrow = 0))
    colnames(queries) <- c("Identifier", "DAG", "Event", "Instrument", "Field", "Repetition", "Description", "Query", "Code", "Link")

    # Naming the first column of the REDCap's database as record_id
    if (all(!names(data) == "record_id")) {
      names(data)[1] <- "record_id"
    }

    # Error: More than one filter applied.
    if (!is.na(filter) & length(filter) > 1) {
      stop("More than one filter applied, please select only one.", call. = FALSE)
    }

    # We save de original dataset
    data0 <- data

    # Filtering the data using the information of the argument 'filter'
    if (!is.na(filter) & length(filter) == 1) {

      # Error: logic used in the filter is incorrect
      command <- paste0("data", "<-dplyr::filter(data,", filter, ")")

      evaluation <- try(eval(parse(text = command)), silent = TRUE)

      if(inherits(evaluation, "try-error")){

        stop("The logic used in the filter is incorrect. Please review and adjust the filter's logic.")

      } else {

        eval(parse(text = command))

      }

      # Error: filtering results in zero observations
      if (nrow(data) == 0) {
        warning("The applied filter is accurate, but it does not correspond to any observations. Please ensure that you have selected the appropriate filter.", call. = FALSE)
      }

    }

    # Applying a filter of the chosen events to the database
    if (all(!is.na(event))) {

      # Error: one of the specified events does not exist in the database
      if (any(c("redcap_event_name", "redcap_event_name.factor") %in% names(data0))) {
        if (any(!event %in% data0$redcap_event_name) & any(!event %in% data0$redcap_event_name.factor)) {
          stop("One of the events mentioned does not exist in the database, please verify the argument 'event'.")
        }
      }

      # Identify queries for each event
      for (k in 1:length(event)) {

        # If the event is specified in the factor form
        if (any(names(data0) == "redcap_event_name.factor") & all(event %in% data0$redcap_event_name.factor)) {
          ids <- data0$record_id[data0$redcap_event_name.factor %in% event[k]]
        }

        # If the event is specified in the raw form
        if (all(event %in% data0$redcap_event_name)) {
          ids <- data0$record_id[data0$redcap_event_name %in% event[k]]
        }


        # Identification of the record_ids that do not present the events specified
        raw <- data %>%
                dplyr::filter(!data$record_id %in% ids)
        raw <- raw %>%
                dplyr::slice(rep(1:dplyr::n(), each = length(event[k])))

        # Identification of queries, using the structure built before
        if (nrow(raw) > 0) {

          # Retrieve the names of the missing events - raw version
          if ("redcap_event_name" %in% names(raw)) {
            raw$redcap_event_name <- rep(event[k], length(event[k]))
          }

          # Retrieve the names of the missing events - factor version
          if ("redcap_event_name.factor" %in% names(raw)) {
            for (i in 1:nrow(raw)) {

              raw$redcap_event_name.factor[i] <- unique(data0$redcap_event_name.factor[data0$redcap_event_name %in% raw$redcap_event_name[i]])

            }
          }

          # Queries
          x <- raw[, c("record_id", grep("redcap", names(raw), value = T))]
          excel <- data.frame(
            Identifier = x[, "record_id"],
            DAG = if (any(c("redcap_data_access_group", "redcap_data_access_group.factor") %in% names(x))) {
              ifelse("redcap_data_access_group.factor" %in% names(x),
                     as.character(x[, "redcap_data_access_group.factor"]),
                     as.character(x[, "redcap_data_access_group"]))
            } else {
              "-"
            },
            Event = rep(event[k], length(event[k])),
            Instrument = "-",
            Field = "-",
            Repetition = "-",
            Description = if ("redcap_event_name.factor" %in% names(x)) {
              as.character(x[, "redcap_event_name.factor"])
            } else{
              "-"
            },
            Query = if (!is.na(query_name)) {
              if (length(query_name) > 1) {
                query_name[k]
              } else {
                query_name
              }
            } else {
              paste0("The event '", if ("redcap_event_name.factor" %in% names(x)) {
                as.character(x[, "redcap_event_name.factor"])
              } else {
                as.character(x[, "redcap_event_name"])
              }
              , "' is missing.")
            },
            Code = "",
            stringsAsFactors = FALSE
          )

          # Add the column Link
          if (all(c("domain", "redcap_version", "proj_id") %in% names(link))) {
            excel[, "Link"] <- paste0("https://", link[["domain"]], "/redcap_v", link[["redcap_version"]], "/DataEntry/record_home.php?pid=", link[["proj_id"]], "&id=", x[, "record_id"])
          }

          # Add the identified queries to the structure
          queries <- rbind(queries, excel)
        }
      }
    }

    # If the argument 'addTo' is specified, combine the queries generated with a previous data frame of queries
    if (!is.na(addTo)) {
      col_names <- names(queries)
      queries <- merge(queries,
                       addTo$queries,
                       by = intersect(names(addTo$queries), names(queries)),
                       all = TRUE)
      queries <- queries %>% dplyr::select(col_names)
    }

    # Classify each query with it's own code
    if (nrow(queries) != 0) {

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
      report <- data.frame("var" = queries$Event,
                           "descr" = queries$Description)

      # If there is no previous report specified we convert the events and their description to factors using the dictionary
      if (all(addTo %in% NA)) {
        report$var <- factor(report$var, levels = c(unique(event)))
        report$descr <- factor(report$descr)
      }

      # Report of all variables (including the ones with zero queries)
      if (report_zeros == TRUE) {
        report <- report %>%
          dplyr::group_by(.data$var, .drop = FALSE) %>%
          dplyr::summarise("total" = dplyr::n())
      } else {
        report <- report %>%
          dplyr::group_by(.data$var, .drop = TRUE) %>%
          dplyr::summarise("total" = dplyr::n())
      }

    } else {
      # If there is none query, the function still creates a report containing all selected variables.

      # Message: if there is none query to be identified
      message("There is no query to be identified.")

      report <- as.data.frame(matrix(ncol = 2, nrow = length(event)))
      colnames(report) <- c("var", "descr")

      report$var <- event
      report$descr <- if ("redcap_event_name.factor" %in% names(data)) {
        as.character(unique(data0$redcap_event_name.factor[which(data0$redcap_event_name %in% event)]))
      } else{
        "-"
      }
      report$total <- 0

    }

    # We check if there is more than one report_title and if it isn't the case we stabilish the caption for the report_title
    if (all(is.na(report_title))) {
      report_title <- "Report of queries"
    } else {
      if (length(report_title) > 1) {
        stop("There is more than one title for the report, please choose only one.", call. = FALSE)
      }
    }

    # Arrange report
    report <- report %>%
      dplyr::mutate(descr = unique(data0$redcap_event_name.factor[data0$redcap_event_name %in% report$var])) %>%
      dplyr::select("var", "descr", "total") %>%
      dplyr::arrange(dplyr::desc(.data$total))
    names(report) <- c("Events", "Description", "Total")
    rownames(report) <- NULL

    # Viewer
    result <- knitr::kable(report,
                           align = c("ccccc"),
                           row.names = FALSE,
                           caption = report_title,
                           format = "html",
                           longtable = TRUE)
    result <- kableExtra::kable_styling(result,
                                        bootstrap_options = c("striped", "condensed"),
                                        full_width = FALSE)
    result <- kableExtra::row_spec(result, 0, italic = FALSE, extra_css = "border-bottom: 1px solid grey")

    # Return the final product
    list(queries = queries,
         results = result)
  }
