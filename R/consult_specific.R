#' Read Consult Data
#'
#' Reads the `consult` table from the metrics database, with the client's role,
#' school, and department (from `person_history`, as of the consult), and adds
#' calendar and fiscal year columns with [add_year_info()], using `created`.
#'
#' `created` is a timestamp in the database. It is converted to a date in Chicago
#' time (so a request submitted in the evening is not dated the next day in UTC).
#' Some consults have a `response_date` earlier than the date they were created;
#' for these, `created` is set to `response_date`. `created_datetime` keeps the
#' time the request was submitted, as an ISO 8601 string in Chicago time with its
#' UTC offset, e.g. `"2026-09-30T14:22:34-05:00"` (`NA` when `created` was
#' replaced by `response_date`, since the time would then belong to a different
#' day). `response_time` is the number of days
#' from the request to the first response.
#'
#' @param con a connection to the metrics database, e.g. from [get_metrics_db_conn()]
#'
#' @return a data frame with one row per consult, including `date_`,
#'   `cal_year_`, `cal_month_`, `cal_day_`, and `fis_year_`
#' @seealso [read_workshop_data()], [read_byod_data()], [read_project_data()],
#'   [read_consult_survey_data()]
#' @export
read_consult_data <- function(con) {
  created <- created_local <- created_date <- response_date <- NULL

  con %>%
    dplyr::tbl("consult") %>%
    join_person_info(con) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      created_local = lubridate::with_tz(created, "America/Chicago"),
      created_date = lubridate::as_date(created_local)
    ) %>%
    dplyr::mutate(
      response_time = as.numeric(difftime(response_date, created_date, units = "days")),
      created = dplyr::case_when(
        is.na(response_date) ~ created_date,
        created_date > response_date ~ response_date,
        TRUE ~ created_date
      ),
      created_datetime = dplyr::if_else(created == created_date,
                                        format_chicago_datetime(created_local), NA_character_)
    ) %>%
    dplyr::select(-"created_local", -"created_date") %>%
    add_year_info(created) %>%
    dplyr::select(dplyr::contains("id"), dplyr::everything())
}

#' Read Consult Survey Data
#'
#' Reads the `consult_survey` table from the metrics database, with the
#' respondent's role, school, and department (from `person_history`, as of
#' their most recent consult before the survey), and adds calendar and fiscal
#' year columns with [add_year_info()], using `recorded_date`.
#'
#' `recorded_date` is a timestamp in the database. It is converted to a date in
#' Chicago time (so a response recorded in the evening is not dated the next day
#' in UTC). `recorded_datetime` keeps the time the response was recorded, as an
#' ISO 8601 string in Chicago time with its UTC offset, e.g.
#' `"2026-05-06T10:35:30-05:00"`.
#'
#' Responses are not matched to a specific consult, since a person may have had
#' several consults. Unfinished responses (`finished` is `FALSE`) only answered
#' `how_satisfied`. `how_consult_helped` holds the checked options separated by
#' `"; "`, e.g. `"Saved me a lot of time; I learned something new"`.
#'
#' @param con a connection to the metrics database, e.g. from [get_metrics_db_conn()]
#'
#' @return a data frame with one row per survey response, including `date_`,
#'   `cal_year_`, `cal_month_`, `cal_day_`, and `fis_year_`
#' @seealso [read_consult_data()]
#' @export
read_consult_survey_data <- function(con) {
  recorded_date <- NULL

  con %>%
    dplyr::tbl("consult_survey") %>%
    join_person_info(con) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      recorded_datetime = format_chicago_datetime(recorded_date),
      recorded_date = lubridate::as_date(lubridate::with_tz(recorded_date, "America/Chicago"))
    ) %>%
    add_year_info(recorded_date) %>%
    dplyr::select(dplyr::contains("id"), dplyr::everything())
}
