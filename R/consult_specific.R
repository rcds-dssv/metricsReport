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
#' @seealso [read_workshop_data()], [read_byod_data()], [read_project_data()]
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
