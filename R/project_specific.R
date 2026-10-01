#' Read Project Data
#'
#' Reads the `project` table from the metrics database, with the researcher's
#' role, school, and department (from `person_history`), and adds calendar and
#' fiscal year columns with [add_year_info()], using `start_date`.
#'
#' `start_date` is derived during ingestion from the first available of the
#' actual start date, estimated start date, September 1 of the `fy_start`
#' fiscal year, and the SharePoint creation date. `start_date_source` records
#' which one was used (`"actual"`, `"estimated"`, `"fiscal_year"`, or
#' `"created"`). Calendar dates (e.g. `cal_month_`) are only meaningful for `"actual"` and
#' `"estimated"`; fiscal years are meaningful for all but `"created"`.
#'
#' @param con a connection to the metrics database, e.g. from [get_metrics_db_conn()]
#'
#' @return a data frame with one row per project, including `date_`,
#'   `cal_year_`, `cal_month_`, `cal_day_`, and `fis_year_`
#' @seealso [read_workshop_data()], [read_consult_data()], [read_byod_data()]
#' @export
read_project_data <- function(con) {
  start_date <- NULL

  con %>%
    dplyr::tbl("project") %>%
    join_person_info(con) %>%
    dplyr::collect() %>%
    add_year_info(start_date) %>%
    dplyr::select(dplyr::contains("id"), dplyr::everything())
}
