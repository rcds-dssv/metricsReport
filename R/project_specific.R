#' Read Project Data
#'
#' Reads the `project` table from the metrics database and adds calendar and
#' fiscal year columns with [add_year_info()], using `start_date`.
#'
#' `start_date` is derived during ingestion from the first available of the
#' actual start date, estimated start date, September 1 of the `fy_start`
#' fiscal year, and the SharePoint creation date. `start_date_source` records
#' which one was used (`"actual"`, `"estimated"`, `"fiscal_year"`, or
#' `"created"`). Calendar quarters are only meaningful for `"actual"` and
#' `"estimated"`; fiscal years are meaningful for all but `"created"`.
#'
#' @param con a connection to the metrics database, e.g. from [get_metrics_db_conn()]
#'
#' @return a data frame with one row per project, including `date_`,
#'   `cal_year_`, `cal_month_`, `cal_day_`, `cal_quarter_`, `fis_year_`,
#'   `fis_quarter_`, and `quarter_name_`
#' @export
read_project_data <- function(con) {
  start_date <- NULL

  con %>%
    dplyr::tbl("project") %>%
    dplyr::collect() %>%
    add_year_info(start_date)
}
