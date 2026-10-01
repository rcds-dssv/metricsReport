#' Read BYOD Data
#'
#' Reads the `byod` table from the metrics database, with each participant's
#' role, school, and department (from `person_history`), and adds calendar and
#' fiscal year columns with [add_year_info()].
#'
#' BYOD is recorded by quarter (`quarter` 1 = Fall, 2 = Winter, 3 = Spring,
#' 4 = Summer, and the calendar `year` of that quarter), not by date. So each
#' row is dated at the start of its quarter (Fall = Sept. 1, Winter = Jan. 1,
#' Spring = Apr. 1, Summer = July 1), which puts Fall in the next fiscal year.
#' `quarter_string` is the quarter's name, as a factor in academic-year order.
#'
#' @param con a connection to the metrics database, e.g. from [get_metrics_db_conn()]
#'
#' @return a data frame with one row per BYOD participant per quarter, including
#'   `date_`, `cal_year_`, `cal_month_`, `cal_day_`, and `fis_year_`
#' @seealso [read_workshop_data()], [read_consult_data()], [read_project_data()]
#' @export
read_byod_data <- function(con) {
  quarter <- year <- quarter_start <- NULL

  con %>%
    dplyr::tbl("byod") %>%
    join_person_info(con) %>%
    dplyr::collect() %>%
    dplyr::select("smartsheet_rid", "person_id", "quarter", "year", "assigned_group",
                  "role", "school", "department") %>%
    dplyr::mutate(
      quarter_string = factor(c("Fall", "Winter", "Spring", "Summer")[quarter],
                              levels = c("Fall", "Winter", "Spring", "Summer")),
      quarter_start = as.Date(paste(year, c(9, 1, 4, 7)[quarter], "01", sep = "-"))
    ) %>%
    add_year_info(quarter_start) %>%
    dplyr::select(-"quarter_start") %>%
    dplyr::select(dplyr::contains("id"), dplyr::everything())
}
