#' @export
close_if_open <- function(conn) {
  tryCatch({
    close(conn)
  },
  error = function(e) {
    invisible()
  })
}

#' @export
get_env_var <- function(var_name, env_file = NULL) {
  envvar <- Sys.getenv(var_name)
  if (envvar == "" && !is.null(env_file)) {
    readRenviron(env_file)
    envvar <- Sys.getenv(var_name)
  }

  if (envvar == "") {
    warning(paste0("Environment variable ", var_name, " not set."))
  }

  return(envvar)
}

#' @export
all_roles <- function() {
  return(c(
    "Undergraduate Student", "Graduate Student", "Master's Student",
    "PhD Student", "Professional Student", "Faculty", "Staff", "Postdoc",
    "Other"
  ))
}

#' @export
all_shcools <- function() {
  return(c(
    "Weinberg", "TGS", "McCormick", "Communication", "SESP", "Medill",
    "Law", "Kellogg", "Feinberg", "Bienen", "SPS", "Admin", "NW Medicine",
    "Other", "External", "NU-Q"
  ))
}

# Join each row's person information (role, school, department at the time of the
# touch-point) from the person_history table, by history_id. `d` is a lazy table
# from the same connection, so the join runs in the database before collecting.
join_person_info <- function(d, con) {
  d %>%
    dplyr::left_join(
      dplyr::tbl(con, "person_history") %>%
        dplyr::select("id", "role", "school", "department"),
      by = c(history_id = "id")
    )
}

# Format a date-time as an ISO 8601 string in Chicago time with its UTC offset, e.g.
# "2026-09-30T14:22:34-05:00" (-05:00 in daylight saving time, -06:00 otherwise; NA stays NA). The string
# shows the Chicago clock time, and the offset makes the moment unambiguous: readr::read_csv(),
# lubridate::ymd_hms(), and Python's datetime.fromisoformat() and pandas.to_datetime() all read it as the
# right time.
format_chicago_datetime <- function(x) {
  out <- format(lubridate::with_tz(x, "America/Chicago"), "%Y-%m-%dT%H:%M:%S%z")
  # %z gives "-0500"; ISO 8601 (and Python before 3.11) wants "-05:00"
  out <- sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", out)
  out[is.na(x)] <- NA_character_
  out
}

#' Add Calendar and Fiscal Year Columns
#'
#' Adds `date_` (a copy of `date_col`), `cal_year_`, `cal_month_`, `cal_day_`,
#' and `fis_year_` (the fiscal year starts on Sept. 1, so e.g. Sept. 2025 is in
#' fiscal year 2026). Quarters are not added; use [month_to_quarter()] if you
#' need them.
#'
#' @param d a data frame
#' @param date_col the (unquoted) date column to use
#'
#' @return `d` with the columns above added, each as a factor
#' @importFrom rlang .data
#' @export
add_year_info <- function(d, date_col) {
  d %>%
    dplyr::mutate(
      date_ = {{ date_col }},
      cal_year_ = lubridate::year(.data[["date_"]]),
      cal_month_  = lubridate::month(date_),
      cal_day_  = lubridate::day(date_),
      fis_year_ = .data[["cal_year_"]] + ifelse(cal_month_ >= 9, 1, 0)
    ) %>%
    dplyr::mutate(
      cal_year_ = factor(cal_year_),
      cal_month_ = factor(cal_month_),
      cal_day_ = factor(cal_day_),
      fis_year_ = factor(fis_year_)
    )
}

#' @export
filter_by_date <- function(d, from_date="2010-01-01", to_date="2099-12-31") {
  d %>%
    dplyr::filter(.data[["date_"]] >= lubridate::ymd(from_date), .data[["date_"]] <= lubridate::ymd(to_date))
}

# combine multiple boolean columns into one such that
# if any condition is true then the new column is true
#' @importFrom rlang :=
#' @export
combine_cols <- function(d, grouped_colname, colnames) {
  d %>%
    dplyr::mutate(
      {{ grouped_colname }} := dplyr::if_any(dplyr::all_of(colnames), ~.x)
    )
}



