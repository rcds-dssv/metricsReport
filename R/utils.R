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

reorder_quarters <- function(d) {
  quarter_order <- 2010:2030 %>%
    purrr::map(~ paste0(.x, " Q", 1:4), ) %>%
    unlist()
  quarter_names <- 2010:2030 %>%
    purrr::map(~ paste(c("Winter", "Spring", "Summer", "Fall"), .x)) %>%
    unlist()
  names(quarter_names) <- quarter_order

  quarters <- paste0(d$cal_year_, " Q", d$cal_quarter_)
  i <- match(unique(quarters), quarter_order)
  quarter_names <- quarter_names[min(i):max(i)]

  d %>%
    dplyr::mutate(
      quarter_name_ = factor(quarter_names[quarters], levels = quarter_names)
    )
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

# Format a date-time as a "YYYY-MM-DD HH:MM:SS" string in Chicago time (NA stays NA),
# so it is written to csv files as the local clock time.
format_chicago_datetime <- function(x) {
  out <- format(lubridate::with_tz(x, "America/Chicago"), "%Y-%m-%d %H:%M:%S")
  out[is.na(x)] <- NA_character_
  out
}

#' @importFrom rlang .data
#' @export
add_year_info <- function(d, date_col) {
  # combine all combination of year and quarter in order

  d %>%
    dplyr::mutate(
      date_ = {{ date_col }},
      cal_year_ = lubridate::year(.data[["date_"]]),
      cal_month_  = lubridate::month(date_),
      cal_day_  = lubridate::day(date_),
      cal_quarter_ = lubridate::quarter(.data[["date_"]]),
      fis_year_ = .data[["cal_year_"]] + ifelse(cal_month_ >= 9, 1, 0),
      fis_quarter_ = (((cal_month_ - 9) %% 12) %/% 3) + 1
    ) %>%
    reorder_quarters() %>%
    dplyr::mutate(
      cal_year_ = factor(cal_year_),
      cal_month_ = factor(cal_month_),
      cal_day_ = factor(cal_day_),
      cal_quarter_ = factor(cal_quarter_, c(1,2,3,4)),
      fis_year_ = factor(fis_year_),
      fis_quarter_ = factor(fis_quarter_, c(1,2,3,4))
    )
}

#' @export
filter_by_date <- function(d, from_date="2010-01-01", to_date="2099-12-31") {
  d %>%
    dplyr::filter(.data[["date_"]] >= lubridate::ymd(from_date), .data[["date_"]] <= lubridate::ymd(to_date)) %>%
    reorder_quarters()
}

#' @importFrom rlang .data
#' @export
filter_by_quarter <- function(d, from_quarter, to_quarter) {
  yq1 <- as.integer(stringr::str_extract(from_quarter, "^([[:digit:]]{4}).*([[:digit:]]{1})$", group = c(1,2)))
  yq2 <- as.integer(stringr::str_extract(to_quarter, "^([[:digit:]]{4}).*([[:digit:]]{1})$", group = c(1,2)))

  if (any(is.na(c(yq1, yq2)))) {
    warning("Invalid quarter format. Returning original data frame")
  }

  d %>%
    mutate(
      cal_year_tmp_ = as.numeric(as.character(.data[["cal_year_"]])),
      cal_quarter_tmp_ = as.numeric(as.character(.data[["cal_quarter_"]]))
    ) %>%
    dplyr::filter(
      (.data[["cal_year_tmp_"]] == yq1[1] & .data[["cal_quarter_tmp_"]] >= yq1[2]) |
      (.data[["cal_year_tmp_"]] == yq2[1] & .data[["cal_quarter_tmp_"]] <= yq2[2]) |
      (.data[["cal_year_tmp_"]] > yq1[1] & .data[["cal_year_tmp_"]] < yq2[1])
    ) %>%
    dplyr::select(-cal_year_tmp_, -cal_quarter_tmp_) %>%
    reorder_quarters()
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



