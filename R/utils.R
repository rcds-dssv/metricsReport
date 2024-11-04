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

#' @export
add_year_info <- function(d, date_col) {
  d %>%
    dplyr::mutate(
      date_ = {{ date_col }},
      cal_year_ = lubridate::year(date_),
      cal_quarter_ = lubridate::quarter(date_),
      fis_year_ = cal_year_ + ifelse(cal_quarter_ <= 2, 0, 1)
    )
}

#' @export
filter_by_date <- function(d, from_date="2010-01-01", to_date="2099-12-31") {
  d %>%
    dplyr::filter(date_ >= lubridate::ymd(from_date), date_ <= lubridate::ymd(to_date))
}

#' @export
filter_by_quarter <- function(d, from_quarter, to_quarter) {
  yq1 <- as.integer(stringr::str_extract(from_quarter, "^([[:digit:]]{4}).*([[:digit:]]{1})$", group = c(1,2)))
  yq2 <- as.integer(stringr::str_extract(to_quarter, "^([[:digit:]]{4}).*([[:digit:]]{1})$", group = c(1,2)))

  if (any(is.na(c(yq1, yq2)))) {
    warning("Invalid quarter format. Returning original data frame")
  }

  d %>%
    dplyr::filter(
      (cal_year_ == yq1[1] & cal_quarter_ >= yq1[2]) |
      (cal_year_ == yq2[1] & cal_quarter_ <= yq2[2]) |
      (cal_year_ > yq1[1] & cal_year_ < yq2[1])
    )
}

# combine multiple boolean columns into one such that
# if any condition is true then the new column is true
#' @export
combine_cols <- function(d, grouped_colname, colnames) {
  d %>%
    dplyr::mutate(
      {{ grouped_colname }} := if_any(all_of(colnames), ~.x)
    )
}



