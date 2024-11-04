#' @export
read_workshop_data <- function(con) {
  con %>%
    dplyr::tbl("workshop") %>%
    collect() %>%
    add_year_info(start_date)
}

#' @export
summarise_workshop_missing <- function(d) {
  d %>%
    dplyr::filter(is.na(registration) | is.na(attendance) | is.na(hours)) %>%
    select(id, smartsheet_rid, name, start_date, registration, attendance, hours)
}

#' @export
fill_missing_registrations <- function(d, con) {
  # uses workshop registration table to get number of registrations
  # if the workshop doesn't exist in the registration table,
  # then use attendance number if it exists

  workshop_missing_reg <- d %>%
    filter(is.na(registration)) %>%
    pull(id)

  workshop_reg_numbers <- con %>%
    dplyr::tbl("workshop_registration") %>%
    filter(workshop_id %in% workshop_missing_reg) %>%
    group_by(workshop_id) %>%
    summarise(n_reg = n()) %>%
    collect()

  d <- d %>%
    left_join(workshop_reg_numbers, by = c("id" = "workshop_id")) %>%
    mutate(registration = coalesce(registration, n_reg, attendance)) %>%
    select(-n_reg)

  return(d)
}

#' @export
fill_missing_hours <- function(d) {
  d %>%
    mutate(hours = ifelse(is.na(hours), 1, hours))
}

#' @export
fill_missing_attendance <- function(d) {
  # roughly the mean of attendance is 40% of registration
  d %>%
    mutate(attendance = ifelse(is.na(attendance), as.integer(ceiling(registration*0.4)), attendance))
}

#' @export
drop_library <- function(d) {
  d %>%
    dplyr::filter(provider != "Library")
}

#' @export
combine_languages <- function(d, languages) {
  combine_cols(d, other_language, languages)
}


# Functions to group data according to some meaningful category -----------

#' @export
group_by_quarter <- function(d) {
  d %>%
    dplyr::group_by(cal_year_, cal_quarter_, .add = TRUE)
}

#' @export
group_by_team <- function(d) {
  d %>%
    mutate(focus = case_when(
      focus_dssv ~ "DSSV",
      focus_rdm ~ "RDM",
      focus_rcs ~ "RCS",
      TRUE ~ "Other"
    )) %>%
    dplyr::group_by(focus, .add = TRUE)
}

#' @export
group_data_by <- function(d, by) {
  if (is.na(by)) {
    return(d)
  }

  if (stringr::str_detect(by, "quarter")) {
    d <- group_by_quarter(d)
  }
  if (stringr::str_detect(by, "team")) {
    d <- group_by_team(d)
  }

  return(d)
}

# Counting / Statistics Functions -----------

#' @export
summarise_workshops_metrics <- function(
    d,
    grouping_variable = NULL,
    group_by_quarter = TRUE,
    group_by_team = TRUE,
    remove_library = TRUE
  ) {

  if (remove_library) {
    d <- d %>% drop_library()
  }

  if (group_by_quarter) {
    d <- group_by_quarter(d)
  }

  if (group_by_team) {
    d <- group_by_team(d)
  }

  if (!missing(grouping_variable)) {
    print("Ho")
    d <- d %>% group_by({{ grouping_variable }}, .add=TRUE)
  }

  d %>%
    mutate(attendance_hour = attendance * hours) %>%
    dplyr::summarise(
      n_workshops = n(),
      n_registrations = sum(registration, na.rm = TRUE),
      n_attendance = sum(attendance, na.rm = TRUE),
      n_hours = sum(hours, na.rm = TRUE),
      n_attendance_hour = sum(attendance_hour, na.rm = TRUE)
    )
}

#' @export
summarise_statistical_workshops_metrics <- function(
    d,
    group_by_quarter = TRUE,
    group_by_team = TRUE
  ) {
  summarise_workshops_metrics(
    d,
    grouping_variable = statistics,
    group_by_quarter = group_by_quarter,
    group_by_team = group_by_team,
    remove_library = TRUE
  )
}

#' @export
count_languages <- function(
    d, grouping = NA,
    remove_library = TRUE,
    language_to_group = c()
  ) {

  d <- group_data_by(d, grouping)
}
