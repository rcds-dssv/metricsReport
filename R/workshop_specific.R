#' @export
read_workshop_data <- function(con) {
  start_date <- NULL

  con %>%
    dplyr::tbl("workshop") %>%
    dplyr::collect() %>%
    add_year_info(start_date) %>%
    # order columns
    select(
      id, series, name, eventbrite_id, smartsheet_rid,
      focus_area, focus_dssv, focus_rdm, focus_rcs,
      provider, rcs_taught,
      start_date, hours,
      registration, attendance, recording_views, waitlist,
      surveys, recommend, learn,
      campus, event_type,
      fee, open_registration, subtopic,
      ai, bash, bio_genomics, cloud, data_management, ern, gis,
      git, globus, gpu, julia, matlab, python, quest, r, sql, statistics,
      visualization, subtopic_other, subtopic_other_text,
      date_, cal_year_, cal_quarter_, fis_year_, fis_quarter_,
      quarter_name_
    )
}

#' @importFrom rlang .data
#' @export
summarise_workshops_missing <- function(d) {
  columns_to_select <- c(
    "id", "smartsheet_rid", "series", "name", "quarter_name_", "start_date",
    "registration", "attendance", "hours"
  )

  d %>%
    dplyr::filter(is.na(.data[["registration"]]) | is.na(.data[["attendance"]]) | is.na(.data[["hours"]])) %>%
    dplyr::select(dplyr::all_of(columns_to_select))
}

#' @importFrom rlang .data
#' @export
fill_missing_registrations <- function(d, con) {
  # uses workshop registration table to get number of registrations
  # if the workshop doesn't exist in the registration table,
  # then use attendance number if it exists
  n_reg <- registration <- NULL

  workshop_missing_reg <- d %>%
    dplyr::filter(is.na(.data[["registration"]])) %>%
    dplyr::pull(dplyr::all_of("id"))

  workshop_reg_numbers <- con %>%
    dplyr::tbl("workshop_registration") %>%
    dplyr::filter(.data[["workshop_id"]] %in% workshop_missing_reg) %>%
    dplyr::select(.data[["workshop_id"]], .data[["person_id"]]) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[["workshop_id"]]) %>%
    dplyr::summarise(n_reg = dplyr::n()) %>%
    dplyr::collect()

  d <- d %>%
    dplyr::left_join(workshop_reg_numbers, by = c("id" = "workshop_id")) %>%
    dplyr::mutate(
      registration = dplyr::coalesce(
        .data[["registration"]],
        .data[["n_reg"]],
        .data[["attendance"]])
    ) %>%
    dplyr::select(-dplyr::all_of("n_reg"))

  return(d)
}

#' @export
fill_missing_hours <- function(d) {
  hours <- NULL
  d %>%
    dplyr::mutate(hours = ifelse(is.na(hours), 1, hours))
}

#' @importFrom rlang .data
#' @export
fill_missing_attendance <- function(d, attendance_rate = NA, use_model = FALSE) {

  registration <- attendance <- hours <- event_type <- fee <-
    bio_genomics <- cloud <- data_management <- globus <- gis <-
    julia <- matlab <- python <- quest <- r <- sql <- statistics <-
    visualization <- event_type_Virtual <- Other <-  NULL

  if (!use_model) {

    attendance_rate <- sum(d[["attendance"]], na.rm = TRUE) / sum(d[["registration"]], na.rm = TRUE)

    message("Estimated attendance rate: ", round(attendance_rate, 2))

    return(d %>%
      dplyr::mutate(attendance = ifelse(is.na(attendance), as.integer(ceiling(registration*attendance_rate)), attendance)))
  }

  else {

    ws <- d %>%
      # remove cancelled workshops and workshops with missing attendance / reg info
      dplyr::filter(!stringr::str_detect(.data[["name"]], "Next Steps in Python: Lunch Lessons: List Comprehensions"),
             !stringr::str_detect(.data[["name"]], "Next Steps in Python: Lunch Lessons: Efficient computing with NumPy"),
             !is.na(.data[["registration"]]),
             !is.na(.data[["attendance"]])) %>%
      # limit training to workshops on and after FY2021
      dplyr::filter(as.numeric(as.character(.data[["fis_year_"]])) >= 2021) %>%
      # if attendance exceeds registration, then set registration number
      # to attendance
      dplyr::mutate(
        registration = dplyr::case_when(
          .data[["attendance"]] > .data[["registration"]] ~ .data[["attendance"]],
          TRUE ~ .data[["registration"]]
        ))

    # Define preprocessing
    workshops_rec <- recipes::recipe(
      1 ~ registration + attendance + hours + event_type + fee + bio_genomics + cloud + data_management +
        globus + gis + julia + matlab + python + quest + r + sql + statistics + visualization,
      data = ws
    ) %>%
      recipes::step_mutate(
        large_registration = registration > 100,
        data_management = cloud | data_management | globus,
        Other = julia | matlab | sql | gis,
        quest = quest | bio_genomics,
        fee = tidyr::replace_na(fee, FALSE),
        hours = tidyr::replace_na(hours, 1)
      ) %>%
      recipes::step_mutate_at(recipes::all_logical_predictors(), fn = as.integer) %>%
      recipes::step_dummy(event_type) %>%
      recipes::prep(training = ws)

    ws_train <- recipes::bake(workshops_rec, new_data = ws)

    logistic_mod <- stats::glm(
      attendance/registration ~ hours + fee + event_type_Virtual + data_management + python + quest + r +
        statistics + visualization + Other,
      data = ws_train, family = stats::binomial, weights = registration)

    att_pred <- as.integer(stats::predict(logistic_mod, recipes::bake(workshops_rec, new_data = d), type = "response") * d[["registration"]])

    d <- d %>%
      dplyr::mutate(attendance = ifelse(is.na(attendance), att_pred, attendance))

    return(d)
  }

}

#' @importFrom rlang .data
#' @export
drop_library <- function(d) {
  d %>%
    dplyr::filter(.data[["provider"]] != "Library")
}

#' @export
combine_languages <- function(d, languages) {
  other_language <- NULL
  combine_cols(d, other_language, languages)
}

#' Set Topic
#' @param d workshop dataset
#' @param topic_categories a character vector whose name is the column and the value is the corresponding topic
#' @export
set_topic <- function(d, topic_categories, category_order = NULL, blank_category = "") {
  if (is.null(category_order)) {
    category_order <- unique(topic_categories)
  }

  d <- d %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      topic = paste(unique(topic_categories[dplyr::c_across(dplyr::all_of(names(topic_categories)))]), collapse = ","),
      topic = ifelse(topic == "", blank_category, topic)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::separate_rows(topic, sep = ",") %>%
    dplyr::mutate(topic = factor(topic, levels = unique(c(category_order, blank_category))))

  return(d)
}

# Functions to group data according to some meaningful category -----------

#' @importFrom rlang .data
#' @export
group_by_quarter <- function(d) {
  d %>%
    dplyr::group_by(.data[["quarter_name_"]], .add = TRUE)
}

#' @importFrom rlang .data
#' @export
group_by_team <- function(d) {

  d %>%
    dplyr::mutate(focus = dplyr::case_when(
      focus_dssv ~ "DSSV",
      focus_rdm ~ "RDM",
      focus_rcs ~ "RCS",
      TRUE ~ "Other"
    )) %>%
    dplyr::group_by(.data[["focus"]], .add = TRUE)
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

#' @importFrom rlang .data
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
    d <- d %>% dplyr::group_by(dplyr::across({{ grouping_variable }}), .add=TRUE)
  }

  d %>%
    dplyr::mutate(attendance_hour = .data[["attendance"]] * .data[["hours"]]) %>%
    dplyr::summarise(
      n_workshops = dplyr::n(),
      n_registrations = sum(.data[["registration"]], na.rm = TRUE),
      n_attendance = sum(.data[["attendance"]], na.rm = TRUE),
      n_hours = sum(.data[["hours"]], na.rm = TRUE),
      n_attendance_hour = sum(.data[["attendance_hour"]], na.rm = TRUE)
    )
}

#' @export
summarise_statistical_workshops_metrics <- function(
    d,
    group_by_quarter = TRUE,
    group_by_team = TRUE
  ) {

  statistics <- NULL

  summarise_workshops_metrics(
    d,
    grouping_variable = statistics,
    group_by_quarter = group_by_quarter,
    group_by_team = group_by_team,
    remove_library = TRUE
  )
}

#' @importFrom rlang .data
#' @export
pivot_summary_longer <- function(metric_summary) {
  metric_summary %>%
    dplyr::mutate(
      n_workshops = as.numeric(.data[["n_workshops"]]),
      n_registrations = as.numeric(.data[["n_registrations"]]),
      n_attendance = as.numeric(.data[["n_attendance"]]),
      n_attendance_hour = as.numeric(.data[["n_attendance_hour"]])
    ) %>%
    tidyr::pivot_longer(
      cols = c("n_workshops", "n_registrations", "n_attendance", "n_hours", "n_attendance_hour"),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(metric = factor(metric, levels = c("n_workshops", "n_registrations", "n_attendance", "n_hours", "n_attendance_hour")))
}
