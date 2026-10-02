# General-purpose helpers for building report tables and plots (role/school
# recoding, breakdowns of a column over time, returning-client summaries,
# flextable/PDF fitting, and Sankey rendering). These are not specific to any one service (workshops,
# consults, ...) beyond the shared `fis_year_` / `person_id` conventions used
# throughout this package.

#' Fit a Flextable to a Maximum Width for PDF Output
#'
#' Rescales column widths (proportional to each column's max character count,
#' considering both header and body text) so the table fits within
#' `max_width`, then shrinks the font further via [flextable::fit_to_width()]
#' if still needed. Intended for PDF/LaTeX output, where flextable does not
#' autofit within the page width the way it does for HTML.
#'
#' @param ft a flextable object
#' @param max_width total table width, in inches
#' @param base_font starting font size, in points
#' @param min_font smallest font size to allow; not enforced by flextable, informational only
#' @param padding_in cell padding, in inches
#'
#' @return a flextable object resized to fit within `max_width`
#' @export
myflextablefitter <- function(ft,
                               max_width = 6.2,
                               base_font = 8,
                               min_font = 6,
                               padding_in = 0.06) {
  if (!inherits(ft, "flextable")) stop("myflextablefitter: input must be a flextable object (use %>% flextable() first).")

  # body data and cols
  data_df <- ft$body$dataset
  cols <- names(data_df)

  # --- compute max chars per column considering both body and header text ---
  # body text max
  body_max_chars <- vapply(cols, function(j) {
    max(nchar(as.character(data_df[[j]])), na.rm = TRUE)
  }, integer(1))

  # extract visible header labels (after set_header_labels)
  header_df <- ft$header$content$data
  header_max_chars <- vapply(seq_along(cols), function(i) {
    header_label <- as.character(header_df[[i]]$txt)
    as.integer(round(nchar(header_label) / 2)) # allow the header to wrap to 2 lines
  }, integer(1))

  # combine, ensure at least 1 char
  max_chars <- pmax(body_max_chars, header_max_chars, 1L)

  is_num <- vapply(data_df, is.numeric, logical(1))
  min_chars_for_numeric <- 4L
  max_chars[is_num] <- pmax(max_chars[is_num], min_chars_for_numeric)

  # cap extremely large header lengths (avoid one column dominating)
  max_chars <- pmin(max_chars, 60L)

  rel <- max_chars / sum(max_chars)
  widths_in <- rel * max_width

  # apply fixed layout + explicit widths
  ft <- flextable::set_table_properties(ft, layout = "fixed")
  for (i in seq_along(cols)) {
    ft <- flextable::width(ft, j = cols[i], width = widths_in[i], unit = "in")
  }

  # set font / padding
  ft <- flextable::fontsize(ft, size = base_font, part = "all")
  pt_padding <- padding_in * 72 # approx points per inch
  ft <- flextable::padding(ft, padding.top = pt_padding, padding.bottom = pt_padding, part = "all")

  # try to shrink to fit if still too wide (reduces font proportionally)
  ft <- flextable::fit_to_width(ft, max_width = max_width, unit = "in")

  ft
}

#' Apply Flextable PDF Fitting Only When Rendering to PDF
#'
#' Passes `ft` through [myflextablefitter()] when knitting to LaTeX/PDF output;
#' otherwise returns `ft` unchanged, since HTML output does not need the
#' fixed-width rescaling.
#'
#' @inheritParams myflextablefitter
#' @param ... additional arguments passed to [myflextablefitter()]
#'
#' @return a flextable object
#' @export
myflextablefitter_if_pdf <- function(ft, ...) {
  if (!inherits(ft, "flextable")) stop("myflextablefitter_if_pdf: input must be a flextable.")
  if (knitr::is_latex_output()) {
    myflextablefitter(ft, ...)
  } else {
    ft
  }
}

#' Recode Role and School into the Standard Reporting Categories
#'
#' Applies the same role/school recoding to any service's data (workshop
#' registrations, consults, BYOD, projects) so all services always use
#' identical categories:
#'
#' * missing or blank roles and schools become "Other"
#' * the "Graduate Student" role is recoded to "PhD Student"
#' * role becomes a factor with levels `role_order`
#' * "NW Medicine", "Lurie Childrens", and "SRA Lab" are combined into
#'   "Medical Affiliates"
#' * "Communication" and "Medill" are combined into "Communication/Medill"
#' * "Bienen", "Law", "NU-Q", and "TGS" (each small) are combined into
#'   "Bienen/Law/NU-Q/TGS"
#'
#' @param df a data frame containing at least the columns `role` and `school`
#' @param role_order the role factor levels, in display order. Any role not
#'   listed here becomes `NA`.
#'
#' @return `df` with `role` and `school` recoded as factors
#' @importFrom rlang .data
#' @export
recode_role_school <- function(df,
                               role_order = c(
                                 "Undergraduate Student", "Graduate Student", "Master's Student",
                                 "PhD Student", "Professional Student", "Postdoc", "Staff",
                                 "Faculty", "Other"
                               )) {
  df %>%
    dplyr::mutate(
      role = as.character(.data[["role"]]),
      role = ifelse(is.na(.data[["role"]]) | stringr::str_trim(.data[["role"]]) == "", "Other", .data[["role"]]),
      role = dplyr::if_else(.data[["role"]] == "Graduate Student", "PhD Student", .data[["role"]]),
      role = factor(.data[["role"]], levels = role_order)
    ) %>%
    dplyr::mutate(
      school = as.character(.data[["school"]]),
      school = ifelse(is.na(.data[["school"]]) | stringr::str_trim(.data[["school"]]) == "", "Other", .data[["school"]]),
      school = dplyr::case_when(
        .data[["school"]] %in% c("NW Medicine", "Lurie Childrens", "SRA Lab") ~ "Medical Affiliates",
        .data[["school"]] %in% c("Communication", "Medill") ~ "Communication/Medill",
        .data[["school"]] %in% c("Bienen", "Law", "NU-Q", "TGS") ~ "Bienen/Law/NU-Q/TGS",
        TRUE ~ .data[["school"]]
      ),
      school = factor(.data[["school"]])
    )
}

#' Summarize Returning Clients Between a Pair of Fiscal Years
#'
#' Given a pair of fiscal years, computes how many unique people appeared in
#' the earlier year, the later year, and both years.
#'
#' The `year_pair` label (e.g. "2020-2021") uses a non-breaking hyphen in HTML
#' output so tables don't wrap it onto two lines; for other output (e.g. PDF,
#' where the font may lack that character) it uses a plain hyphen.
#'
#' @param year_pair a length-2 vector giving the earlier and later fiscal year
#' @param df a data frame containing at least the columns `fis_year_` and `person_id`
#'
#' @return a one-row data frame with columns `year_pair`, `n_earlier`, `n_later`,
#'   `n_both`, and `pct_returning` (the percentage, 0-100, of people in the
#'   earlier year who also appear in the later year; unrounded)
#' @importFrom rlang .data
#' @export
summarize_year_pair <- function(year_pair, df) {
  id_earlier <- df %>%
    dplyr::filter(.data[["fis_year_"]] == year_pair[1]) %>%
    dplyr::pull(.data[["person_id"]]) %>%
    unique()

  id_later <- df %>%
    dplyr::filter(.data[["fis_year_"]] == year_pair[2]) %>%
    dplyr::pull(.data[["person_id"]]) %>%
    unique()

  df %>%
    dplyr::filter(.data[["fis_year_"]] %in% year_pair) %>%
    dplyr::select(.data[["person_id"]], .data[["fis_year_"]]) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[["person_id"]]) %>%
    dplyr::summarise(years_attended = dplyr::n_distinct(.data[["fis_year_"]]), .groups = "drop") %>%
    dplyr::summarise(
      n_earlier = sum(.data[["person_id"]] %in% id_earlier),
      n_later   = sum(.data[["person_id"]] %in% id_later),
      n_both    = sum(.data[["person_id"]] %in% id_earlier & .data[["person_id"]] %in% id_later),
      pct_returning = 100 * .data[["n_both"]] / .data[["n_earlier"]]
    ) %>%
    # U+2011 is a non-breaking hyphen
    dplyr::mutate(year_pair = paste0(year_pair[1], if (knitr::is_html_output()) "‑" else "-", year_pair[2])) %>%
    dplyr::select("year_pair", dplyr::everything())
}

#' Summarize the Percentage of Returning Clients per Year
#'
#' Creates a flextable summarizing, for each fiscal year, the number of unique
#' individuals, how many of them were repeat users (either they appear more
#' than once within that year, or they had already appeared in an earlier
#' year), and the resulting percentage of repeaters.
#'
#' @param df a data frame containing at least the columns `fis_year_` and `person_id`
#'
#' @return a flextable object. The underlying data (`$body$dataset`) keeps
#'   `pct_repeaters` numeric (0-100, rounded to 1 decimal) so it can be
#'   plotted; the "%" sign is added only in the displayed table.
#' @importFrom rlang .data
#' @export
summarize_returning_clients <- function(df) {
  df %>%
    dplyr::mutate(year_num = as.integer(as.character(.data[["fis_year_"]]))) %>%
    # per person-year counts
    dplyr::group_by(.data[["person_id"]], .data[["year_num"]]) %>%
    dplyr::summarize(n_in_year = dplyr::n(), .groups = "drop") %>%
    # compute first year each person appears
    dplyr::group_by(.data[["person_id"]]) %>%
    dplyr::mutate(first_year = min(.data[["year_num"]])) %>%
    dplyr::ungroup() %>%
    # mark if the person is a within-year repeater or has appeared previously
    dplyr::mutate(
      is_within_year_repeat = .data[["n_in_year"]] > 1,
      is_returner = .data[["first_year"]] < .data[["year_num"]],
      is_repeater = .data[["is_within_year_repeat"]] | .data[["is_returner"]]
    ) %>%
    # keep each person at most once per year, then count
    dplyr::filter(.data[["is_repeater"]]) %>%
    dplyr::distinct(.data[["person_id"]], .data[["year_num"]]) %>%
    dplyr::count(.data[["year_num"]], name = "n_repeaters") %>%
    # compute total unique people per year
    dplyr::left_join(
      df %>%
        dplyr::mutate(year_num = as.integer(as.character(.data[["fis_year_"]]))) %>%
        dplyr::distinct(.data[["person_id"]], .data[["year_num"]]) %>%
        dplyr::count(.data[["year_num"]], name = "total_in_year"),
      by = "year_num"
    ) %>%
    # percentage of repeaters
    dplyr::mutate(pct_repeaters = round(100 * .data[["n_repeaters"]] / .data[["total_in_year"]], 1)) %>%
    dplyr::select("year_num", "total_in_year", "n_repeaters", "pct_repeaters") %>%
    flextable::flextable() %>%
    flextable::colformat_num(col_keys = "year_num", big.mark = "", digits = 0) %>%
    flextable::colformat_double(j = "pct_repeaters", digits = 1, suffix = "%") %>%
    flextable::set_header_labels(
      year_num = "Fiscal Year",
      total_in_year = "Number of Unique Individuals",
      n_repeaters = "Number of Returning Individuals",
      pct_repeaters = "Percent of Individuals Who are Return Users"
    ) %>%
    flextable::autofit()
}

#' Keep Each Person's Most Recent Row
#'
#' Keeps one row per group (e.g. per person per fiscal year): the most recent,
#' ordered by `date_` and then by `tp_seq_` (the order of touch-points in the
#' combined data), when those columns are present. Used so that each person has
#' a single role and school in a year, their latest recorded one.
#'
#' @param df a data frame
#' @param by the grouping columns, as strings (e.g. `c("fis_year_", "person_id")`)
#'
#' @return `df` with one row per group
#' @export
latest_per_person <- function(df, by) {
  order_cols <- intersect(c("date_", "tp_seq_"), names(df))
  df %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(order_cols))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
}

#' Compute the Breakdown of a Categorical Column for a Given Fiscal Year
#'
#' For a given fiscal year, computes the count and percentage falling into each
#' level of `col`, counting either unique people or raw records.
#'
#' With `count = "people"` each person is counted once per year, with the
#' value of `col` from their most recent row that year (see
#' [latest_per_person()]), so someone whose role changed mid-year counts once,
#' in their latest role, and the percentages sum to 100%. With
#' `count = "records"` no de-duplication is done and every row is counted, with
#' its own value, so people who use a service repeatedly are weighted by how
#' often they used it.
#'
#' @param input_df a data frame containing at least the columns `fis_year_`,
#'   `person_id`, and `col`
#' @param year the fiscal year to filter to
#' @param col the name (as a string) of the categorical column to break down
#' @param count either "people" (default, count unique people per year) or
#'   "records" (count every row)
#'
#' @return a data frame with columns `fis_year_`, `col`, `n`, and `pct`
#' @importFrom rlang .data
#' @export
get_df_breakdown <- function(input_df, year, col, count = c("people", "records")) {
  count <- match.arg(count)

  if (count == "people") {
    # one row per person per year: their most recent value of `col`
    input_df <- latest_per_person(input_df, c("fis_year_", "person_id"))
  }

  d <- input_df %>%
    dplyr::select(.data[["fis_year_"]], .data[["person_id"]], dplyr::all_of(col))

  d %>%
    dplyr::group_by(.data[["fis_year_"]], !!rlang::sym(col)) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data[["fis_year_"]]) %>%
    dplyr::mutate(pct = .data[["n"]] / sum(.data[["n"]])) %>%
    # !! uses the `year` argument, even if the data has its own `year` column (e.g. BYOD)
    dplyr::filter(.data[["fis_year_"]] == !!year) %>%
    dplyr::ungroup()
}

#' Compare Unique-People and Per-Record Breakdowns Side by Side
#'
#' Runs [get_df_breakdown()] both ways for the same column and joins the
#' results, so you can see how many distinct people fall into each level, how
#' many records they account for, and the ratio between the two. The ratio is
#' a measure of usage intensity: how many times the average person in that
#' group used the service that year.
#'
#' @inheritParams get_df_breakdown
#'
#' @return a data frame with columns `col`, `n_people`, `pct_people`,
#'   `n_records`, `pct_records`, and `records_per_person`, sorted by
#'   `n_people` (descending)
#' @importFrom rlang .data
#' @export
get_df_breakdown_compare <- function(input_df, year, col) {
  people <- get_df_breakdown(input_df, year, col, count = "people") %>%
    dplyr::select(dplyr::all_of(col), n_people = "n", pct_people = "pct")

  records <- get_df_breakdown(input_df, year, col, count = "records") %>%
    dplyr::select(dplyr::all_of(col), n_records = "n", pct_records = "pct")

  people %>%
    dplyr::full_join(records, by = col) %>%
    dplyr::mutate(
      n_people = tidyr::replace_na(.data[["n_people"]], 0L),
      n_records = tidyr::replace_na(.data[["n_records"]], 0L),
      records_per_person = ifelse(
        .data[["n_people"]] > 0,
        round(.data[["n_records"]] / .data[["n_people"]], 2),
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data[["n_people"]]))
}

#' Compute a Ranked Breakdown Table for a Given Fiscal Year
#'
#' Wraps [get_df_breakdown()], sorting by percentage (descending), converting
#' the percentage to a whole number out of 100, and attaching the requested
#' `year` as its own column.
#'
#' @inheritParams get_df_breakdown
#'
#' @return a data frame with columns `col`, `fis_year_`, `n`, `pct`, and `year`
#' @importFrom rlang .data
#' @export
get_df_breakdown_tbl <- function(input_df, year, col, count = c("people", "records")) {
  count <- match.arg(count)
  get_df_breakdown(input_df, year, col, count = count) %>%
    dplyr::select(dplyr::all_of(c(col, "fis_year_", "n", "pct"))) %>%
    dplyr::arrange(dplyr::desc(.data[["pct"]])) %>%
    dplyr::mutate(
      pct = round(.data[["pct"]] * 100),
      year = year
    )
}

#' Build a Wide Flextable Comparing a Column's Breakdown Across Years
#'
#' For each year in `year_array`, computes the breakdown of `col` (via
#' [get_df_breakdown_tbl()]) and arranges the results side by side as a
#' flextable, with a merged two-row header showing "n" and "pct" for each
#' year.
#'
#' @inheritParams get_df_breakdown
#' @param year_array a vector of fiscal years to include as columns
#' @param show_percent_symbol if `TRUE` (the default), append "%" to the
#'   percentages
#'
#' @return a flextable object
#' @export
make_df_time_table <- function(input_df, year_array, col, count = c("people", "records"),
                               show_percent_symbol = TRUE) {
  count <- match.arg(count)
  foo <- purrr::map_dfr(
    year_array,
    ~ get_df_breakdown_tbl(input_df = input_df, year = .x, col = col, count = count)
  ) %>%
    dplyr::select(dplyr::all_of(col), "year", "n", "pct") %>%
    tidyr::pivot_wider(
      names_from = "year",
      values_from = c("n", "pct"),
      names_glue = "{year}_{.value}",
      values_fill = 0
    ) %>%
    dplyr::select(dplyr::all_of(col), dplyr::all_of(paste0(rep(year_array, each = 2), "_", c("n", "pct"))))

  if (show_percent_symbol) {
    foo <- foo %>% dplyr::mutate(dplyr::across(dplyr::ends_with("_pct"), ~ paste0(.x, "%")))
  }

  # Define the header structure
  header <- data.frame(
    col_keys = names(foo),
    line1 = c("", as.vector(rbind(gsub("^20", "FY", year_array), rep("", length(year_array))))),
    line2 = c(col, rep(c("n", "pct"), times = length(year_array))),
    stringsAsFactors = FALSE
  )

  foo %>%
    flextable::flextable() %>%
    flextable::set_header_df(mapping = header, key = "col_keys") %>%
    flextable::merge_h(part = "header") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(align = "left", j = 1, part = "all") %>%
    flextable::align(align = "center", j = 2:ncol(foo), part = "all") %>%
    flextable::vline(j = seq(1, ncol(foo) - 1, by = 2), part = "all") %>%
    flextable::hline_top(part = "all") %>%
    flextable::hline(i = 2, part = "header") %>%
    flextable::autofit()
}

#' Plot a Column's Breakdown Over Time
#'
#' Builds on [make_df_time_table()] and reshapes the result into a line plot
#' of percentage over fiscal year, one line per level of `col`.
#'
#' @inheritParams make_df_time_table
#' @param show_percent_symbol if `TRUE` (the default), append "%" to the
#'   percentage axis labels. Independent of the same argument in
#'   [make_df_time_table()].
#'
#' @return a ggplot object
#' @importFrom rlang .data
#' @export
make_df_time_plot <- function(input_df, year_array, col, count = c("people", "records"),
                              show_percent_symbol = TRUE) {
  count <- match.arg(count)
  # the plot needs numeric pct values, so never add "%" in the table here
  df_flex <- make_df_time_table(input_df, year_array, col, count = count, show_percent_symbol = FALSE)
  df_wide <- df_flex$body$dataset
  df_long <- df_wide %>%
    dplyr::select(-dplyr::ends_with("_n")) %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("_pct"),
      names_to = "year",
      names_pattern = "(\\d+)_pct",
      values_to = "pct"
    ) %>%
    dplyr::mutate(year = as.integer(.data[["year"]]))

  col_sym <- rlang::sym(col)
  # a different point shape for each group as well as a different color, so the lines can be told apart
  # without color (e.g., printed in black and white); enough shapes for up to 15 groups
  n_groups <- dplyr::n_distinct(df_long[[col]])
  shapes <- c(16, 17, 15, 18, 3, 4, 8, 1, 2, 0, 5, 6, 7, 9, 10)[seq_len(min(n_groups, 15))]
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data[["year"]], y = .data[["pct"]], group = !!col_sym,
                                             color = !!col_sym, shape = !!col_sym)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_shape_manual(values = shapes) +
    ggplot2::labs(x = "Fiscal Year", y = "Percentage", color = col, shape = col) +
    ggplot2::theme_minimal()

  # pct is already on a 0-100 scale (see get_df_breakdown_tbl())
  if (show_percent_symbol) {
    p <- p + ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))
  }
  p
}

#' Convert a Month to a Fiscal or Academic Quarter
#'
#' Not used in the FY26 report, which works with months instead; this is here
#' to reproduce earlier results that were grouped by quarter. Both versions
#' start the year in September, at the start of the fiscal year (see
#' [add_year_info()]).
#'
#' * `"fiscal"`: four 3-month quarters, `"Q1"` = Sept. - Nov., `"Q2"` = Dec. -
#'   Feb., `"Q3"` = Mar. - May, `"Q4"` = June - Aug.
#' * `"academic"`: `"Fall"` = Sept. - Dec., `"Winter"` = Jan. - Mar.,
#'   `"Spring"` = Apr. - June, `"Summer"` = July - Aug. These approximate
#'   Northwestern's academic quarters, so they are not all the same length
#'   (Fall is 4 months and Summer is 2).
#'
#' @param m a numeric vector of month numbers (1-12), e.g. from
#'   `lubridate::month(date)`
#' @param type `"fiscal"` (the default) or `"academic"`
#'
#' @return a factor of quarter names, with levels in fiscal-year order
#' @examples
#' month_to_quarter(c(9, 12, 3, 6))
#' month_to_quarter(c(9, 12, 3, 6), type = "academic")
#' @export
month_to_quarter <- function(m, type = c("fiscal", "academic")) {
  type <- match.arg(type)
  if (type == "fiscal") {
    # months since the start of the fiscal year (Sept. = 0), in blocks of 3
    q <- ((m - 9) %% 12) %/% 3 + 1
    factor(paste0("Q", q), levels = paste0("Q", 1:4))
  } else {
    factor(dplyr::case_when(
      m %in% 9:12 ~ "Fall",
      m %in% 1:3  ~ "Winter",
      m %in% 4:6  ~ "Spring",
      m %in% 7:8  ~ "Summer"
    ), levels = c("Fall", "Winter", "Spring", "Summer"))
  }
}

#' Render a Sankey Diagram, Falling Back to a Static Image for PDF Output
#'
#' Builds a [networkD3::sankeyNetwork()] widget from `links` and `nodes`. When
#' knitting to HTML the interactive widget is returned directly; when
#' knitting to PDF/LaTeX (where interactive widgets can't be embedded) the
#' widget is instead rendered to a temporary HTML file and captured as a PNG
#' via `webshot2`, which is returned via [knitr::include_graphics()].
#'
#' Requires the Suggested packages networkD3, htmlwidgets, webshot2, and
#' htmltools, which are not installed by default with this package since they
#' are only needed for this one function.
#'
#' @param links a data frame of Sankey links with columns `source`, `target`, `value`
#' @param nodes a data frame of Sankey nodes with a `name` column
#' @param file_prefix prefix used for the intermediate HTML/PNG file names
#' @param width,height size of the diagram, in pixels
#'
#' @return for HTML output, a `sankeyNetwork` htmlwidget; for PDF/LaTeX output,
#'   the result of [knitr::include_graphics()] pointing at the rendered PNG
#' @export
render_sankey <- function(links, nodes, file_prefix = "sankey", width = 400, height = 400) {
  require_pkgs <- function(pkgs) {
    missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_pkgs) > 0) {
      stop("render_sankey() requires the following package(s): ", paste(missing_pkgs, collapse = ", "))
    }
  }

  require_pkgs("networkD3")

  p <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    fontSize = 16,
    nodeWidth = 60,
    width = width,
    height = height
  )
  if (knitr::is_html_output()) {
    return(p)
  } else if (knitr::is_latex_output()) {
    require_pkgs(c("htmlwidgets", "webshot2", "htmltools"))
    p <- htmlwidgets::prependContent(p,
      htmltools::tags$style("text { font-family: sans-serif !important; }")
    )
    html_file <- paste0(file_prefix, "_tmp.html")
    png_file <- paste0(file_prefix, ".png")
    htmlwidgets::saveWidget(p, html_file, selfcontained = TRUE)
    webshot2::webshot(html_file, file = png_file, vwidth = max(800, width), vheight = max(800, height), zoom = 2, cliprect = "viewport")
    return(knitr::include_graphics(png_file))
  }
}

#' Fiscal Year Helpers
#'
#' Northwestern fiscal years run Sept. 1 - Aug. 31 and are named for the
#' calendar year they end in (FY26 = Sept. 1, 2025 - Aug. 31, 2026), as in
#' [add_year_info()]. `date_to_fy()` gives the fiscal year of a date;
#' `fy_first_day()` and `fy_last_day()` give the first and last day of a
#' fiscal year.
#'
#' @param d a vector of dates
#' @param fy a vector of fiscal years (e.g. `2026`)
#'
#' @return `date_to_fy()`: an integer vector of fiscal years;
#'   `fy_first_day()`, `fy_last_day()`: a vector of dates
#' @name fiscal_year_helpers
NULL

#' @rdname fiscal_year_helpers
#' @export
date_to_fy <- function(d) as.integer(lubridate::year(d) + (lubridate::month(d) >= 9))

#' @rdname fiscal_year_helpers
#' @export
fy_first_day <- function(fy) as.Date(paste0(fy - 1, "-09-01"))

#' @rdname fiscal_year_helpers
#' @export
fy_last_day <- function(fy) as.Date(paste0(fy, "-08-31"))

#' Get the Time of Day from a Date-Time Column
#'
#' Returns the Chicago time of day (`"HH:MM:SS"`) from a date-time column of a
#' data frame read from the metrics csv files (e.g. `start_datetime` in
#' `workshops.csv` or `created_datetime` in `consults.csv`).
#'
#' * Text columns: the csv files hold Chicago clock times, either as ISO 8601
#'   with the UTC offset (`"2026-09-30T14:22:34-05:00"`) or, in files made
#'   before Oct. 2026, without it (`"2026-09-30 14:22:34"`). Both have the time
#'   at the same place in the string, so read these columns as text (e.g.
#'   `col_types = readr::cols(start_datetime = "c")`) to use either kind of file.
#' * Date-time (`POSIXct`) columns are converted to Chicago time. This is right
#'   for the newer files (`readr::read_csv()` reads the offset), but not for the
#'   older files without an offset, which `read_csv()` reads as UTC.
#'
#' @param df a data frame
#' @param col name of the date-time column, as a string
#'
#' @return a character vector of times of day, or `NA` if `col` is not in `df`
#'   (e.g. in csv files made before the column was added)
#' @export
time_of_day <- function(df, col) {
  if (!col %in% names(df)) return(NA_character_)
  x <- df[[col]]
  if (inherits(x, "POSIXct")) format(x, "%H:%M:%S", tz = "America/Chicago") else stringr::str_sub(as.character(x), 12, 19)
}

#' Count Unique People by Role and School
#'
#' Counts unique people (`person_id`) in each role and school combination, in
#' one fiscal year or over all rows. Each person counts once, with their most
#' recent role and school (see [latest_per_person()]).
#'
#' @param df a data frame with `person_id`, `role`, `school`, and (if `year` is
#'   given) `fis_year_` columns
#' @param year a fiscal year, or `NULL` to use every row of `df`
#'
#' @return a data frame with `role`, `school`, and `n` (unique people)
#' @seealso [role_school_cell()], [plot_role_school_heatmap()]
#' @export
role_school_counts <- function(df, year = NULL) {
  # !! uses the `year` argument, even if the data has its own `year` column (e.g. BYOD)
  if (!is.null(year)) df <- df %>% dplyr::filter(.data[["fis_year_"]] == !!year)
  df %>%
    latest_per_person("person_id") %>%
    dplyr::count(.data[["role"]], .data[["school"]])
}

#' Get One Role and School Combination
#'
#' Unique people in one role and school combination in a fiscal year, and their
#' percentage of all unique people that year. Used for takeaways that describe
#' the largest role-and-school group, so it warns if the combination is not the
#' largest one.
#'
#' @inheritParams role_school_counts
#' @param role_name,school_name the role and school of the combination
#'
#' @return a one-row data frame with `n` (unique people) and `pct`
#' @seealso [role_school_counts()]
#' @export
role_school_cell <- function(df, year, role_name, school_name) {
  counts <- role_school_counts(df, year)
  n_cell <- sum(counts$n[counts$role == role_name & counts$school == school_name])
  if (n_cell < max(counts$n)) warning(school_name, " ", role_name, " is no longer the largest role x school group")
  n_total <- df %>% dplyr::filter(.data[["fis_year_"]] == !!year) %>% dplyr::distinct(.data[["person_id"]]) %>% nrow()
  dplyr::tibble(n = n_cell, pct = 100 * n_cell / n_total)
}

#' Plot a Heatmap of Unique People by Role and School
#'
#' Each cell is the number of unique people with that role and school, and
#' their percentage of all unique people in `df` (that year), e.g. "97 (19%)";
#' blank cells are 0. Counts come from [role_school_counts()]. Schools with the
#' most people are on the left, and roles are in their factor order from the top.
#'
#' @inheritParams role_school_counts
#' @param title plot title
#'
#' @return a ggplot object
#' @seealso [role_school_counts()]
#' @export
plot_role_school_heatmap <- function(df, year, title) {
  role <- school <- n <- NULL

  # unique people (the denominator for the percentages), in the same rows as the counts
  df_year <- if (is.null(year)) df else df %>% dplyr::filter(.data[["fis_year_"]] == !!year)
  n_total <- dplyr::n_distinct(df_year$person_id)
  pct_label <- function(x) paste0(x, " (", round(100 * x / n_total), "%)")

  role_school_counts(df, year) %>%
    # keep only the roles and schools that appear, then fill in the empty cells
    dplyr::mutate(dplyr::across(c(role, school), forcats::fct_drop)) %>%
    tidyr::complete(role, school, fill = list(n = 0L)) %>%
    dplyr::mutate(
      # schools with the most people on the left, roles in the usual order from the top
      school = forcats::fct_reorder(school, n, sum, .desc = TRUE),
      role = forcats::fct_rev(role)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = school, y = role, fill = n)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = dplyr::if_else(n > 0, pct_label(n), ""), color = n > max(n) / 2),
                       size = 2.7, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "white")) +
    ggplot2::scale_fill_gradient(low = "#eef3fa", high = "#08306b", labels = pct_label) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(fill = "Unique\nPeople (%)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), panel.grid = ggplot2::element_blank())
}

#' Count Cumulative Unique People by Fiscal Year
#'
#' For each fiscal year, the number of distinct people (`person_id`) with any
#' row in that year or earlier.
#'
#' @param df a data frame with `person_id` and `fis_year_` columns
#' @param years the fiscal years to count through
#'
#' @return a data frame with `fis_year_` and `cumulative_unique`
#' @export
cumulative_unique_people <- function(df, years) {
  df_years <- as.integer(as.character(df$fis_year_))
  dplyr::tibble(
    fis_year_ = years,
    cumulative_unique = purrr::map_int(years, ~ dplyr::n_distinct(df$person_id[df_years <= .x]))
  )
}

#' Make a Table of a Measure by Fiscal Year and Service
#'
#' One row per fiscal year and one column per service (in the factor order of
#' `service_`), with each cell given by `value`.
#'
#' @param df a data frame with `fis_year_` and `service_` columns, one row per
#'   fiscal year and service
#' @param value an expression, evaluated in `df`, giving each cell's text, e.g.
#'   `paste0(n_ai, " of ", n_total, " (", round(pct_ai), "%)")`
#'
#' @return a flextable object
#' @export
make_service_year_table <- function(df, value) {
  fis_year_ <- service_ <- NULL

  df %>%
    dplyr::arrange(service_) %>%
    dplyr::mutate(value = {{ value }}) %>%
    dplyr::select(fis_year_, service_, value) %>%
    tidyr::pivot_wider(names_from = service_, values_from = value, values_fill = "") %>%
    dplyr::arrange(fis_year_) %>%
    dplyr::mutate(fis_year_ = as.character(fis_year_)) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(fis_year_ = "Fiscal Year") %>%
    flextable::autofit() %>%
    myflextablefitter_if_pdf()
}

#' Plot a Percentage by Fiscal Year
#'
#' A line plot of a percentage over fiscal years (x-axis labeled "FY20",
#' "FY21", ...), optionally one line per group, and optionally with the
#' percentage written above each point. The y-axis starts at 0.
#'
#' @param df a data frame with a `fis_year_` column
#' @param pct the column holding the percentage (0 - 100)
#' @param years the fiscal years for the x-axis breaks
#' @param color optional column to draw one line per group (e.g. `service_`)
#' @param title,ylab plot title and y-axis label
#' @param show_labels whether to write the percentage above each point
#' @param y_max optional top of the y-axis (e.g. `100`)
#'
#' @return a ggplot object
#' @export
plot_pct_by_year <- function(df, pct, years, color = NULL, title = NULL, ylab = "Percent",
                             show_labels = TRUE, y_max = NA) {
  fis_year_ <- NULL
  grouped <- !rlang::quo_is_null(rlang::enquo(color))

  p <- df %>%
    dplyr::mutate(fis_year_ = as.integer(as.character(fis_year_))) %>%
    ggplot2::ggplot(ggplot2::aes(x = fis_year_, y = {{ pct }}, color = {{ color }})) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2)

  if (show_labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = paste0(round({{ pct }}), "%")),
                                vjust = -0.8, size = 3, show.legend = FALSE)
  }

  p <- p +
    ggplot2::scale_x_continuous(breaks = years, labels = paste0("FY", stringr::str_sub(years, 3))) +
    # leave room above the points for the labels
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
                                expand = ggplot2::expansion(mult = c(0.02, if (show_labels) 0.12 else 0.05))) +
    ggplot2::expand_limits(y = c(0, y_max)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("Fiscal Year") +
    ggplot2::ylab(ylab)

  if (grouped) {
    p <- p + ggplot2::labs(color = "") + ggplot2::theme(legend.position = "bottom")
  }
  p
}
