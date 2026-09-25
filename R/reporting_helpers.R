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
#' * "Communication", "Bienen", "Medill", and "SESP" are combined into
#'   "Comm/Bien/Medi/SESP"
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
      role = forcats::fct_recode(.data[["role"]], "PhD Student" = "Graduate Student"),
      role = factor(.data[["role"]], levels = role_order)
    ) %>%
    dplyr::mutate(
      school = as.character(.data[["school"]]),
      school = ifelse(is.na(.data[["school"]]) | stringr::str_trim(.data[["school"]]) == "", "Other", .data[["school"]]),
      school = factor(.data[["school"]]),
      school = forcats::fct_collapse(
        .data[["school"]],
        `Medical Affiliates` = c("NW Medicine", "Lurie Childrens", "SRA Lab"),
        `Comm/Bien/Medi/SESP` = c("Communication", "Bienen", "Medill", "SESP")
      )
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

#' Summarize the Fraction of Returning Clients per Year
#'
#' Creates a flextable summarizing, for each fiscal year, the number of unique
#' individuals, how many of them were repeat users (either they appear more
#' than once within that year, or they had already appeared in an earlier
#' year), and the resulting fraction of repeaters.
#'
#' @param df a data frame containing at least the columns `fis_year_` and `person_id`
#'
#' @return a flextable object
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
    # fraction of repeaters
    dplyr::mutate(frac_repeaters = round(.data[["n_repeaters"]] / .data[["total_in_year"]], 3)) %>%
    dplyr::select("year_num", "total_in_year", "n_repeaters", "frac_repeaters") %>%
    flextable::flextable() %>%
    flextable::colformat_num(col_keys = "year_num", big.mark = "", digits = 0) %>%
    flextable::set_header_labels(
      year_num = "Fiscal Year",
      total_in_year = "Number of Unique Individuals",
      n_repeaters = "Number of Returning Individuals",
      frac_repeaters = "Fraction of Individuals Who are Return Users"
    ) %>%
    flextable::autofit()
}

#' Compute the Breakdown of a Categorical Column for a Given Fiscal Year
#'
#' For a given fiscal year, computes the count and percentage falling into each
#' level of `col`, counting either unique people or raw records.
#'
#' With `count = "people"` the data are first reduced to distinct
#' `(fis_year_, person_id, col)` rows, so each person is counted once per year.
#' Note that `col` has to stay in that key -- it is the column being broken
#' down, so each person needs a value for it -- which means anyone with two
#' different recorded values in the same year (e.g. a role change mid-year)
#' contributes to both levels. With `count = "records"` no de-duplication is
#' done and every row is counted, so people who use a service repeatedly are
#' weighted by how often they used it.
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

  d <- input_df %>%
    dplyr::select(.data[["fis_year_"]], .data[["person_id"]], dplyr::all_of(col))

  if (count == "people") {
    d <- dplyr::distinct(d)
  }

  d %>%
    dplyr::group_by(.data[["fis_year_"]], !!rlang::sym(col)) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data[["fis_year_"]]) %>%
    dplyr::mutate(pct = .data[["n"]] / sum(.data[["n"]])) %>%
    dplyr::filter(.data[["fis_year_"]] == year) %>%
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
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data[["year"]], y = .data[["pct"]], group = !!col_sym, color = !!col_sym)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = "Fiscal Year", y = "Percentage", color = col) +
    ggplot2::theme_minimal()

  # pct is already on a 0-100 scale (see get_df_breakdown_tbl())
  if (show_percent_symbol) {
    p <- p + ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))
  }
  p
}

#' Categorize a Month Number into an Academic Quarter
#'
#' Maps a numeric month (1-12) to the academic quarter used throughout this
#' package's fiscal-year conventions (the fiscal/academic year starts in
#' September; see [add_year_info()]).
#'
#' @param m a numeric vector of month numbers (1-12)
#'
#' @return a character vector of quarter names: "Fall", "Winter", "Spring", or "Summer"
#' @export
quarter_from_month <- function(m) {
  dplyr::case_when(
    m %in% 9:12 ~ "Fall",
    m %in% 1:3  ~ "Winter",
    m %in% 4:6  ~ "Spring",
    m %in% 7:8  ~ "Summer"
  )
}

#' Combine Workshop and Consult Record Counts by Academic Quarter
#'
#' Tags each workshop record (by its `start_date`) and each consult record (by
#' its `created` date) with an academic quarter via [quarter_from_month()],
#' then counts records per service and quarter -- useful for comparing team
#' "load" across the year.
#'
#' @param ws_df a workshop data frame containing a `start_date` column
#' @param consult_df a consult data frame containing a `created` column
#'
#' @return a data frame with columns `service_`, `quarter_string`, and `n`
#' @importFrom rlang .data
#' @export
get_quarterly_load <- function(ws_df, consult_df) {
  dplyr::bind_rows(
    ws_df %>%
      dplyr::mutate(
        quarter_string = quarter_from_month(lubridate::month(.data[["start_date"]])),
        service_ = "Workshop"
      ),
    consult_df %>%
      dplyr::mutate(
        quarter_string = quarter_from_month(lubridate::month(.data[["created"]])),
        service_ = "Consult"
      )
  ) %>%
    dplyr::mutate(quarter_string = factor(.data[["quarter_string"]], levels = c("Fall", "Winter", "Spring", "Summer"))) %>%
    dplyr::count(.data[["service_"]], .data[["quarter_string"]], name = "n")
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
#'
#' @return for HTML output, a `sankeyNetwork` htmlwidget; for PDF/LaTeX output,
#'   the result of [knitr::include_graphics()] pointing at the rendered PNG
#' @export
render_sankey <- function(links, nodes, file_prefix = "sankey") {
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
    width = 400,
    height = 400
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
    webshot2::webshot(html_file, file = png_file, vwidth = 800, vheight = 800, zoom = 2, cliprect = "viewport")
    return(knitr::include_graphics(png_file))
  }
}
