# get smartsheet token

get_ss_token <- function(env_file = NULL) {
  return(get_env_var("SMARTSHEET_TOKEN", env_file))
}

get_sheet_id <- function(name = NULL) {
  sheets <- c(
    "consults" = "6556636794906500",
    "workshops" = "564352335695748",
    "workshop_registrations" = "5611139475918724"
  )

  if (is.null(name)) {
    return(sheets)
  } else {
    if (name %in% names(sheets)) {
      return(sheets[name])
    }
  }
  return(NULL)
}

#' @export
import_smartsheet <- function(
  sheet_id = NULL, sheet_name = NULL,
  out_file = "tmp.xlsx", rm_outfile = FALSE, env_file = ".env",
  overwrite_existing_file = FALSE, return_dataframe = TRUE
) {

  if (is.null(sheet_id) && is.null(sheet_name)) {
    stop("specify sheet_id or sheet_name")
  } else if (is.null(sheet_id)) {
    sheet_id <- get_sheet_id(sheet_name)
    if (is.null(sheet_id)) {
      stop("sheet not found")
    }
  }

  # create excel file to save sheet to
  if (file.exists(out_file) && !overwrite_existing_file) {
    stop("out_file already exists")
  }

  ss_file <- file(out_file, open = "wb")

  if (rm_outfile) {
    on.exit({
      close_if_open(ss_file)
      unlink(out_file)
    })
  } else {
    on.exit({
      close_if_open(ss_file)
    })
  }

  # get smartsheet token
  ss_token <- get_ss_token(env_file)

  # api call to get smartsheet content
  root_url <- "https://api.smartsheet.com/2.0/sheets/"

  r <- httr::GET(
    paste0(root_url, sheet_id),
    httr::add_headers(
      Authorization = paste("Bearer", ss_token),
      Accept="application/vnd.ms-excel"
    )
  )

  # write smartsheet content to file
  writeBin(r$content, ss_file)
  close(ss_file)

  if (return_dataframe) {
    return(readxl::read_excel(out_file))
  } else {
    invisible()
  }
}
