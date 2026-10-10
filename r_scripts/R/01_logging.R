# =============================================================================
# 01_logging.R
#
# A minimal logger. update_bib_from_orcid() opens a dated log file and every
# detail of the run (progress, tables of decisions, the BibTeX added) goes
# there, so the console only shows a short summary. When no log file is open
# (e.g. when a step function is called on its own), messages go to the console
# via message() instead.
# =============================================================================

# Path of the open log file (NULL when none is open)
LOG_STATE <- new.env()
LOG_STATE$path <- NULL

#' Open a log file; subsequent log_*() calls append to it
#'
#' @param path Path of the log file (created, or overwritten if it exists).
#' @return `path`, invisibly.
log_start <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  writeLines(character(), path)
  LOG_STATE$path <- path
  invisible(path)
}

#' Close the log file; later log_*() calls go to the console again
log_stop <- function() {
  LOG_STATE$path <- NULL
  invisible(NULL)
}

#' Write lines verbatim to the log (or the console if no log is open)
#'
#' @param lines Character vector.
log_lines <- function(lines) {
  lines <- enc2utf8(as.character(lines))
  if (is.null(LOG_STATE$path)) {
    message(paste(lines, collapse = "\n"))
  } else {
    cat(lines, file = LOG_STATE$path, sep = "\n", append = TRUE)
  }
  invisible(NULL)
}

#' Log a time-stamped message
#'
#' @param ... Pieces of the message, pasted together without separators.
log_msg <- function(...) {
  txt <- paste0(...)
  if (is.null(LOG_STATE$path)) message(txt)
  else log_lines(paste0(format(Sys.time(), "[%H:%M:%S] "), txt))
}

#' Log a section heading
#'
#' @param title Heading text.
log_section <- function(title) {
  log_lines(c("", paste0("=========== ", title, " ==========="), ""))
}

#' Log a data frame as a plain-text table
#'
#' @param df A data frame.
#' @param cols Columns to show (default: all).
log_table <- function(df, cols = names(df)) {
  if (is.null(df) || !nrow(df)) return(log_lines("None."))
  old <- options(width = 10000)
  on.exit(options(old))
  log_lines(utils::capture.output(print(df[, cols, drop = FALSE], row.names = FALSE)))
}
