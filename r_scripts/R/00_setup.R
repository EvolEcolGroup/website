# =============================================================================
# 00_setup.R
#
# Package checks and small general-purpose helpers used by every other file.
# =============================================================================

# Fail early with a clear message if a dependency is missing.
for (pkg in c("httr2", "stringi")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required. Install it with install.packages(\"",
         pkg, "\")", call. = FALSE)
  }
}

#' Null-coalescing operator
#'
#' Returns `a` unless it is `NULL` (or zero-length), in which case `b`.
#'
#' @param a,b Any R objects.
#' @return `a` if it has length > 0, otherwise `b`.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' Safely extract a nested element from a list
#'
#' JSON returned by the APIs is full of optional fields. This walks down a
#' path of names / indices and returns `default` instead of erroring when any
#' step is missing, `NULL`, or out of range.
#'
#' @param x A (nested) list.
#' @param ... Path of names (character) and/or positions (numeric) to follow.
#' @param default Value returned when the path does not exist.
#' @return The element found at the end of the path, or `default`.
#' @examples
#' x <- list(a = list(b = list(c = 5)))
#' safe_get(x, "a", "b", "c")             # 5
#' safe_get(x, "a", "zzz", "c")           # NA
safe_get <- function(x, ..., default = NA) {
  for (p in list(...)) {
    if (is.null(x) || !is.list(x)) return(default)
    if (is.numeric(p) && p > length(x)) return(default)
    x <- x[[p]]
  }
  if (is.null(x)) default else x
}

#' Coerce an arbitrary value to a single character string (or NA)
#'
#' @param x Anything; typically a JSON scalar that may be `NULL`.
#' @return A length-1 character vector, `NA_character_` if `x` is empty.
scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  as.character(x[[1]])
}
