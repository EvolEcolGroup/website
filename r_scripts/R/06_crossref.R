# =============================================================================
# 06_crossref.R
#
# Crossref API: fetch a record by DOI (with per-run caching), search by title,
# and small accessors for Crossref records (year, type label).
# =============================================================================

# DOIs for which Crossref answered 404 during this session (e.g. DataCite DOIs
# from Zenodo or Dryad). Unlike network errors, this is a permanent answer that
# can be cached in the .bib (see find_preprint_entries()).
CROSSREF_NOT_FOUND <- new.env()

#' Fetch the full Crossref metadata record for a DOI
#'
#' Requests are throttled (3 per second) and transient errors (HTTP 429/5xx) are
#' retried. A DOI that Crossref does not know (HTTP 404) returns `NULL`: this
#' happens for DOIs registered elsewhere, e.g. DataCite (Zenodo, Dryad, ...).
#'
#' @param doi A normalised DOI.
#' @param mailto Optional contact e-mail; gives access to Crossref's faster,
#'   more reliable "polite pool".
#' @return The `message` element of the Crossref response (a nested list), or
#'   `NULL` if the record could not be retrieved.
#' @examples
#' \dontrun{
#' msg <- fetch_crossref_work("10.1038/s41586-020-2649-2")
#' msg$title[[1]]
#' }
fetch_crossref_work <- function(doi, mailto = NULL) {
  url <- paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE))

  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, make_user_agent(mailto))
  if (!is.null(mailto)) req <- httr2::req_url_query(req, mailto = mailto)
  req <- httr2::req_throttle(req, rate = 3)
  req <- httr2::req_retry(req, max_tries = 3,
                          is_transient = function(r) httr2::resp_status(r) %in%
                            c(429L, 500L, 502L, 503L, 504L))
  req <- httr2::req_error(req, is_error = function(r) FALSE)   # we inspect status ourselves

  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    warning("Crossref request failed for ", doi, ": ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(resp)) return(NULL)

  status <- httr2::resp_status(resp)
  if (status == 404L) {                                   # not a Crossref DOI
    assign(doi, TRUE, envir = CROSSREF_NOT_FOUND)
    return(NULL)
  }
  if (status >= 400L) {
    warning("Crossref returned HTTP ", status, " for ", doi, call. = FALSE)
    return(NULL)
  }
  httr2::resp_body_json(resp)[["message"]]
}

#' Look for a work in Crossref by its title (for ORCID works without a DOI)
#'
#' Runs a bibliographic query and accepts the best hit only if its title is very
#' similar to the requested one, the publication year is compatible (+/- 1, or
#' not before `min_year`) and, if `author_family` is given, at least one of
#' those surnames appears among the authors. Corrections/errata are never
#' accepted. These checks guard against false positives. Also used to find the
#' published version of a preprint (see [find_published_version()]).
#'
#' @param title Title of the work as listed on ORCID.
#' @param year Optional publication year (integer) used as a sanity check.
#' @param author_family Optional surname(s); at least one must appear in the
#'   author list.
#' @param mailto Optional contact e-mail.
#' @param min_similarity Minimum title similarity (0-1) to accept a hit.
#' @param exclude_types Crossref types to skip (e.g. `"posted-content"` to
#'   ignore preprints).
#' @param min_year Optional earliest acceptable publication year.
#' @return A Crossref `message` item (nested list) or `NULL` if nothing acceptable.
search_crossref_by_title <- function(title, year = NA, author_family = NULL,
                                     mailto = NULL, min_similarity = 0.95,
                                     exclude_types = NULL, min_year = NA) {
  if (is.na(title) || !nzchar(title)) return(NULL)

  req <- httr2::request("https://api.crossref.org/works")
  req <- httr2::req_user_agent(req, make_user_agent(mailto))
  req <- httr2::req_url_query(req, query.bibliographic = title, rows = 5)
  if (!is.null(mailto)) req <- httr2::req_url_query(req, mailto = mailto)
  req <- httr2::req_throttle(req, rate = 3)
  req <- httr2::req_retry(req, max_tries = 3)
  req <- httr2::req_error(req, is_error = function(r) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) return(NULL)

  items  <- httr2::resp_body_json(resp)[["message"]][["items"]] %||% list()
  target <- normalize_title(title)
  # Corrections, errata etc. share the original title; never accept them
  # unless the title we are looking for is itself one.
  notice_re <- "^(correction|erratum|corrigendum|retraction|reply|response|comment) "
  wanted_families <- tolower(stringi::stri_trans_general(author_family %||% character(),
                                                         "Latin-ASCII"))

  for (it in items) {
    cand_type <- scalar_chr(it[["type"]])
    if (!is.null(exclude_types) && !is.na(cand_type) && cand_type %in% exclude_types) next

    cand_norm <- normalize_title(scalar_chr(safe_get(it, "title", 1)))
    if (grepl(notice_re, cand_norm) && !grepl(notice_re, target)) next
    sim <- best_title_match(cand_norm, target, "x")$similarity
    if (is.na(sim) || sim < min_similarity) next

    # Year sanity checks
    cand_year <- extract_year(it)
    if (!is.na(year) && !is.na(cand_year) && abs(cand_year - year) > 1L) next
    if (!is.na(min_year) && !is.na(cand_year) && cand_year < min_year) next

    # Author sanity check: at least one surname in common
    if (length(wanted_families)) {
      fams <- vapply(it[["author"]] %||% list(),
                     function(a) scalar_chr(a[["family"]]), character(1))
      fams <- tolower(stringi::stri_trans_general(fams[!is.na(fams)], "Latin-ASCII"))
      if (!any(fams %in% wanted_families)) next
    }
    return(it)
  }
  NULL
}

#' Extract the publication year from a Crossref record
#'
#' Prefers the print date (the year normally cited), then the generic `issued`
#' date, then the online date.
#'
#' @param msg Crossref `message` (nested list).
#' @return Integer year or `NA_integer_`.
extract_year <- function(msg) {
  for (f in c("published-print", "issued", "published-online", "published")) {
    y <- safe_get(msg, f, "date-parts", 1, 1)
    if (!is.null(y) && !is.na(y)) return(as.integer(y))
  }
  NA_integer_
}

#' Crossref type of a record, as stored in the `orcidsync-type` field
#'
#' The type, followed by `:subtype` when Crossref gives one
#' (e.g. `"journal-article"`, `"posted-content:preprint"`).
#'
#' @param msg Crossref `message` (nested list) or `NULL`.
#' @return Character scalar, or `NA_character_`.
crossref_type_label <- function(msg) {
  if (is.null(msg)) return(NA_character_)
  type    <- scalar_chr(msg[["type"]])
  subtype <- scalar_chr(msg[["subtype"]])
  if (is.na(type)) NA_character_ else if (is.na(subtype)) type else paste0(type, ":", subtype)
}

#' Fetch a Crossref record, re-using earlier results
#'
#' A thin caching wrapper around [fetch_crossref_work()], so each DOI is
#' requested at most once per run (results, including "not found", are stored
#' in `cache`).
#'
#' @param doi Normalised DOI.
#' @param mailto Optional e-mail for Crossref.
#' @param cache An environment created with `new.env()`, or `NULL` for no caching.
#' @return As [fetch_crossref_work()].
crossref_cached <- function(doi, mailto = NULL, cache = NULL) {
  if (is.null(cache)) return(fetch_crossref_work(doi, mailto))
  if (exists(doi, envir = cache, inherits = FALSE)) return(get(doi, envir = cache))
  msg <- fetch_crossref_work(doi, mailto)
  assign(doi, msg, envir = cache)
  msg
}
