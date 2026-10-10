# =============================================================================
# 04_orcid.R
#
# Step 1: download and flatten the list of works on a public ORCID record.
# =============================================================================

#' Build a User-Agent string for API requests
#'
#' @param mailto Optional contact e-mail (recommended by Crossref's "polite pool").
#' @return A character string.
make_user_agent <- function(mailto = NULL) {
  base <- "orcid-bib-updater/0.1 (R script)"
  if (is.null(mailto)) base else paste0(base, " (mailto:", mailto, ")")
}

#' Download the list of works from a public ORCID record
#'
#' Uses the public ORCID API (no authentication needed for public records).
#' ORCID has already grouped duplicate versions of the same work, so the result
#' contains one `group` per distinct work.
#'
#' @param orcid ORCID iD, e.g. `"0000-0003-1895-450X"`.
#' @param mailto Optional contact e-mail added to the User-Agent.
#' @return The parsed JSON response (nested list) with a top-level `group` element.
#' @examples
#' \dontrun{
#' works_json <- fetch_orcid_works("0000-0003-1895-450X")
#' length(works_json$group)
#' }
fetch_orcid_works <- function(orcid, mailto = NULL) {
  if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", orcid)) {
    stop("'", orcid, "' is not a valid ORCID iD (expected 0000-0000-0000-000X).",
         call. = FALSE)
  }
  resp <- httr2::request(sprintf("https://pub.orcid.org/v3.0/%s/works", orcid))
  resp <- httr2::req_headers(resp, Accept = "application/json")
  resp <- httr2::req_user_agent(resp, make_user_agent(mailto))
  resp <- httr2::req_retry(resp, max_tries = 3)       # retry transient failures
  resp <- httr2::req_perform(resp)                     # errors on HTTP >= 400
  httr2::resp_body_json(resp)
}

#' Flatten the ORCID works JSON into a data frame
#'
#' Produces one row per (work, DOI). A work with several DOIs (e.g. preprint +
#' published version) yields several rows sharing the same `group_id`; a work
#' with no DOI yields one row with `doi = NA`. DOIs flagged by ORCID as
#' `part-of` (the DOI of a containing book/proceedings, not of the work itself)
#' are ignored.
#'
#' @param works_json Output of [fetch_orcid_works()].
#' @return A data frame with columns `group_id`, `put_code`, `title`, `year`,
#'   `type`, `journal`, `doi`.
parse_orcid_works <- function(works_json) {
  groups <- works_json[["group"]] %||% list()

  rows <- lapply(seq_along(groups), function(i) {
    g  <- groups[[i]]
    ws <- safe_get(g, "work-summary", 1, default = list())  # preferred version

    # All DOIs attached to this work (group-level external ids)
    ext  <- safe_get(g, "external-ids", "external-id", default = list())
    dois <- unlist(lapply(ext, function(e) {
      is_doi  <- identical(tolower(scalar_chr(safe_get(e, "external-id-type"))), "doi")
      is_part <- identical(tolower(scalar_chr(safe_get(e, "external-id-relationship"))),
                           "part-of")
      if (is_doi && !is_part) scalar_chr(safe_get(e, "external-id-value")) else NULL
    }))
    dois <- unique(stats::na.omit(normalize_doi(dois)))
    if (length(dois) == 0L) dois <- NA_character_

    data.frame(
      group_id = i,
      put_code = suppressWarnings(as.integer(scalar_chr(safe_get(ws, "put-code")))),
      title    = scalar_chr(safe_get(ws, "title", "title", "value")),
      year     = suppressWarnings(as.integer(
                   scalar_chr(safe_get(ws, "publication-date", "year", "value")))),
      type     = scalar_chr(safe_get(ws, "type")),
      journal  = scalar_chr(safe_get(ws, "journal-title", "value")),
      doi      = dois,                       # scalar fields are recycled
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0L) {
    return(data.frame(group_id = integer(), put_code = integer(), title = character(),
                      year = integer(), type = character(), journal = character(),
                      doi = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}
