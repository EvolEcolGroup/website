# =============================================================================
# 09_missing_from_orcid.R
#
# Reverse direction (optional, not used by update_bib_from_orcid()): bib
# entries that are NOT on ORCID, e.g. to upload them there.
# =============================================================================

#' Identify bib entries that do not appear on an ORCID record
#'
#' The mirror image of [classify_orcid_works()]: for every entry in the .bib
#' file, decide whether it is already on ORCID. An entry counts as present when
#'
#' \enumerate{
#'   \item its DOI equals one of the DOIs of an ORCID work (any version in a
#'     group), or
#'   \item (no DOI match) its title is at least `title_threshold` similar to the
#'     title of an ORCID work. This covers entries without a DOI and entries
#'     whose DOI differs from the one on ORCID (e.g. preprint vs published).
#' }
#'
#' Everything else is reported as `missing_from_orcid`.
#'
#' @param bib Output of [read_bib_entries()].
#' @param orcid_works Output of [parse_orcid_works()].
#' @param title_threshold Similarity (0-1) at or above which titles count as the
#'   same work. Raise it (e.g. 0.95) for fewer title matches, i.e. more entries
#'   flagged as missing.
#' @return `bib` (without the `raw` column) plus `status` (`"on_orcid"` or
#'   `"missing_from_orcid"`), `match_type` (`"doi"`, `"title"` or `NA`),
#'   `orcid_title` (the matching ORCID title, if any) and `title_similarity`.
#' @examples
#' \dontrun{
#' bib   <- read_bib_entries("andrea_manica_pubs.bib")
#' works <- parse_orcid_works(fetch_orcid_works("0000-0003-1895-450X"))
#' table(find_bib_missing_from_orcid(bib, works)$status)
#' }
find_bib_missing_from_orcid <- function(bib, orcid_works, title_threshold = 0.9) {
  n <- nrow(bib)

  # (a) DOI match: is the entry's DOI among any DOI listed on ORCID?
  doi_hit <- !is.na(bib$doi) & bib$doi %in% stats::na.omit(orcid_works$doi)

  # ORCID titles are compared once per work (not once per DOI row)
  works_u     <- orcid_works[!duplicated(orcid_works$group_id), ]
  orcid_tnorm <- normalize_title(works_u$title)

  # (b) Title match, only for entries not already matched by DOI
  sim <- rep(NA_real_, n)
  orcid_title <- rep(NA_character_, n)
  for (i in which(!doi_hit)) {
    m <- best_title_match(bib$title_norm[i], orcid_tnorm, works_u$title)
    sim[i]         <- m$similarity
    orcid_title[i] <- m$key          # best_title_match returns the label we passed
  }
  title_hit <- !doi_hit & !is.na(sim) & sim >= title_threshold

  # For DOI matches, report the ORCID title too
  orcid_title[doi_hit] <- orcid_works$title[match(bib$doi[doi_hit], orcid_works$doi)]

  out <- bib[, setdiff(names(bib), "raw"), drop = FALSE]
  out$status <- ifelse(doi_hit | title_hit, "on_orcid", "missing_from_orcid")
  out$match_type <- ifelse(doi_hit, "doi", ifelse(title_hit, "title", NA_character_))
  # Keep the best-candidate title only when it is a real match
  out$orcid_title <- ifelse(doi_hit | title_hit, orcid_title, NA_character_)
  out$title_similarity <- round(ifelse(doi_hit, 1, sim), 3)
  out
}

#' Subset a .bib file to the entries missing from ORCID
#'
#' End-to-end helper: reads the .bib file, downloads the ORCID works (unless you
#' supply them), and returns the original text of every entry that is not on
#' ORCID, ready to be written to a new .bib file for upload. The original .bib
#' file is not modified.
#'
#' @param bib_path Path to the existing .bib file.
#' @param orcid ORCID iD. Ignored if `orcid_works` is supplied.
#' @param orcid_works Optional output of [parse_orcid_works()], to avoid a second
#'   download if you already have it (e.g. `parse_orcid_works(fetch_orcid_works(id))`).
#'   Do not pre-filter it by type, or entries of that type will look missing.
#' @param mailto Optional contact e-mail for the ORCID request.
#' @param title_threshold Title similarity (0-1) counted as "same work"; see
#'   [find_bib_missing_from_orcid()].
#' @return A list with:
#' \describe{
#'   \item{bibtex}{Character vector with the original text of each missing entry.}
#'   \item{missing}{Data frame describing the missing entries (`key`, `type`,
#'     `year`, `title`, `doi`).}
#'   \item{title_matched}{Entries treated as present only because of a title
#'     match (no DOI match); worth a quick look as they are the riskiest calls.}
#'   \item{report}{Every bib entry with its status; see
#'     [find_bib_missing_from_orcid()].}
#' }
#' @examples
#' \dontrun{
#' res <- subset_bib_missing_from_orcid("andrea_manica_pubs.bib", "0000-0003-1895-450X")
#' write_bib_subset(res$bibtex, "missing_from_orcid.bib")
#' }
subset_bib_missing_from_orcid <- function(bib_path, orcid = NULL, orcid_works = NULL,
                                          mailto = NULL, title_threshold = 0.9) {
  bib <- read_bib_entries(bib_path)
  log_msg("Read ", nrow(bib), " entries from ", bib_path, ".")

  if (is.null(orcid_works)) {
    if (is.null(orcid)) stop("Supply either 'orcid' or 'orcid_works'.", call. = FALSE)
    orcid_works <- parse_orcid_works(fetch_orcid_works(orcid, mailto))
  }
  log_msg("ORCID lists ", length(unique(orcid_works$group_id)), " works.")

  report  <- find_bib_missing_from_orcid(bib, orcid_works, title_threshold)
  missing <- report$status == "missing_from_orcid"

  log_msg(sum(!missing), " bib entries found on ORCID (",
      sum(report$match_type %in% "doi"), " by DOI, ",
      sum(report$match_type %in% "title"), " by title); ",
      sum(missing), " missing.")

  cols <- c("key", "type", "year", "title", "doi")
  list(
    bibtex        = bib$raw[missing],
    missing       = report[missing, cols, drop = FALSE],
    title_matched = report[report$match_type %in% "title",
                           c("key", "title", "orcid_title", "title_similarity")],
    report        = report
  )
}

