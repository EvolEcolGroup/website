# =============================================================================
# 08_new_records.R
#
# Step 3: ORCID works missing from the .bib file. Full metadata is retrieved
# from Crossref, BibTeX entries are formatted, and they are added to the bib.
# =============================================================================

#' Find ORCID works missing from a .bib file and build BibTeX records for them
#'
#' Reads the .bib file, downloads the ORCID works list (unless supplied),
#' identifies works not yet in the file (by DOI, then by title), retrieves full
#' metadata from Crossref and formats BibTeX entries. Nothing is written to the
#' .bib file: use [add_new_records()] for that.
#'
#' Works that need your attention are returned separately rather than silently
#' dropped (and are never added automatically): `possible_duplicates` (probably already in the .bib file under another
#' DOI or title wording) and `unresolved` (could not be retrieved from Crossref,
#' e.g. Zenodo/Dryad datasets, or works with no DOI).
#'
#' @param bib_path Path to the existing .bib file.
#' @param orcid ORCID iD.
#' @param mailto Optional e-mail for the Crossref polite pool (recommended).
#' @param title_threshold Title similarity (0-1) above which a work is considered
#'   already present in the .bib file. Lower it to be more aggressive in flagging
#'   duplicates.
#' @param author_style `"initials"` (match existing file) or `"full"`.
#' @param container_field `"journal"` (match existing file) or `"standard"`.
#' @param resolve_no_doi If `TRUE`, try to find ORCID works that lack a DOI in
#'   Crossref by title search (high-confidence matches only).
#' @param author_family Optional surname required in author lists of title-search
#'   hits (e.g. `"Manica"`); makes the title search safer.
#' @param exclude_types ORCID work types to ignore, e.g. `c("data-set", "other")`.
#' @param orcid_works Optional output of [parse_orcid_works()], to avoid
#'   downloading the ORCID record again.
#' @return A list with:
#' \describe{
#'   \item{bibtex}{Character vector, one new BibTeX entry per element.}
#'   \item{records}{Data frame summarising the new records (`key`, `type`, `year`,
#'     `title`, `doi`), in the same order as `bibtex`.}
#'   \item{possible_duplicates}{ORCID works probably already in the .bib file,
#'     with the matching key and title similarity.}
#'   \item{unresolved}{New ORCID works for which no Crossref record was obtained,
#'     with the reason.}
#'   \item{orcid_report}{Every ORCID work with its classification status.}
#'   \item{orcid_works}{The parsed ORCID works (see [parse_orcid_works()]).}
#' }
#' @examples
#' \dontrun{
#' res <- find_new_records("andrea_manica_pubs.bib", "0000-0003-1895-450X",
#'                         mailto = "you@university.ac.uk")
#' cat(res$bibtex, sep = "\n\n")
#' }
find_new_records <- function(bib_path, orcid, mailto = NULL,
                             title_threshold = 0.9,
                             author_style = "initials",
                             container_field = "journal",
                             resolve_no_doi = TRUE,
                             author_family = NULL,
                             exclude_types = character(0),
                             orcid_works = NULL) {

  # ---- 1. Existing bib ------------------------------------------------------
  bib <- read_bib_entries(bib_path)
  log_msg("Read ", nrow(bib), " entries from ", bib_path,
      " (", sum(!is.na(bib$doi)), " with a DOI).")

  # ---- 2. ORCID works -------------------------------------------------------
  works <- orcid_works %||% parse_orcid_works(fetch_orcid_works(orcid, mailto))
  log_msg("ORCID lists ", length(unique(works$group_id)), " works (",
      sum(!is.na(works$doi)), " DOIs).")
  if (length(exclude_types)) works <- works[!(works$type %in% exclude_types), ]

  # ---- 3. Compare with bib --------------------------------------------------
  report <- classify_orcid_works(works, bib, title_threshold)
  log_msg("Status: ", paste(names(table(report$status)), table(report$status),
                               sep = " = ", collapse = ", "))

  unresolved <- data.frame()
  add_unresolved <- function(rows, reason) {
    if (nrow(rows) == 0L) return()
    unresolved <<- rbind(unresolved,
                         cbind(rows[, c("group_id", "title", "year", "type", "doi")],
                               reason = reason, stringsAsFactors = FALSE))
  }

  # ---- 4. Crossref metadata for new works -----------------------------------
  # 4a. works with a DOI
  todo_doi <- report[report$status == "new", ]
  log_msg("Querying Crossref for ", nrow(todo_doi), " new DOI(s)...")
  hits <- list()                      # each: list(group_id, msg)
  for (i in seq_len(nrow(todo_doi))) {
    msg <- fetch_crossref_work(todo_doi$doi[i], mailto)
    if (is.null(msg)) {
      add_unresolved(todo_doi[i, ], "DOI not found in Crossref (maybe DataCite/other agency)")
    } else {
      hits[[length(hits) + 1L]] <- list(group_id = todo_doi$group_id[i], msg = msg)
    }
  }

  # 4b. works without a DOI: try a title search
  todo_nodoi <- report[report$status == "new_no_doi", ]
  for (i in seq_len(nrow(todo_nodoi))) {
    msg <- if (resolve_no_doi) {
      search_crossref_by_title(todo_nodoi$title[i], todo_nodoi$year[i],
                               author_family, mailto)
    } else NULL
    if (is.null(msg)) {
      add_unresolved(todo_nodoi[i, ], "No DOI on ORCID and no confident Crossref match")
    } else {
      hits[[length(hits) + 1L]] <- list(group_id = todo_nodoi$group_id[i], msg = msg)
    }
  }

  # ---- 5. Convert to records ------------------------------------------------
  recs <- lapply(hits, function(h) {
    r <- crossref_to_record(h$msg, author_style, container_field)
    r$group_id <- h$group_id
    r
  })

  # A work with several new DOIs (preprint + article): keep one per ORCID group,
  # preferring the published version over the preprint ("posted-content").
  if (length(recs)) {
    gid     <- vapply(recs, function(r) r$group_id, numeric(1))
    is_prep <- vapply(recs, function(r) identical(r$crossref_type, "posted-content"),
                      logical(1))
    keep <- !duplicated(gid[order(gid, is_prep)])
    ord  <- order(gid, is_prep)
    recs <- recs[ord][keep]
  }

  # Drop repeated DOIs, and anything whose Crossref title matches the bib
  # (the ORCID title may have differed from the Crossref one).
  dup_rows <- data.frame()
  kept <- list()
  for (r in recs) {
    if (!is.na(r$doi) && (r$doi %in% bib$doi || r$doi %in% vapply(kept, function(k) k$doi, ""))) next
    m <- best_title_match(normalize_title(r$title), bib$title_norm, bib$key)
    if (!is.na(m$similarity) && m$similarity >= title_threshold) {
      dup_rows <- rbind(dup_rows, data.frame(
        group_id = r$group_id, title = r$title, year = r$year, type = r$crossref_type,
        doi = r$doi, status = "possible_duplicate", matched_key = m$key,
        title_similarity = round(m$similarity, 3), stringsAsFactors = FALSE))
      next
    }
    kept[[length(kept) + 1L]] <- r
  }

  # Put new records in chronological order (like the existing file)
  if (length(kept)) {
    yrs  <- vapply(kept, function(r) if (is.na(r$year)) 9999L else r$year, integer(1))
    kept <- kept[order(yrs)]
  }

  # ---- 6. Keys and BibTeX text ----------------------------------------------
  all_keys <- bib$key
  entries  <- character()
  summary_rows <- list()
  for (r in kept) {
    key      <- make_cite_key(r$first_family, r$year, r$title, all_keys)
    all_keys <- c(all_keys, key)
    entries  <- c(entries, format_bibtex_entry(key, r$bibtype, r$fields))
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      key = key, type = r$bibtype, year = r$year, title = r$title, doi = r$doi,
      stringsAsFactors = FALSE)
  }
  records <- if (length(summary_rows)) do.call(rbind, summary_rows) else
    data.frame(key = character(), type = character(), year = integer(),
               title = character(), doi = character())

  # Possible duplicates found at ORCID-comparison time + those found post-Crossref
  pd_cols <- c("group_id", "title", "year", "type", "doi", "status",
               "matched_key", "title_similarity")
  possible_dups <- rbind(report[report$status == "possible_duplicate", pd_cols], dup_rows)

  log_msg("New records: ", nrow(records),
      " | possible duplicates: ", nrow(possible_dups),
      " | unresolved: ", nrow(unresolved))

  list(bibtex = entries, records = records, possible_duplicates = possible_dups,
       unresolved = unresolved, orcid_report = report, orcid_works = works)
}

#' Add new BibTeX entries to a .bib file
#'
#' Inserts the entries in chronological position (see [insert_bib_entries()]),
#' then re-reads the file to check that exactly `length(entries)` entries were
#' added. Citation keys are not checked here: [find_new_records()] already
#' makes them unique.
#'
#' @param bib_path Path to the .bib file (modified in place).
#' @param entries Character vector of BibTeX entries, e.g. `find_new_records()$bibtex`.
#' @param years Integer vector of their years, e.g. `find_new_records()$records$year`.
#' @param backup If `TRUE`, a time-stamped backup is made first (see
#'   [backup_bib_file()]) and restored if the check fails. Set to `FALSE` when
#'   the caller has made a backup itself.
#' @param backup_dir Folder for the backup copy.
#' @return Path of the backup (`NA` if none was made), invisibly.
#' @examples
#' \dontrun{
#' res <- find_new_records("andrea_manica_pubs.bib", "0000-0003-1895-450X")
#' add_new_records("andrea_manica_pubs.bib", res$bibtex, res$records$year)
#' }
add_new_records <- function(bib_path, entries, years = rep(NA_integer_, length(entries)),
                            backup = TRUE, backup_dir = dirname(bib_path)) {
  bk <- NA_character_
  if (!length(entries)) return(invisible(bk))
  n_before <- nrow(read_bib_entries(bib_path))
  if (backup) {
    bk <- backup_bib_file(bib_path, backup_dir)
    log_msg("Backup written to ", bk)
  }
  insert_bib_entries(bib_path, entries, years)
  verify_entry_count(bib_path, n_before + length(entries), bk)
  log_msg("Added ", length(entries), " new record(s) to ", bib_path, ".")
  invisible(bk)
}
