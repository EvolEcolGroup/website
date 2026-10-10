# =============================================================================
# 11_update_bib_from_orcid.R
#
# Main entry point: runs every step on a .bib file and an ORCID record, writes
# the details to a dated log file and prints a short summary.
# =============================================================================

#' Update a .bib file from an ORCID record
#'
#' Runs the whole pipeline:
#' \enumerate{
#'   \item downloads the works listed on the ORCID record ([fetch_orcid_works()]);
#'   \item if `replace_preprints`, swaps preprints that have since been
#'     published for their published versions ([replace_published_preprints()]).
#'     This must come before step 3: ORCID usually groups a preprint with its
#'     published version, so step 3 sees that group as already in the bib and
#'     never adds the published DOI;
#'   \item finds ORCID works missing from the bib, retrieves their metadata from
#'     Crossref ([find_new_records()]) and, if `add_new`, adds them to the bib
#'     in chronological position ([add_new_records()]).
#' }
#' Works that are probably already in the bib under another DOI or title
#' (possible duplicates) and works that could not be retrieved from Crossref
#' are never added: they are listed in the log for you to check by hand.
#'
#' Before anything is changed, a time-stamped backup of the bib is made with
#' [backup_bib_file()]. If the run fails part-way, the bib is restored from it;
#' if the run ends without changing the bib, the backup is deleted again.
#'
#' Every detail (progress, the plan for each preprint, the BibTeX added,
#' possible duplicates, unresolved works, warnings) goes to a log file named
#' `update_bib_from_orcid_<YYYY-MM-DD_HHMMSS>.log` in `log_dir`. The console
#' only shows a short summary.
#'
#' Speed-up: results that never (or rarely) change are cached in the .bib
#' itself, in two extra fields that BibTeX/biblatex ignore. `orcidsync-type`
#' is the Crossref type of the entry's DOI (permanent, so looked up once).
#' `orcidsync-checked`, on preprints still unpublished, is the last date
#' Crossref and the preprint servers were asked; those checks are repeated only
#' after `recheck_days`. ORCID is downloaded and matched on every run. To force
#' a full re-check, delete the fields or set `recheck_days = 0`. If you change
#' an entry's DOI by hand, delete its `orcidsync-type`.
#'
#' @param bib_path Path to the .bib file to update.
#' @param orcid ORCID iD, e.g. `"0000-0003-1895-450X"`.
#' @param mailto Optional e-mail address, sent to Crossref (its "polite pool"
#'   is faster and more reliable). Recommended.
#' @param author_family Optional surname(s) required in the author list of
#'   Crossref title-search hits for ORCID works without a DOI (e.g. `"Manica"`).
#' @param title_threshold Title similarity (0-1) at or above which a work counts
#'   as already in the bib.
#' @param exclude_types ORCID work types to ignore, e.g. `c("data-set", "other")`.
#' @param replace_preprints Run step 2 (replace published preprints).
#' @param add_new Add the new records found in step 3 to the bib. If `FALSE`
#'   they are only listed in the log.
#' @param recheck_days Days before a still-unpublished preprint is checked
#'   again with Crossref and the preprint servers (`0` = every run).
#' @param keep_key If `TRUE`, a preprint replaced by its published version
#'   keeps its citation key, so `\cite{}` commands keep working.
#' @param check_unknown_dois Ask Crossref about DOIs not recognised as preprint
#'   DOIs, to catch preprint servers missing from `PREPRINT_DOI_PATTERNS`. Only
#'   uncached DOIs are queried; see [find_preprint_entries()].
#' @param resolve_no_doi Try to find ORCID works that have no DOI in Crossref
#'   by title search.
#' @param author_style `"initials"` (`"FK McMeechan"`) or `"full"`
#'   (`"McMeechan, Francis K."`) for new entries.
#' @param container_field `"journal"` (container title always in `journal`) or
#'   `"standard"` (`booktitle` etc. as BibTeX expects).
#' @param backup_dir Folder for the backup copy (default: next to the bib).
#' @param log_dir Folder for the log file (default: next to the bib).
#' @param dry_run If `TRUE`, nothing is written to the bib and no backup is
#'   made; the log shows what would have been done. (Step 3 then works on the
#'   bib as it is, before any preprint replacement.)
#' @return Invisibly, a list with `preprints` (the preprint plan, see
#'   [replace_published_preprints()]), `added` (data frame of the records added,
#'   or that would be added), `bibtex` (their text), `possible_duplicates`,
#'   `unresolved`, `backup` (path, or `NA` if none was kept) and `log` (path).
#' @examples
#' \dontrun{
#' update_bib_from_orcid("andrea_manica_pubs.bib", "0000-0003-1895-450X",
#'                       mailto = "you@cam.ac.uk", author_family = "Manica")
#' }
update_bib_from_orcid <- function(bib_path, orcid, mailto = NULL,
                                  author_family = NULL,
                                  title_threshold = 0.9,
                                  exclude_types = character(0),
                                  replace_preprints = TRUE,
                                  add_new = TRUE,
                                  recheck_days = 30,
                                  keep_key = TRUE,
                                  check_unknown_dois = TRUE,
                                  resolve_no_doi = TRUE,
                                  author_style = "initials",
                                  container_field = "journal",
                                  backup_dir = dirname(bib_path),
                                  log_dir = dirname(bib_path),
                                  dry_run = FALSE) {
  if (!file.exists(bib_path)) stop("Bib file not found: ", bib_path, call. = FALSE)
  write_bib <- !dry_run

  # ---- Log file ---------------------------------------------------------------
  log_path <- file.path(log_dir, paste0("update_bib_from_orcid_",
                                        format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".log"))
  log_start(log_path)
  on.exit(log_stop(), add = TRUE)
  log_lines(c(paste0("update_bib_from_orcid() run on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
              paste0("  bib file:          ", normalizePath(bib_path)),
              paste0("  ORCID:             ", orcid),
              paste0("  replace preprints: ", replace_preprints),
              paste0("  add new records:   ", add_new),
              paste0("  recheck days:      ", recheck_days),
              paste0("  dry run:           ", dry_run)))

  # ---- Backup, so that a failed run can be undone -----------------------------
  md5_before <- unname(tools::md5sum(bib_path))
  backup <- if (write_bib) backup_bib_file(bib_path, backup_dir) else NA_character_
  if (!is.na(backup)) log_msg("Backup written to ", backup)

  pp  <- NULL
  res <- NULL
  n_warnings <- 0L
  tryCatch(withCallingHandlers({

    log_section("ORCID")
    orcid_works <- parse_orcid_works(fetch_orcid_works(orcid, mailto))
    log_msg("ORCID lists ", length(unique(orcid_works$group_id)), " works (",
            sum(!is.na(orcid_works$doi)), " DOIs).")

    # ---- Step 2: preprints that have since been published ---------------------
    if (replace_preprints) {
      log_section("PUBLISHED PREPRINTS")
      pp <- replace_published_preprints(
        bib_path           = bib_path,
        mailto             = mailto,
        keep_key           = keep_key,
        orcid_works        = orcid_works,
        check_unknown_dois = check_unknown_dois,
        author_style       = author_style,
        container_field    = container_field,
        title_threshold    = title_threshold,
        recheck_days       = recheck_days,
        dry_run            = dry_run,
        backup             = FALSE)            # covered by the backup above
      log_lines("")
      log_table(pp$plan, c("key", "action", "preprint_doi", "published_doi", "new_key", "note"))
      if (length(pp$new_entries)) {
        log_lines(c("", "Replacement entries:", ""))
        log_lines(paste0(pp$new_entries, "\n"))
      }
    }

    # ---- Step 3: ORCID works missing from the bib -----------------------------
    log_section("NEW RECORDS")
    res <- find_new_records(
      bib_path        = bib_path,
      orcid           = orcid,
      mailto          = mailto,
      title_threshold = title_threshold,
      author_style    = author_style,
      container_field = container_field,
      resolve_no_doi  = resolve_no_doi,
      author_family   = author_family,
      exclude_types   = exclude_types,
      orcid_works     = orcid_works)
    if (add_new && write_bib) {
      add_new_records(bib_path, res$bibtex, res$records$year, backup = FALSE)
    }
    log_lines(c("", if (add_new && write_bib) "Records added:" else
      "Records found (NOT added to the bib):", ""))
    if (length(res$bibtex)) log_lines(paste0(res$bibtex, "\n")) else log_lines("None.")

    log_section("POSSIBLE DUPLICATES (not added; check manually)")
    log_table(res$possible_duplicates,
              c("title", "year", "doi", "matched_key", "title_similarity"))

    log_section("COULD NOT BE RETRIEVED FROM CROSSREF (not added; add manually)")
    log_table(res$unresolved, intersect(c("title", "year", "type", "doi", "reason"),
                                        names(res$unresolved)))

  }, warning = function(w) {
    # Warnings go to the log only, and are counted for the summary
    n_warnings <<- n_warnings + 1L
    log_msg("WARNING: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    log_msg("ERROR: ", conditionMessage(e))
    changed  <- unname(tools::md5sum(bib_path)) != md5_before
    restored <- !is.na(backup) && changed && file.copy(backup, bib_path, overwrite = TRUE)
    if (restored) log_msg("The bib file was restored from ", backup)
    if (!is.na(backup) && !changed) unlink(backup)       # nothing to undo
    stop(conditionMessage(e),
         if (restored) paste0("\nThe bib file was restored from ", backup),
         "\nDetails in ", log_path, call. = FALSE)
  })

  # ---- Keep the backup only if the bib was changed ----------------------------
  changed <- unname(tools::md5sum(bib_path)) != md5_before
  if (!is.na(backup) && !changed) {
    unlink(backup)
    backup <- NA_character_
    log_msg("The bib file was not changed; backup deleted.")
  }

  # ---- Console summary ----------------------------------------------------------
  plan <- if (is.null(pp)) NULL else pp$plan
  n_action <- function(a) if (is.null(plan)) 0L else sum(plan$action == a)
  verb <- if (dry_run) "would be " else ""
  added_label <- if (add_new && write_bib) "added" else "found (not added)"
  lines <- c(
    paste0("update_bib_from_orcid: ", bib_path, " (ORCID ", orcid, ")",
           if (dry_run) " [DRY RUN: bib not modified]"),
    if (replace_preprints) c(
      paste0("  Preprints ", verb, "replaced by published version: ", n_action("replace")),
      paste0("  Preprints ", verb, "removed (published version already in bib): ",
             n_action("remove"))),
    paste0("  New publications ", added_label, ": ", nrow(res$records)),
    paste0("  Possible duplicates (not added): ", nrow(res$possible_duplicates)),
    paste0("  Not retrievable from Crossref (not added): ", nrow(res$unresolved)),
    if (n_warnings) paste0("  Warnings: ", n_warnings),
    if (!is.na(backup)) paste0("  Backup: ", backup),
    paste0("  Log: ", log_path))
  cat(lines, sep = "\n")
  log_section("SUMMARY")
  log_lines(lines)

  invisible(list(preprints = plan, added = res$records, bibtex = res$bibtex,
                 possible_duplicates = res$possible_duplicates,
                 unresolved = res$unresolved, backup = backup, log = log_path))
}
