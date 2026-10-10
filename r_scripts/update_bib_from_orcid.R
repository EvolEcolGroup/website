#!/usr/bin/env Rscript
# =============================================================================
# update_bib_from_orcid.R
#
# Keep a BibTeX file in sync with an ORCID record:
#   - preprints in the bib that have since been published are replaced by the
#     published version (or removed, if that is already in the bib);
#   - publications on ORCID that are missing from the bib are retrieved from
#     Crossref and added to it, in chronological position.
# A time-stamped backup of the bib is made first. Details go to a dated log
# file; the console only shows a short summary.
#
# This file only loads the functions, which live in R/ (one file per step):
#   00_setup.R               package checks, small helpers
#   01_logging.R             log file used for all detailed output
#   02_text.R                normalising DOIs, titles, markup
#   03_bib_file.R            reading / writing / backing up the .bib
#   04_orcid.R               step 1: works on the ORCID record
#   05_matching.R            matching works by DOI, title and authors
#   06_crossref.R            Crossref API
#   07_bibtex_format.R       Crossref record -> BibTeX entry
#   08_new_records.R         step 3: find and add ORCID works missing from the bib
#   09_missing_from_orcid.R  (optional) bib entries missing from ORCID
#   10_preprints.R           step 2: replace preprints that have been published
#   11_update_bib_from_orcid.R  update_bib_from_orcid(), which runs it all
#
# Requirements: R >= 4.1 and the packages httr2 and stringi
#   install.packages(c("httr2", "stringi"))
#
# Usage from R:
#   source("update_bib_from_orcid.R")
#   update_bib_from_orcid("andrea_manica_pubs.bib", "0000-0003-1895-450X",
#                         mailto = "you@cam.ac.uk", author_family = "Manica")
#   (see ?update_bib_from_orcid in R/11_update_bib_from_orcid.R for all options)
#
# Usage from the shell:
#   Rscript update_bib_from_orcid.R <bib_file> <orcid> [mailto] [author_family]
# =============================================================================

local({
  # Folder of this script, whether it is source()d or run with Rscript
  script_dir <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) { script_dir <- dirname(normalizePath(f)); break }
  }
  if (is.null(script_dir)) {
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    script_dir <- if (length(f)) dirname(normalizePath(f[1])) else getwd()
  }
  r_dir <- file.path(script_dir, "R")
  if (!dir.exists(r_dir)) stop("Cannot find the R/ folder next to update_bib_from_orcid.R",
                               " (looked in ", r_dir, ").", call. = FALSE)
  for (f in sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = globalenv())
  }
})

# Run from the shell: Rscript update_bib_from_orcid.R <bib_file> <orcid> [mailto] [author_family]
if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2L) {
    cat("Usage: Rscript update_bib_from_orcid.R <bib_file> <orcid> [mailto] [author_family]\n")
    quit(status = 1L)
  }
  update_bib_from_orcid(bib_path      = args[1],
                        orcid         = args[2],
                        mailto        = if (length(args) >= 3L) args[3] else NULL,
                        author_family = if (length(args) >= 4L) args[4] else NULL)
}
