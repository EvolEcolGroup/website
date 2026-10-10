# =============================================================================
# 05_matching.R
#
# Matching works between ORCID, Crossref and the .bib file: title similarity,
# author overlap, and classification of ORCID works as in_bib / new / ...
# =============================================================================

#' Find the closest title in a reference set
#'
#' Similarity is `1 - (Levenshtein distance / length of the longer title)` on
#' normalised titles, so 1 means identical and values near 1 mean "almost the
#' same" (e.g. a trailing full stop or different capitalisation).
#'
#' @param title_norm A single normalised title.
#' @param ref_titles_norm Character vector of normalised reference titles.
#' @param ref_keys Character vector (same length) of labels to return for the best hit.
#' @return A list with `similarity` (numeric, `NA` if not computable) and `key`.
best_title_match <- function(title_norm, ref_titles_norm, ref_keys) {
  none <- list(similarity = NA_real_, key = NA_character_)
  if (is.na(title_norm) || !nzchar(title_norm)) return(none)

  ok <- !is.na(ref_titles_norm) & nzchar(ref_titles_norm)
  if (!any(ok)) return(none)

  d     <- drop(utils::adist(title_norm, ref_titles_norm[ok]))
  denom <- pmax(nchar(title_norm), nchar(ref_titles_norm[ok]))
  sim   <- 1 - d / denom
  best  <- which.max(sim)
  list(similarity = sim[best], key = ref_keys[ok][best])
}

#' Classify each ORCID work relative to the existing .bib file
#'
#' Status values:
#' \itemize{
#'   \item `in_bib`: a DOI of the work (or of a sibling version in the same ORCID
#'     group) is already in the .bib file.
#'   \item `possible_duplicate`: no DOI match, but the title is nearly identical
#'     (>= `title_threshold`) to an entry already in the .bib file. This catches
#'     entries without a DOI, and entries stored under a different DOI (e.g. the
#'     preprint instead of the published version).
#'   \item `new`: has a DOI and nothing similar is in the .bib file.
#'   \item `new_no_doi`: no DOI and nothing similar in the .bib file.
#' }
#'
#' @param orcid_works Output of [parse_orcid_works()].
#' @param bib Output of [read_bib_entries()].
#' @param title_threshold Similarity (0-1) at or above which titles count as the same.
#' @return `orcid_works` with extra columns `status`, `matched_key` (the .bib
#'   entry it matched, if any) and `title_similarity`.
classify_orcid_works <- function(orcid_works, bib, title_threshold = 0.9) {
  n <- nrow(orcid_works)
  if (n == 0L) {
    return(cbind(orcid_works, status = character(), matched_key = character(),
                 title_similarity = numeric(), stringsAsFactors = FALSE))
  }

  # (a) DOI-based match; a work counts as present if ANY of its DOIs is in the bib
  doi_hit    <- !is.na(orcid_works$doi) & orcid_works$doi %in% stats::na.omit(bib$doi)
  group_hit  <- as.logical(stats::ave(doi_hit, orcid_works$group_id, FUN = any))

  matched_key <- rep(NA_character_, n)
  matched_key[doi_hit] <- bib$key[match(orcid_works$doi[doi_hit], bib$doi)]

  # (b) Title-based match, only for works not already matched by DOI
  title_norm <- normalize_title(orcid_works$title)
  sim        <- rep(NA_real_, n)
  for (i in which(!group_hit)) {
    m <- best_title_match(title_norm[i], bib$title_norm, bib$key)
    sim[i]         <- m$similarity
    matched_key[i] <- m$key
  }

  orcid_works$status <- ifelse(group_hit, "in_bib",
                        ifelse(!is.na(sim) & sim >= title_threshold, "possible_duplicate",
                        ifelse(is.na(orcid_works$doi), "new_no_doi", "new")))
  # Only report a matched key when it is meaningful
  matched_key[orcid_works$status %in% c("new", "new_no_doi")] <- NA_character_
  orcid_works$matched_key      <- matched_key
  orcid_works$title_similarity <- round(sim, 3)
  orcid_works
}


# Words ignored when comparing titles word by word
TITLE_STOPWORDS <- c("a", "an", "the", "of", "on", "in", "and", "for", "to", "with",
                     "from", "by", "at", "is", "are", "as", "its", "their")

#' Similarity between titles that tolerates rewording
#'
#' Returns the larger of (i) the character-level similarity used elsewhere
#' (1 - Levenshtein distance / length) and (ii) the word-overlap coefficient
#' (shared content words / words in the shorter title, stop words removed).
#' (ii) stays high when a title is shortened or a subtitle added or dropped on
#' publication, which (i) penalises. Word overlap is only used when both titles
#' have at least 3 content words, to avoid spurious matches on short titles.
#'
#' @param a A single normalised title (see [normalize_title()]).
#' @param b Character vector of normalised titles.
#' @return Numeric vector (0-1), same length as `b`; 0 where `b` is `NA`.
title_similarity <- function(a, b) {
  out <- numeric(length(b))
  if (is.na(a) || !nzchar(a)) return(out)
  ok <- !is.na(b) & nzchar(b)
  if (!any(ok)) return(out)

  lev <- 1 - drop(utils::adist(a, b[ok])) / pmax(nchar(a), nchar(b[ok]))
  wa  <- setdiff(strsplit(a, " ")[[1]], TITLE_STOPWORDS)
  ov  <- vapply(strsplit(b[ok], " "), function(wb) {
    wb <- setdiff(wb, TITLE_STOPWORDS)
    if (length(wa) < 3L || length(wb) < 3L) return(0)
    length(intersect(wa, wb)) / min(length(wa), length(wb))
  }, numeric(1))
  out[ok] <- pmax(lev, ov)
  out
}

#' Extract the surnames of the authors in a BibTeX author field
#'
#' Works for both `"FK McMeechan and A Manica"` and `"McMeechan, F. K. and ..."`.
#' Used to check that a title-search hit shares at least one author.
#'
#' @param author Character scalar, the BibTeX `author` field.
#' @return Lower-case, accent-free character vector of surnames.
bib_author_families <- function(author) {
  if (is.na(author) || !nzchar(author)) return(character())
  a   <- trimws(strsplit(gsub("[{}]", "", author), "\\s+and\\s+")[[1]])
  fam <- ifelse(grepl(",", a), sub(",.*$", "", a), sub("^.*\\s", "", a))
  unique(tolower(stringi::stri_trans_general(trimws(fam), "Latin-ASCII")))
}

#' Share of a preprint's authors found in a published record
#'
#' Used to confirm candidates found by title (from ORCID or a Crossref search):
#' a genuine published version shares most of its authors with the preprint.
#'
#' @param bib_author The preprint's BibTeX `author` field.
#' @param crossref_authors The `author` array of the candidate Crossref record.
#' @return Proportion (0-1) of the preprint's surnames present in the published
#'   author list, or `NA` if the preprint has no authors.
author_overlap <- function(bib_author, crossref_authors) {
  pre <- bib_author_families(bib_author)
  if (!length(pre)) return(NA_real_)
  pub <- vapply(crossref_authors %||% list(),
                function(a) scalar_chr(a[["family"]]), character(1))
  pub <- tolower(stringi::stri_trans_general(pub[!is.na(pub)], "Latin-ASCII"))
  mean(pre %in% pub)
}
