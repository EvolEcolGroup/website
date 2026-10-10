# =============================================================================
# 02_text.R
#
# Text normalisation: DOIs, titles, HTML entities, markup, BibTeX escaping.
# =============================================================================

#' Normalise DOIs to a canonical form
#'
#' DOIs are case-insensitive and appear in many guises (`https://doi.org/...`,
#' `doi:...`, trailing full stops). This converts them all to bare lower-case
#' `10.xxxx/yyyy` strings so they can be compared reliably. Anything that does
#' not look like a DOI becomes `NA`.
#'
#' @param x Character vector of DOIs / DOI URLs.
#' @return Character vector of the same length with normalised DOIs or `NA`.
#' @examples
#' normalize_doi(c("https://doi.org/10.1038/NATURE12345", "doi:10.1/abc."))
normalize_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", x)  # strip resolver prefix
  x <- sub("^doi:\\s*", "", x)                        # strip "doi:" prefix
  x <- sub("[.,;]+$", "", x)                          # strip trailing punctuation
  x[!grepl("^10\\.\\d{4,9}/", x)] <- NA_character_    # not a DOI -> NA
  x
}

#' Decode HTML/XML character entities
#'
#' Crossref titles frequently contain entities such as `&amp;` or `&#x2013;`.
#' `&amp;` is decoded last so that something like `&amp;lt;` is not decoded twice.
#'
#' @param x Character vector.
#' @return Character vector with entities replaced by the characters they encode.
decode_entities <- function(x) {
  ok <- !is.na(x)
  y <- x[ok]

  # Numeric entities, decimal (&#8211;) and hexadecimal (&#x2013;)
  m <- gregexpr("&#[xX]?[0-9A-Fa-f]+;", y)
  regmatches(y, m) <- lapply(regmatches(y, m), function(ents) {
    vapply(ents, function(e) {
      is_hex <- grepl("^&#[xX]", e)
      code   <- sub("^&#[xX]?([0-9A-Fa-f]+);$", "\\1", e)
      intToUtf8(strtoi(code, base = if (is_hex) 16L else 10L))
    }, character(1), USE.NAMES = FALSE)
  })

  # Named entities (ampersand last, see above)
  named <- c("&lt;" = "<", "&gt;" = ">", "&quot;" = "\"", "&apos;" = "'",
             "&nbsp;" = " ", "&amp;" = "&")
  for (e in names(named)) y <- gsub(e, named[[e]], y, fixed = TRUE)

  x[ok] <- y
  x
}

#' Strip markup and tidy whitespace in text coming from the APIs
#'
#' Removes HTML / JATS / MathML tags (`<i>`, `<jats:italic>`, `<mml:math>`...),
#' decodes entities and collapses whitespace.
#'
#' @param x Character vector.
#' @return Cleaned character vector.
#' @examples
#' clean_text("Evolution of <i>Homo sapiens</i> &amp; friends")
clean_text <- function(x) {
  x <- gsub("</?[A-Za-z][^>]*>", "", x)   # tags only (a bare '<' is left alone)
  x <- decode_entities(x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

#' Normalise a title for fuzzy comparison
#'
#' Removes markup, LaTeX braces/commands, accents and punctuation, and lower-cases,
#' so that titles written slightly differently in ORCID, Crossref and the .bib
#' file compare as (nearly) identical.
#'
#' @param x Character vector of titles.
#' @return Character vector of normalised titles.
#' @examples
#' normalize_title("Deﬁning {and} Measuring Success, in Conservation!")
normalize_title <- function(x) {
  x <- clean_text(x)
  x <- gsub("\\\\[A-Za-z]+|[{}\\\\]", "", x)                    # LaTeX leftovers
  x <- stringi::stri_trans_general(x, "Latin-ASCII")           # accents, ligatures
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]+", " ", x)                              # punctuation -> space
  trimws(gsub("\\s+", " ", x))
}

#' Escape characters that are special in BibTeX text fields
#'
#' Escapes `&`, `%` and `#` (unless already escaped). Not applied to DOI / URL
#' fields, where these characters are legitimate.
#'
#' @param x Character vector.
#' @return Character vector with special characters escaped.
escape_bibtex <- function(x) {
  gsub("(?<!\\\\)([&%#])", "\\\\\\1", x, perl = TRUE)
}

#' Remove braces from a string if they are unbalanced
#'
#' Unbalanced braces would break the whole .bib file, so as a safeguard they are
#' stripped if the opening and closing counts differ.
#'
#' @param x A single string.
#' @return The string, with all braces removed if they were unbalanced.
balance_braces <- function(x) {
  n_open  <- lengths(regmatches(x, gregexpr("{", x, fixed = TRUE)))
  n_close <- lengths(regmatches(x, gregexpr("}", x, fixed = TRUE)))
  if (n_open != n_close) gsub("[{}]", "", x) else x
}
