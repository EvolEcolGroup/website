# =============================================================================
# 07_bibtex_format.R
#
# Conversion of Crossref records into BibTeX entries: authors, entry type,
# container field, citation keys and the text of the entry.
# =============================================================================

#' Convert given names to initials in the style used in the .bib file
#'
#' `"Francis K."` -> `"FK"`, `"Anne-Sophie"` -> `"A-S"`. Tokens that are already
#' 2-3 capital letters (`"FK"`) are assumed to be initials and kept unchanged.
#'
#' @param given Character scalar of given name(s).
#' @return Character scalar of initials (possibly empty).
#' @examples
#' initials_from_given("Anne-Sophie")   # "A-S"
#' initials_from_given("J. W.")         # "JW"
initials_from_given <- function(given) {
  if (is.na(given) || !nzchar(given)) return("")
  tokens <- strsplit(trimws(gsub("\\.", " ", given)), "\\s+")[[1]]
  inits <- vapply(tokens, function(tok) {
    if (grepl("^[A-Z]{2,3}$", tok)) return(tok)           # already initials
    parts <- strsplit(tok, "-", fixed = TRUE)[[1]]        # keep hyphenated form
    parts <- parts[nzchar(parts)]
    paste(substr(parts, 1, 1), collapse = "-")
  }, character(1))
  paste(inits, collapse = "")
}

#' Format a single Crossref author for BibTeX
#'
#' \itemize{
#'   \item `"initials"`: `"FK McMeechan"` (the convention used in your .bib file)
#'   \item `"full"`: `"McMeechan, Francis K."` (standard BibTeX)
#' }
#' Organisational authors (consortia) are wrapped in braces so BibTeX treats them
#' as a single name.
#'
#' @param author One element of Crossref's `author` array (list with `given`,
#'   `family` and/or `name`).
#' @param style `"initials"` or `"full"`.
#' @return A character scalar, or `NA_character_` if the author has no name.
format_author <- function(author, style = c("initials", "full")) {
  style  <- match.arg(style)
  family <- clean_text(scalar_chr(author[["family"]]) %||% "")
  given  <- clean_text(scalar_chr(author[["given"]])  %||% "")
  org    <- clean_text(scalar_chr(author[["name"]])   %||% "")
  family <- if (is.na(family)) "" else family
  given  <- if (is.na(given))  "" else given
  org    <- if (is.na(org))    "" else org

  if (!nzchar(family) && nzchar(org)) return(paste0("{", org, "}"))  # consortium
  if (!nzchar(family)) { family <- given; given <- "" }              # mononym
  if (!nzchar(family)) return(NA_character_)

  if (style == "full") {
    if (nzchar(given)) paste0(family, ", ", given) else family
  } else {
    trimws(paste(initials_from_given(given), family))
  }
}

#' Format the complete author list ("A and B and C")
#'
#' @param authors Crossref `author` array (list of author lists).
#' @param style `"initials"` or `"full"`; see [format_author()].
#' @return A single string, or `NA_character_` if there are no authors.
format_authors <- function(authors, style = "initials") {
  out <- vapply(authors %||% list(), format_author, character(1), style = style)
  out <- out[!is.na(out)]
  if (length(out) == 0L) NA_character_ else paste(out, collapse = " and ")
}

#' Choose the BibTeX field that holds the container (journal, book, ...)
#'
#' Your existing .bib file stores the container title in `journal` for every
#' entry type, so that is the default (`"journal"`). `"standard"` uses the field
#' BibTeX normally expects for each type instead.
#'
#' @param bibtype BibTeX entry type, e.g. `"article"`.
#' @param mode `"journal"` (match the existing file) or `"standard"`.
#' @return Field name (character scalar).
container_field_for <- function(bibtype, mode = c("journal", "standard")) {
  mode <- match.arg(mode)
  if (mode == "journal") return("journal")
  switch(bibtype,
         article       = "journal",
         inproceedings = ,
         incollection  = ,
         inbook        = "booktitle",
         "howpublished")
}

# Crossref work type -> BibTeX entry type (anything else becomes @misc)
CROSSREF_TYPE_MAP <- c(
  "journal-article"     = "article",
  "proceedings-article" = "inproceedings",
  "book-chapter"        = "incollection",
  "reference-entry"     = "incollection",
  "book"                = "book",
  "monograph"           = "book",
  "edited-book"         = "book",
  "posted-content"      = "misc",        # preprints (bioRxiv, arXiv, ...)
  "dataset"             = "misc"
)

#' Convert a Crossref record into BibTeX fields
#'
#' Collects title (plus subtitle), complete author list, container title, volume,
#' issue, pages (or article number if there are no pages), publisher (for
#' non-journal items), year, DOI and URL. The URL is the DOI resolver link, which
#' is stable and always available.
#'
#' @param msg Crossref `message` (nested list), from [fetch_crossref_work()].
#' @param author_style `"initials"` or `"full"`; see [format_author()].
#' @param container_field `"journal"` or `"standard"`; see [container_field_for()].
#' @return A list with `bibtype`, `fields` (named character vector in the order
#'   they will be printed; missing ones are `NA`), plus `first_family`, `year`,
#'   `title`, `doi` and `crossref_type` used for key generation and reporting.
crossref_to_record <- function(msg, author_style = "initials",
                               container_field = "journal") {
  cr_type <- scalar_chr(msg[["type"]])
  bibtype <- if (!is.na(cr_type) && cr_type %in% names(CROSSREF_TYPE_MAP)) {
    unname(CROSSREF_TYPE_MAP[cr_type])
  } else "misc"

  # --- Title (append subtitle if Crossref stores it separately) ---------------
  title    <- clean_text(scalar_chr(safe_get(msg, "title", 1)))
  subtitle <- clean_text(scalar_chr(safe_get(msg, "subtitle", 1)))
  if (!is.na(subtitle) && nzchar(subtitle) && !is.na(title) &&
      !grepl(normalize_title(subtitle), normalize_title(title), fixed = TRUE)) {
    title <- paste0(title, ": ", subtitle)
  }

  # --- Container: journal / book / proceedings; for preprints the server name -
  container <- clean_text(scalar_chr(safe_get(msg, "container-title", 1)))
  if (is.na(container) || !nzchar(container)) {
    container <- clean_text(scalar_chr(safe_get(msg, "institution", 1, "name")))
  }
  if (is.na(container) || !nzchar(container)) {
    container <- clean_text(scalar_chr(safe_get(msg, "group-title")))
  }

  # --- Pages: normalise dashes; fall back to article number -------------------
  pages <- scalar_chr(msg[["page"]])
  if (!is.na(pages)) pages <- gsub("[\u2010-\u2015\u2212]", "-", pages)
  if (is.na(pages) || !nzchar(pages)) pages <- scalar_chr(msg[["article-number"]])

  doi  <- normalize_doi(scalar_chr(msg[["DOI"]]))
  year <- extract_year(msg)
  authors <- msg[["author"]] %||% list()

  fields <- c(
    title     = title,
    author    = format_authors(authors, author_style),
    container = container,
    volume    = scalar_chr(msg[["volume"]]),
    number    = scalar_chr(msg[["issue"]]),
    pages     = pages,
    publisher = if (bibtype == "article") NA_character_ else clean_text(scalar_chr(msg[["publisher"]])),
    year      = as.character(year),
    doi       = doi,
    url       = if (!is.na(doi)) paste0("https://doi.org/", doi) else NA_character_,
    crossref_type_label(msg)          # cache field: no need to look this DOI up again
  )
  names(fields)[length(fields)] <- TYPE_FIELD
  names(fields)[names(fields) == "container"] <- container_field_for(bibtype, container_field)

  first_family <- scalar_chr(safe_get(authors, 1, "family")) %||% NA_character_
  if (is.na(first_family)) first_family <- scalar_chr(safe_get(authors, 1, "name"))

  list(bibtype = bibtype, fields = fields, first_family = first_family,
       year = year, title = title, doi = doi, crossref_type = cr_type)
}

#' Generate a unique BibTeX citation key
#'
#' Follows the pattern already used in your file: first author's surname +
#' year + first significant word of the title (e.g. `Manica2002Filial`). If the
#' key already exists (in the .bib file or among other new records) a letter is
#' appended (`...a`, `...b`), as in `Carneiro2016Consistencya`. Comparison is
#' case-insensitive because BibTeX keys are.
#'
#' @param first_family Surname of the first author.
#' @param year Publication year.
#' @param title Title.
#' @param existing_keys Character vector of keys that must not be reused.
#' @return A unique key (character scalar).
#' @examples
#' make_cite_key("Manica", 2002, "Filial cannibalism in teleost fish", character())
make_cite_key <- function(first_family, year, title, existing_keys) {
  fam <- if (is.na(first_family)) "Anon" else
    gsub("[^A-Za-z]", "", stringi::stri_trans_general(first_family, "Latin-ASCII"))
  if (!nzchar(fam)) fam <- "Anon"
  yr <- if (is.na(year)) "nd" else as.character(year)

  # First "meaningful" word of the title (skip articles, prepositions, ...)
  stop_words <- c("a", "an", "the", "of", "on", "in", "and", "for", "to", "with",
                  "from", "by", "at", "is", "are", "do", "does", "how", "what", "why")
  words <- strsplit(normalize_title(title), " ")[[1]]
  words <- words[!(words %in% stop_words) & nchar(words) > 2L]
  w <- if (length(words)) words[1] else ""
  w <- paste0(toupper(substr(w, 1, 1)), substring(w, 2))

  base <- paste0(fam, yr, w)
  key  <- base
  suffixes <- c(letters, as.character(27:999))
  i <- 0L
  while (tolower(key) %in% tolower(existing_keys)) {
    i   <- i + 1L
    key <- paste0(base, suffixes[i])
  }
  key
}

#' Render one BibTeX entry as text
#'
#' Fields that are `NA` or empty are dropped. Output layout matches your file
#' (two-space indent, `name = {value}`). `&`, `%` and `#` are escaped in all
#' fields except `doi` and `url`.
#'
#' @param key Citation key.
#' @param bibtype Entry type (`"article"`, `"misc"`, ...).
#' @param fields Named character vector of field values (see [crossref_to_record()]).
#' @return A character scalar containing the whole entry.
format_bibtex_entry <- function(key, bibtype, fields) {
  fields <- fields[!is.na(fields) & nzchar(trimws(fields))]

  vals <- vapply(seq_along(fields), function(i) {
    v <- fields[[i]]
    if (!(names(fields)[i] %in% c("doi", "url"))) v <- escape_bibtex(v)
    balance_braces(v)
  }, character(1))

  body <- paste0("  ", names(fields), " = {", vals, "}", collapse = ",\n")
  paste0("@", bibtype, "{", key, ",\n", body, "\n}")
}
