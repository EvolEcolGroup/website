#' Read a BibTeX file into a data.frame of publications
#'
#' Turns the .bib file kept up to date by `update_bib_from_orcid()` into a
#' data.frame with one row per entry, ready for [format_bib_publication_list()].
#' Field values are extracted with `extract_bib_field()` (from
#' `R/03_bib_file.R`), so `update_bib_from_orcid.R` must be sourced first.
#'
#' @param bib_path path to the .bib file
#' @returns a data.frame with columns `key`, `title`, `authors` (a list column,
#' one character vector of names per entry, as written in the bib, e.g. "A Manica"),
#' `journal`, `volume`, `number`, `pages`, `year` (integer), `doi` and `url`

read_bib_publications <- function(bib_path) {
  txt <- paste(readLines(bib_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # split into entries and drop anything that is not a real entry
  parts <- strsplit(txt, "\n(?=@[A-Za-z]+\\s*\\{)", perl = TRUE)[[1]]
  parts <- parts[grepl("^\\s*@[A-Za-z]+\\s*\\{", parts) &
                   !grepl("^\\s*@(comment|string|preamble)\\s*\\{", parts, ignore.case = TRUE)]

  get_field <- function(field) {
    x <- vapply(parts, extract_bib_field, character(1), field = field, USE.NAMES = FALSE)
    x <- clean_bib_text(x)
    x[!is.na(x) & x == ""] <- NA_character_
    x
  }

  authors <- get_field("author")
  pubs <- data.frame(
    key = sub("(?s)^\\s*@[A-Za-z]+\\s*\\{\\s*([^,\\s]+)\\s*,.*", "\\1", parts, perl = TRUE),
    title = get_field("title"),
    journal = get_field("journal"),
    volume = get_field("volume"),
    number = get_field("number"),
    pages = get_field("pages"),
    year = suppressWarnings(as.integer(get_field("year"))),
    doi = get_field("doi"),
    url = get_field("url"),
    stringsAsFactors = FALSE
  )
  # authors are separated by " and " in BibTeX
  pubs$authors <- lapply(authors, function(x) {
    if (is.na(x)) return(character(0))
    trimws(strsplit(x, "\\s+and\\s+")[[1]])
  })
  # if there is no url, fall back on the doi
  no_url <- is.na(pubs$url) & !is.na(pubs$doi)
  pubs$url[no_url] <- paste0("https://doi.org/", pubs$doi[no_url])
  pubs
}

#' Remove BibTeX markup from field values
#'
#' Removes protective braces and unescapes the special characters that BibTeX
#' requires to be escaped (e.g. `\&`, also when doubly escaped as `\\&`).
#'
#' @param x a character vector
#' @returns a character vector with the markup removed

clean_bib_text <- function(x) {
  x <- gsub("\\\\+([&%$#_])", "\\1", x)  # \& \% \$ \# \_ (and \\&)
  x <- gsub("[{}]", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

#' Swap an author name from "AN Other" to "Other, AN."
#'
#' Same convention as `scholar:::swap_initials`, which was used to format the
#' Google Scholar lists.
#'
#' @param x a character vector of names in the format "AN Other"
#' @returns a character vector of names in the format "Other, AN."

swap_author_initials <- function(x) {
  sub("(.*) (.*)", "\\2, \\1.", trimws(x))
}

#' Format a publication list from a BibTeX file
#'
#' The equivalent of [format_publication_list()] (in `papers_functions.R`) for a
#' .bib file instead of the csv files downloaded from Google Scholar. Each entry
#' is formatted as
#' `Author, A., Other, AN. & Last, B. (2020). Title. *Journal*. 12 (3), 1-10 [link]`
#' with group members in bold (only for papers published while they were in
#' the group), most recent papers first.
#'
#' @param pubs a data.frame as produced by [read_bib_publications()], or the
#' path to a .bib file
#' @param authors_metadata either a single string (for a single author, in the
#' format "AN Other"), or a data.frame of authors details. The key column is
#' papers_name (which can consist of multiple names, separated by a ;, in the
#' format AN Other); the columns group_start and papers_end give the range of
#' years over which the author is highlighted
#' @param with_url_link if TRUE, include a link to the publication
#' @returns a character vector with one formatted publication per element

format_bib_publication_list <- function(pubs, authors_metadata = NULL, with_url_link = TRUE) {
  if (is.character(pubs)) {
    pubs <- read_bib_publications(pubs)
  }

  # build a table of names to highlight, one row per name, with a range of years
  if (is.null(authors_metadata)) {
    highlight <- data.frame(name = character(0), start = integer(0), end = integer(0))
  } else if (is.character(authors_metadata)) {
    highlight <- data.frame(name = trimws(authors_metadata), start = -Inf, end = Inf)
  } else {
    names_list <- strsplit(as.character(authors_metadata$papers_name), ";")
    n_names <- lengths(names_list)
    highlight <- data.frame(
      name = trimws(unlist(names_list)),
      start = rep(as.integer(authors_metadata$group_start), n_names),
      end = rep(as.integer(authors_metadata$papers_end), n_names)
    )
    highlight <- highlight[!is.na(highlight$name) & highlight$name != "", ]
    highlight$start[is.na(highlight$start)] <- -Inf
    highlight$end[is.na(highlight$end)] <- Inf
  }

  # format the author list of each publication
  author_string <- vapply(seq_len(nrow(pubs)), function(i) {
    authors <- pubs$authors[[i]]
    if (length(authors) == 0) return("")
    year <- pubs$year[i]
    to_bold <- highlight$name[!is.na(year) & highlight$start <= year & highlight$end >= year]
    is_bold <- authors %in% to_bold
    # organisations (no initials) are left as they are
    is_person <- grepl(" ", authors)
    authors[is_person] <- swap_author_initials(authors[is_person])
    authors[is_bold] <- paste0("**", authors[is_bold], "**")
    if (length(authors) > 1) {
      authors[length(authors)] <- paste0("& ", authors[length(authors)])
    }
    paste(authors, collapse = ", ")
  }, character(1))

  # volume (number), pages
  details <- ifelse(is.na(pubs$volume), "", pubs$volume)
  details <- ifelse(is.na(pubs$number), details, trimws(paste0(details, " (", pubs$number, ")")))
  details <- ifelse(is.na(pubs$pages), details,
                    ifelse(details == "", pubs$pages, paste0(details, ", ", pubs$pages)))

  journal <- ifelse(is.na(pubs$journal), "", paste0(" *", pubs$journal, "*."))
  details <- ifelse(details == "", "", paste0(" ", details))

  # format link
  if (with_url_link) {
    url_link <- ifelse(is.na(pubs$url), "", paste0(" [{{< fa link>}}](<", pubs$url, ">)"))
  } else {
    url_link <- ""
  }

  res <- paste0(author_string, " (", pubs$year, "). ", sub("\\.$", "", pubs$title), ".",
                journal, details, url_link)
  # most recent first (keeping the order in the bib within a year)
  res[order(-pubs$year, seq_along(res), na.last = TRUE)]
}
