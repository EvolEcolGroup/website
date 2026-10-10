# =============================================================================
# 03_bib_file.R
#
# Reading and writing the .bib file: field extraction, parsing into a data
# frame, cache fields, backups, and in-place removal / replacement / insertion
# of entries (everything else in the file is preserved byte for byte).
# =============================================================================

#' Extract one field value from the text of a single BibTeX entry
#'
#' Handles values delimited by `{...}` (including nested braces and values that
#' span several lines), by `"..."`, or bare numbers. The field name must start a
#' line, so `title` will not accidentally match `booktitle`.
#'
#' @param entry Character scalar: the full text of one BibTeX entry.
#' @param field Character scalar: field name, e.g. `"doi"` or `"title"`.
#' @return The raw field value as a string, or `NA_character_` if absent.
#' @examples
#' extract_bib_field("@article{k,\n  title = {A {nested} title},\n  year = {2020}\n}", "title")
extract_bib_field <- function(entry, field) {
  # Locate "<field> = " at the start of a line (case-insensitive)
  m <- regexpr(paste0("(?im)^\\s*", field, "\\s*=\\s*"), entry, perl = TRUE)
  if (m == -1L) return(NA_character_)

  rest  <- substring(entry, m + attr(m, "match.length"))
  chars <- strsplit(rest, "")[[1]]
  if (length(chars) == 0L) return(NA_character_)

  if (chars[1] == "{") {
    # Walk forward counting brace depth until the matching closing brace
    depth <- 0L
    end   <- length(chars)
    for (i in seq_along(chars)) {
      if (chars[i] == "{") depth <- depth + 1L
      if (chars[i] == "}") {
        depth <- depth - 1L
        if (depth == 0L) { end <- i; break }
      }
    }
    if (end <= 2L) return("")
    paste(chars[2:(end - 1L)], collapse = "")

  } else if (chars[1] == "\"") {
    # Quoted value: ends at the next unescaped double quote
    q <- which(chars == "\"")
    q <- q[q > 1L & chars[pmax(q - 1L, 1L)] != "\\"]
    if (length(q) == 0L) return(NA_character_)
    if (q[1] <= 2L) return("")
    paste(chars[2:(q[1] - 1L)], collapse = "")

  } else {
    # Bare value (e.g. year = 2020,)
    trimws(sub("^([^,}\n]*).*", "\\1", rest))
  }
}

# Names of the cache fields written to the .bib (see the header)
TYPE_FIELD    <- "orcidsync-type"
CHECKED_FIELD <- "orcidsync-checked"

#' Set one field in the text of a single BibTeX entry
#'
#' Replaces the field's value if the field is present, otherwise appends it as
#' the last field, using the indentation of the entry's other fields. Meant for
#' short, simple values (dates, type labels): the value is written in braces
#' as is.
#'
#' @param entry Character scalar: the full text of one BibTeX entry (trimmed).
#' @param field Field name.
#' @param value New value.
#' @return The modified entry text.
#' @examples
#' set_bib_field("@article{k,\n  year = {2020}\n}", "orcidsync-checked", "2026-10-10")
set_bib_field <- function(entry, field, value) {
  line <- paste0(field, " = {", value, "}")
  pat  <- paste0("(?im)^([ \t]*)", field, "\\s*=\\s*(\\{[^{}\n]*\\}|\"[^\"\n]*\"|[^,}\n]*)")
  if (grepl(pat, entry, perl = TRUE)) {
    return(sub(pat, paste0("\\1", line), entry, perl = TRUE))
  }
  indent <- regmatches(entry, regexpr("(?m)^[ \t]+(?=\\S)", entry, perl = TRUE))
  if (!length(indent)) indent <- "  "
  body <- sub(",?\\s*\\}\\s*$", "", entry)        # drop final "}" (and a trailing comma)
  paste0(body, ",\n", indent, line, "\n}")
}

#' Split the text of a .bib file into chunks, one per entry
#'
#' Splits immediately before every line that starts with `@type{`. Anything
#' before the first entry (e.g. header comments) becomes the first chunk.
#' Re-joining the chunks with `"\n"` reproduces the original text exactly, which
#' is what allows entries to be removed or replaced without touching the rest
#' of the file.
#'
#' @param txt The whole file as a single string (lines joined by `"\n"`).
#' @return Character vector of chunks.
split_bib_text <- function(txt) {
  strsplit(txt, "\n(?=@[A-Za-z]+\\s*\\{)", perl = TRUE)[[1]]
}

#' Citation key of each chunk produced by [split_bib_text()]
#'
#' @param parts Character vector of chunks.
#' @return Character vector (same length) with the entry key, or `NA` for chunks
#'   that are not real entries (header text, `@comment`, `@string`, `@preamble`).
bib_part_keys <- function(parts) {
  is_entry <- grepl("^\\s*@[A-Za-z]+\\s*\\{", parts) &
    !grepl("^\\s*@(comment|string|preamble)\\s*\\{", parts, ignore.case = TRUE)
  keys <- rep(NA_character_, length(parts))
  keys[is_entry] <- sub("(?s)^\\s*@[A-Za-z]+\\s*\\{\\s*([^,\\s]+)\\s*,.*", "\\1",
                        parts[is_entry], perl = TRUE)
  keys
}

#' Read a BibTeX file into a data frame of keys, DOIs and titles
#'
#' A deliberately light-weight parser: only what is needed to decide whether a
#' publication is already present (cite key, entry type, DOI, title). The .bib
#' file itself is never modified.
#'
#' @param bib_path Path to the .bib file.
#' @return A data frame with one row per entry and columns `key`, `type`,
#'   `doi` (normalised, `NA` if absent), `title` (raw), `title_norm`
#'   (normalised for comparison), `year` (integer, `NA` if absent), `cr_type`
#'   and `checked` (the cached `orcidsync-type` and `orcidsync-checked` values,
#'   `NA` if absent; `checked` is a Date) and `raw` (the entry's original text,
#'   kept so entries can be written back verbatim).
#' @examples
#' \dontrun{
#' bib <- read_bib_entries("andrea_manica_pubs.bib")
#' head(bib)
#' }
read_bib_entries <- function(bib_path) {
  if (!file.exists(bib_path)) stop("Bib file not found: ", bib_path, call. = FALSE)

  txt <- paste(readLines(bib_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Split the file at every line that starts a new entry ("@type{")
  parts <- split_bib_text(txt)
  parts <- parts[grepl("^\\s*@[A-Za-z]+\\s*\\{", parts)]

  type  <- tolower(sub("^\\s*@([A-Za-z]+)\\s*\\{.*", "\\1", parts))
  keep  <- !(type %in% c("comment", "string", "preamble"))   # real entries only
  parts <- parts[keep]
  type  <- type[keep]

  key   <- sub("(?s)^\\s*@[A-Za-z]+\\s*\\{\\s*([^,\\s]+)\\s*,.*", "\\1", parts, perl = TRUE)
  doi   <- normalize_doi(vapply(parts, extract_bib_field, character(1), field = "doi",
                                USE.NAMES = FALSE))
  title <- vapply(parts, extract_bib_field, character(1), field = "title",
                  USE.NAMES = FALSE)

  year  <- suppressWarnings(as.integer(vapply(parts, extract_bib_field, character(1),
                                              field = "year", USE.NAMES = FALSE)))

  cr_type <- trimws(vapply(parts, extract_bib_field, character(1), field = TYPE_FIELD,
                           USE.NAMES = FALSE))
  checked <- as.Date(vapply(parts, extract_bib_field, character(1), field = CHECKED_FIELD,
                            USE.NAMES = FALSE), format = "%Y-%m-%d")

  data.frame(key = key, type = type, doi = doi, title = title,
             title_norm = normalize_title(title), year = year,
             cr_type = cr_type, checked = checked,
             raw = trimws(parts), stringsAsFactors = FALSE)
}


#' Make a time-stamped backup copy of a file
#'
#' The copy is named `<name>_backup_<YYYY-MM-DD_HHMMSS>.<ext>`; if that name is
#' already taken (two runs in the same second) a counter is appended. Stops with
#' an error if the copy cannot be verified, so callers never modify a file that
#' has not been backed up.
#'
#' @param path File to back up.
#' @param backup_dir Directory for the copy (default: same folder as `path`).
#' @return Path of the backup file.
#' @examples
#' \dontrun{
#' backup_bib_file("andrea_manica_pubs.bib")
#' # "./andrea_manica_pubs_backup_2026-10-04_153012.bib"
#' }
backup_bib_file <- function(path, backup_dir = dirname(path)) {
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)

  base  <- tools::file_path_sans_ext(basename(path))
  ext   <- tools::file_ext(path)
  ext   <- if (nzchar(ext)) paste0(".", ext) else ".bib"
  stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

  dest <- file.path(backup_dir, paste0(base, "_backup_", stamp, ext))
  i <- 1L
  while (file.exists(dest)) {               # never overwrite an earlier backup
    i    <- i + 1L
    dest <- file.path(backup_dir, paste0(base, "_backup_", stamp, "_", i, ext))
  }

  ok <- file.copy(path, dest, overwrite = FALSE, copy.date = TRUE)
  if (!ok || !file.exists(dest) || file.size(dest) != file.size(path)) {
    stop("Could not create a verified backup of ", path,
         "; the file has NOT been modified.", call. = FALSE)
  }
  dest
}

#' Remove and/or replace entries in a .bib file, leaving everything else intact
#'
#' Works on the raw text, so header comments, formatting and all untouched
#' entries are preserved byte for byte. A replaced entry keeps the blank lines
#' that followed the original.
#'
#' @param bib_path Path to the .bib file (modified in place).
#' @param remove_keys Keys of entries to delete.
#' @param replacements Named character vector: names are keys of entries to
#'   replace, values the full text of the new entries.
#' @return `bib_path`, invisibly.
rewrite_bib_entries <- function(bib_path, remove_keys = character(),
                                replacements = character()) {
  txt   <- paste(readLines(bib_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  parts <- split_bib_text(txt)
  keys  <- bib_part_keys(parts)

  missing <- setdiff(c(remove_keys, names(replacements)), keys)
  if (length(missing)) stop("Keys not found in ", bib_path, ": ",
                            paste(missing, collapse = ", "), call. = FALSE)

  for (k in names(replacements)) {
    i <- which(keys == k)[1]
    trailing <- regmatches(parts[i], regexpr("\\s*$", parts[i]))   # keep spacing
    parts[i] <- paste0(replacements[[k]], trailing)
  }
  parts <- parts[is.na(keys) | !(keys %in% remove_keys)]

  writeLines(enc2utf8(paste(parts, collapse = "\n")), bib_path, useBytes = TRUE)
  invisible(bib_path)
}

#' Write a set of BibTeX entries to a .bib file
#'
#' General-purpose writer (UTF-8, entries separated by blank lines) with an
#' optional comment header.
#'
#' @param entries Character vector of BibTeX entries (one per element).
#' @param path Output file path.
#' @param header Optional character vector of lines written first; each is
#'   prefixed with `% ` so BibTeX ignores it.
#' @return `path`, invisibly.
write_bib_subset <- function(entries, path, header = NULL) {
  head_txt <- if (length(header)) c(paste0("% ", header), "") else character()
  body <- if (length(entries)) paste0(entries, "\n") else "% (no entries)"
  writeLines(enc2utf8(paste(c(head_txt, body), collapse = "\n")), path, useBytes = TRUE)
  invisible(path)
}

#' Insert new entries into a .bib file, keeping it in chronological order
#'
#' Each entry is placed just before the first existing entry with a later year
#' (or at the end of the file if there is none, or if the new entry has no
#' year), so a file sorted by year stays sorted. Works on the raw text, like
#' [rewrite_bib_entries()]: everything else is preserved byte for byte.
#'
#' @param bib_path Path to the .bib file (modified in place).
#' @param entries Character vector of BibTeX entries (one per element).
#' @param years Integer vector (same length) of their publication years.
#' @return `bib_path`, invisibly.
insert_bib_entries <- function(bib_path, entries, years) {
  if (!length(entries)) return(invisible(bib_path))
  txt   <- paste(readLines(bib_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  parts <- split_bib_text(txt)
  keys  <- bib_part_keys(parts)
  part_years <- rep(NA_integer_, length(parts))
  part_years[!is.na(keys)] <- suppressWarnings(as.integer(
    vapply(parts[!is.na(keys)], extract_bib_field, character(1), field = "year",
           USE.NAMES = FALSE)))

  for (i in seq_along(entries)) {
    later <- which(!is.na(keys) & !is.na(part_years) & part_years > years[i])
    if (!is.na(years[i]) && length(later)) {
      j <- later[1]                         # insert before entry j, blank line after
      parts      <- append(parts, paste0(entries[i], "\n"), after = j - 1L)
      keys       <- append(keys, "(new)", after = j - 1L)
      part_years <- append(part_years, years[i], after = j - 1L)
    } else {                                # append at the end of the file
      n <- length(parts)
      parts[n]   <- paste0(sub("\\s*$", "", parts[n]), "\n")
      parts      <- c(parts, entries[i])
      keys       <- c(keys, "(new)")
      part_years <- c(part_years, years[i])
    }
  }
  writeLines(enc2utf8(paste(parts, collapse = "\n")), bib_path, useBytes = TRUE)
  invisible(bib_path)
}

#' Check the number of entries in a .bib file after it has been rewritten
#'
#' Re-reads the file and stops with an error if it does not contain the
#' expected number of entries. If `backup` is given, the file is restored from
#' it first.
#'
#' @param bib_path Path to the .bib file.
#' @param n_expected Expected number of entries.
#' @param backup Path of a backup to restore on failure, or `NA` / `NULL`.
#' @return `TRUE`, invisibly (or an error).
verify_entry_count <- function(bib_path, n_expected, backup = NA_character_) {
  n_now <- tryCatch(nrow(read_bib_entries(bib_path)), error = function(e) -1L)
  if (n_now == n_expected) return(invisible(TRUE))
  restored <- !is.null(backup) && !is.na(backup) &&
    file.copy(backup, bib_path, overwrite = TRUE)
  stop("Check failed after rewriting ", bib_path, " (", n_now, " entries, expected ",
       n_expected, ").", if (restored) paste0(" The original file was restored from ", backup),
       call. = FALSE)
}
