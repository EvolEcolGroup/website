# =============================================================================
# 10_preprints.R
#
# Step 2: preprints (bioRxiv, arXiv, EarthArXiv, OSF, ...) in the bib that have
# since been published are replaced by, or merged into, the published version.
# =============================================================================

# Known preprint servers, identified by the pattern of their DOIs (DOIs are
# normalised to lower case first). Order matters only for readability: patterns
# do not overlap. Extend this table if you use other servers; anything missing
# is still caught by the Crossref check in find_preprint_entries()
# (check_unknown_dois = TRUE), which flags any DOI Crossref types as a preprint.
PREPRINT_DOI_PATTERNS <- c(
  # 10.1101 is shared with Cold Spring Harbor journals (e.g. Genome Research
  # "10.1101/gr.159426.113"), so only the numeric bioRxiv/medRxiv ids match.
  "bioRxiv/medRxiv"  = "^10\\.(1101|64898)/(\\d{6}|\\d{4}\\.\\d{2}\\.\\d{2}\\.\\d+)(v\\d+)?$",
  "arXiv"            = "^10\\.48550/arxiv\\.",
  "EarthArXiv"       = "^10\\.31223/",
  "ESS Open Archive" = "^10\\.1002/essoar\\.",
  # Copernicus/EGU "Discussions" preprints (cp-2019-91, egusphere-2023-123);
  # published Copernicus papers look different (cp-15-1-2019).
  "Copernicus Discussions" = "^10\\.5194/[a-z]+-\\d{4}-\\d+$",
  "OSF Preprints"    = "^10\\.31219/",
  "PsyArXiv"         = "^10\\.31234/",
  "SocArXiv"         = "^10\\.31235/",
  "MetaArXiv"        = "^10\\.31222/",
  "EdArXiv"          = "^10\\.35542/",
  "AfricArXiv"       = "^10\\.31730/",
  "EcoEvoRxiv"       = "^10\\.32942/",
  "ChemRxiv"         = "^10\\.26434/chemrxiv",
  "TechRxiv"         = "^10\\.36227/techrxiv",
  "Research Square"  = "^10\\.21203/rs\\.",
  "Preprints.org"    = "^10\\.20944/preprints",
  "SSRN"             = "^10\\.2139/ssrn\\.",
  "Authorea"         = "^10\\.22541/au\\.",
  "PeerJ Preprints"  = "^10\\.7287/peerj\\.preprints",
  "Qeios"            = "^10\\.32388/",
  "SciELO Preprints" = "^10\\.1590/scielopreprints"
)

# Text clues (journal / url / note ...) used ONLY for entries without a DOI.
# Specific names come before the generic "arxiv" and "preprint".
PREPRINT_TEXT_PATTERNS <- c(
  "EarthArXiv" = "eartharxiv", "ESS Open Archive" = "essoar|essopenarchive",
  "PsyArXiv" = "psyarxiv", "SocArXiv" = "socarxiv", "MetaArXiv" = "metaarxiv",
  "EdArXiv" = "edarxiv", "AfricArXiv" = "africarxiv", "EcoEvoRxiv" = "ecoevorxiv",
  "ChemRxiv" = "chemrxiv", "TechRxiv" = "techrxiv", "bioRxiv/medRxiv" = "biorxiv|medrxiv",
  "arXiv" = "arxiv", "Copernicus Discussions" = "discussions|egusphere",
  "Research Square" = "research ?square", "Preprints.org" = "preprints\\.org",
  "SSRN" = "ssrn", "Authorea" = "authorea", "OSF Preprints" = "osf\\.io",
  "Qeios" = "qeios", "Unknown preprint server" = "preprint"
)

#' Identify the preprint server of DOIs
#'
#' @param doi Character vector of normalised DOIs.
#' @return Character vector with the server name from
#'   `PREPRINT_DOI_PATTERNS`, or `NA` if the DOI is not a known preprint DOI.
#' @examples
#' preprint_server_from_doi(c("10.1101/015800", "10.1101/gr.159426.113",
#'                            "10.31223/x5k17p", "10.5194/cp-2019-91"))
preprint_server_from_doi <- function(doi) {
  vapply(doi, function(d) {
    if (is.na(d)) return(NA_character_)
    hit <- which(vapply(PREPRINT_DOI_PATTERNS, grepl, logical(1), x = d, perl = TRUE))
    if (length(hit)) names(PREPRINT_DOI_PATTERNS)[hit[1]] else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Test whether DOIs belong to a known preprint server
#'
#' @param doi Character vector of normalised DOIs.
#' @return Logical vector (`FALSE` for `NA`).
is_preprint_doi <- function(doi) !is.na(preprint_server_from_doi(doi))

#' Identify the preprint server from free text (for entries without a DOI)
#'
#' @param text Character vector (lower case) combining journal, url, note, ...
#' @return Character vector of server names or `NA`.
preprint_server_from_text <- function(text) {
  vapply(text, function(t) {
    hit <- which(vapply(PREPRINT_TEXT_PATTERNS, grepl, logical(1), x = t, perl = TRUE))
    if (length(hit)) names(PREPRINT_TEXT_PATTERNS)[hit[1]] else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Is a Crossref record a preprint?
#'
#' Crossref types preprints as `posted-content` with subtype `preprint`
#' (SSRN-style working papers are also accepted).
#'
#' @param msg Crossref `message` (nested list) or `NULL`.
#' @return `TRUE` / `FALSE`.
is_crossref_preprint <- function(msg) {
  if (is.null(msg)) return(FALSE)
  subtype <- scalar_chr(msg[["subtype"]])
  identical(scalar_chr(msg[["type"]]), "posted-content") &&
    (is.na(subtype) || subtype %in% c("preprint", "working-paper"))
}

#' Does a cached `orcidsync-type` value denote a preprint?
#'
#' Same rule as [is_crossref_preprint()], applied to [crossref_type_label()].
#'
#' @param x Character vector of type labels.
#' @return Logical vector (`FALSE` for `NA`).
is_preprint_type_label <- function(x) {
  !is.na(x) & x %in% c("posted-content", "posted-content:preprint",
                       "posted-content:working-paper")
}

#' Find all preprint entries in a bib
#'
#' An entry is treated as a preprint when:
#' \enumerate{
#'   \item its DOI matches a known preprint server (`PREPRINT_DOI_PATTERNS`); or
#'   \item it has no DOI but its URL contains a preprint DOI, or it carries an
#'     arXiv identifier (url, `eprint`, or "arXiv:1234.5678" in the journal), or
#'     its journal/url/note text names a preprint server; or
#'   \item (`check_unknown_dois = TRUE`) its DOI is not a known preprint DOI but
#'     Crossref types it as a preprint. This catches servers missing from the
#'     table, at the cost of one Crossref request per remaining DOI (roughly two
#'     minutes for ~300 DOIs). Entries with an `orcidsync-type` field are not
#'     looked up again: the cached type is used instead.
#' }
#' An entry with a journal DOI is never treated as a preprint because of its
#' text alone (e.g. a published paper whose url points to arXiv).
#'
#' @param bib Output of [read_bib_entries()].
#' @param check_unknown_dois Ask Crossref about every other DOI (see above).
#' @param mailto Optional e-mail for Crossref.
#' @param cache Optional environment used by [crossref_cached()].
#' @return Data frame with `key`, `title`, `year`, `server`, `preprint_doi`
#'   (`NA` if unknown), `arxiv_id` and `osf_id` (`NA` if none), `author` (raw
#'   BibTeX field), `checked` (cached date of the last remote check, or `NA`)
#'   and `row` (row index in `bib`). Attribute `learned_types`: named character
#'   vector (key -> [crossref_type_label()]) of the types found by the Crossref
#'   sweep, to be cached in the bib.
find_preprint_entries <- function(bib, check_unknown_dois = TRUE, mailto = NULL,
                                  cache = NULL) {
  n <- nrow(bib)
  fld <- function(f) {
    v <- vapply(bib$raw, extract_bib_field, character(1), field = f, USE.NAMES = FALSE)
    v[is.na(v)] <- ""
    v
  }
  url <- fld("url"); journal <- fld("journal"); note <- fld("note")
  howpub <- fld("howpublished"); publisher <- fld("publisher")
  eprint <- fld("eprint"); archive <- fld("archiveprefix"); author <- fld("author")
  text <- tolower(paste(journal, howpub, publisher, note, url, archive))
  doi  <- bib$doi

  # (a) DOI-less entries: a preprint DOI hidden in the URL
  #     (e.g. https://www.biorxiv.org/content/10.1101/015800v2.full)
  m <- regexpr("10\\.\\d{4,9}/[^[:space:]?#]+", url, perl = TRUE)
  url_doi <- rep(NA_character_, n)
  url_doi[m > 0] <- regmatches(url, m)
  url_doi <- sub("(\\.(abstract|full|pdf))+/?$", "", url_doi)
  url_doi <- normalize_doi(sub("v\\d+/?$", "", url_doi))
  use_url <- is.na(doi) & is_preprint_doi(url_doi)
  doi[use_url] <- url_doi[use_url]

  # (b) arXiv identifiers (new 1806.05449 or old hep-th/9901001 style), from the
  #     DOI, url, journal or note, or from an eprint field
  arx_re <- paste0("(?i)arxiv(?:\\.org/(?:abs|pdf)/|:\\s*|\\.)",
                   "(\\d{4}\\.\\d{4,5}|[a-z-]+(?:\\.[a-z]{2})?/\\d{7})")
  s  <- paste(ifelse(is.na(doi), "", doi), url, journal, note, howpub)
  mm <- regmatches(s, regexec(arx_re, s, perl = TRUE))
  arxiv_id <- vapply(mm, function(x) if (length(x) >= 2L) x[2] else NA_character_,
                     character(1))
  ep_ok <- is.na(arxiv_id) &
    grepl("^(\\d{4}\\.\\d{4,5}|[a-z-]+(\\.[a-z]{2})?/\\d{7})(v\\d+)?$", eprint,
          ignore.case = TRUE) &
    (archive == "" | grepl("arxiv", archive, ignore.case = TRUE))
  arxiv_id[ep_ok] <- sub("v\\d+$", "", eprint[ep_ok])

  # (c) OSF identifiers (OSF Preprints, SocArXiv, PsyArXiv, ...), used to query
  #     the OSF API: from DOIs like 10.31235/osf.io/5mze4_v1 or URLs like
  #     https://osf.io/preprints/socarxiv/5mze4
  osf_id <- rep(NA_character_, n)
  from_doi <- !is.na(doi) & grepl("^10\\.\\d{4,9}/osf\\.io/", doi)
  osf_id[from_doi] <- sub("^10\\.\\d{4,9}/osf\\.io/", "", doi[from_doi])
  mo <- regmatches(url, regexec("osf\\.io/(?:preprints/[a-z]+/)?([a-z0-9]{5,}(?:_v\\d+)?)",
                                url, perl = TRUE))
  url_osf <- vapply(mo, function(x) if (length(x) >= 2L) x[2] else NA_character_,
                    character(1))
  use_osf <- is.na(osf_id) & !is.na(url_osf) & url_osf != "preprints"
  osf_id[use_osf] <- url_osf[use_osf]

  # (d) Classify
  server <- preprint_server_from_doi(doi)
  no_doi <- is.na(doi)
  arx_only <- no_doi & !is.na(arxiv_id)
  server[arx_only] <- "arXiv"
  doi[arx_only]    <- paste0("10.48550/arxiv.", tolower(arxiv_id[arx_only]))
  txt_server <- preprint_server_from_text(text)
  use_txt <- is.na(server) & no_doi & !is.na(txt_server)
  server[use_txt] <- txt_server[use_txt]

  # (e) Entries whose DOI type is cached in the bib (orcidsync-type)
  cr_type <- bib$cr_type %||% rep(NA_character_, n)
  checked <- bib$checked %||% rep(as.Date(NA), n)
  cached  <- is.na(server) & !is.na(doi) & !is.na(cr_type)
  server[cached & is_preprint_type_label(cr_type)] <- "Other (Crossref preprint)"

  # (f) Optional sweep: ask Crossref about every other DOI
  learned <- character()
  if (check_unknown_dois) {
    cand <- which(is.na(server) & !is.na(doi) & !cached)
    log_msg("Checking ", length(cand), " other DOIs on Crossref for preprint status (",
        sum(cached), " cached in the bib; set check_unknown_dois = FALSE to skip)...")
    for (i in cand) {
      msg <- crossref_cached(doi[i], mailto, cache)
      if (is_crossref_preprint(msg)) {
        inst <- scalar_chr(safe_get(msg, "institution", 1, "name"))
        server[i] <- if (!is.na(inst)) inst else "Other (Crossref preprint)"
      }
      lab <- if (is.null(msg) && exists(doi[i], envir = CROSSREF_NOT_FOUND, inherits = FALSE))
        "not-in-crossref" else crossref_type_label(msg)
      if (!is.na(lab)) learned[[bib$key[i]]] <- lab
    }
  }

  rows <- which(!is.na(server))
  out <- data.frame(key = bib$key[rows], title = bib$title[rows], year = bib$year[rows],
                    server = server[rows], preprint_doi = doi[rows],
                    arxiv_id = arxiv_id[rows], osf_id = osf_id[rows],
                    author = author[rows], checked = checked[rows], row = rows,
                    stringsAsFactors = FALSE)
  attr(out, "learned_types") <- learned
  out
}

#' Extract the "is-preprint-of" DOI from a Crossref record
#'
#' Preprint servers that register DOIs with Crossref (bioRxiv, medRxiv,
#' EarthArXiv, Research Square, OSF, Copernicus, ...) can link the preprint to
#' the journal article in `relation$is-preprint-of`.
#'
#' @param msg Crossref `message` (nested list) or `NULL`.
#' @return The published DOI (normalised) or `NA_character_`.
preprint_relation_doi <- function(msg) {
  rels <- safe_get(msg, "relation", "is-preprint-of", default = list())
  dois <- vapply(rels, function(r) {
    if (identical(tolower(scalar_chr(r[["id-type"]])), "doi")) scalar_chr(r[["id"]])
    else NA_character_
  }, character(1))
  dois <- normalize_doi(dois)
  dois <- dois[!is.na(dois) & !is_preprint_doi(dois)]    # must point to a non-preprint
  if (length(dois)) dois[1] else NA_character_
}

#' Ask the bioRxiv / medRxiv API whether a preprint has been published
#'
#' Uses `https://api.biorxiv.org/details/<server>/<doi>`, whose records carry a
#' `published` field (the journal DOI, or `"NA"`).
#'
#' @param doi Preprint DOI.
#' @param server `"biorxiv"` or `"medrxiv"`.
#' @return The normalised published DOI, or `NA_character_`.
fetch_biorxiv_published <- function(doi, server = "biorxiv") {
  req <- httr2::request(paste0("https://api.biorxiv.org/details/", server, "/", doi))
  req <- httr2::req_user_agent(req, make_user_agent())
  req <- httr2::req_throttle(req, rate = 2)
  req <- httr2::req_retry(req, max_tries = 3)
  req <- httr2::req_error(req, is_error = function(r) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) return(NA_character_)

  coll <- httr2::resp_body_json(resp)[["collection"]] %||% list()
  if (length(coll) == 0L) return(NA_character_)
  # The latest version carries the most up-to-date publication status
  normalize_doi(scalar_chr(safe_get(coll[[length(coll)]], "published")))
}

#' Ask the arXiv API whether a preprint has a journal DOI
#'
#' arXiv DOIs are registered with DataCite, not Crossref, so the Crossref
#' relation check does not work for them. Instead, the arXiv API returns an
#' `<arxiv:doi>` element when the authors have added the journal DOI. The arXiv
#' API asks for at most one request every 3 seconds, which is respected here.
#'
#' @param arxiv_id arXiv identifier, e.g. `"1806.05449"`.
#' @return The normalised journal DOI, or `NA_character_`.
fetch_arxiv_published <- function(arxiv_id) {
  req <- httr2::request("https://export.arxiv.org/api/query")
  req <- httr2::req_url_query(req, id_list = arxiv_id)
  req <- httr2::req_user_agent(req, make_user_agent())
  req <- httr2::req_throttle(req, rate = 1 / 3, realm = "arxiv")
  req <- httr2::req_retry(req, max_tries = 3)
  req <- httr2::req_error(req, is_error = function(r) FALSE)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) return(NA_character_)

  xml <- httr2::resp_body_string(resp)
  m <- regmatches(xml, regexpr("<arxiv:doi[^>]*>[^<]+</arxiv:doi>", xml))
  if (length(m) == 0L) return(NA_character_)
  normalize_doi(gsub("<[^>]+>", "", m))
}

#' Ask the OSF API whether an OSF-hosted preprint has been published
#'
#' OSF hosts OSF Preprints, SocArXiv, PsyArXiv, MetaArXiv, EdArXiv, AfricArXiv
#' and others. Its API (`https://api.osf.io/v2/preprints/<id>/`) exposes the
#' journal DOI that authors add when the paper is published (`article_doi`).
#' Versioned ids (`5mze4_v1`) are tried first, then the unversioned id.
#'
#' @param osf_id OSF preprint id, e.g. `"5mze4_v1"` or `"5mze4"`.
#' @return The normalised journal DOI, or `NA_character_`.
fetch_osf_published <- function(osf_id) {
  for (id in unique(c(osf_id, sub("_v\\d+$", "", osf_id)))) {
    req <- httr2::request(paste0("https://api.osf.io/v2/preprints/", id, "/"))
    req <- httr2::req_user_agent(req, make_user_agent())
    req <- httr2::req_throttle(req, rate = 2, realm = "osf")
    req <- httr2::req_retry(req, max_tries = 3)
    req <- httr2::req_error(req, is_error = function(r) FALSE)

    resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
    if (is.null(resp) || httr2::resp_status(resp) >= 400L) next

    art <- safe_get(httr2::resp_body_json(resp), "data", "attributes", "article_doi")
    d <- normalize_doi(scalar_chr(art))
    if (!is.na(d)) return(d)
  }
  NA_character_
}

#' Look for the published version of a preprint among the works on ORCID
#'
#' Your own ORCID record is a strong source: the published article is usually
#' listed there even when no server links it to the preprint. Two checks:
#' \enumerate{
#'   \item **same ORCID group**: ORCID has grouped the preprint DOI with a
#'     non-preprint DOI (certain);
#'   \item **title**: an ORCID work with a non-preprint DOI, published no earlier
#'     than the preprint, whose title is similar (>= `min_similarity`, using
#'     [title_similarity()], which tolerates rewording). Such candidates are
#'     later confirmed by author overlap in [replace_published_preprints()].
#' }
#'
#' @param p One row of [find_preprint_entries()].
#' @param orcid_works Output of [parse_orcid_works()].
#' @param min_similarity Minimum title similarity for check 2.
#' @return `NULL`, or a list with `doi`, `similarity` and `how`
#'   (`"orcid_group"` or `"orcid_title"`).
match_preprint_to_orcid <- function(p, orcid_works, min_similarity = 0.75) {
  if (is.null(orcid_works) || nrow(orcid_works) == 0L) return(NULL)

  # Candidate published works: a DOI that is not a known preprint DOI, and an
  # ORCID type other than "preprint"
  pub_ok <- !is.na(orcid_works$doi) & !is_preprint_doi(orcid_works$doi) &
    !(orcid_works$type %in% "preprint")

  # (1) Same ORCID group as the preprint DOI (with or without version suffix)
  if (!is.na(p$preprint_doi)) {
    ids <- unique(c(p$preprint_doi, sub("_v\\d+$", "", p$preprint_doi)))
    grp <- orcid_works$group_id[orcid_works$doi %in% ids]
    hit <- which(orcid_works$group_id %in% grp & pub_ok)
    if (length(hit)) return(list(doi = orcid_works$doi[hit[1]], similarity = 1,
                                 how = "orcid_group"))
  }

  # (2) Similar title, not earlier than the preprint
  cand <- which(pub_ok & (is.na(orcid_works$year) | is.na(p$year) |
                            orcid_works$year >= p$year))
  if (!length(cand)) return(NULL)
  sim  <- title_similarity(normalize_title(p$title), normalize_title(orcid_works$title[cand]))
  best <- which.max(sim)
  if (sim[best] < min_similarity) return(NULL)
  list(doi = orcid_works$doi[cand][best], similarity = sim[best], how = "orcid_title")
}

#' Find the published version of a preprint
#'
#' First checks whether ORCID groups the preprint with a published DOI (local
#' and certain, see [match_preprint_to_orcid()]). Then tries, in order, stopping
#' at the first success:
#' \enumerate{
#'   \item **Crossref relation**: the preprint's Crossref record links to the
#'     journal article (`is-preprint-of`). For versioned DOIs (`..._v1`, used by
#'     OSF) the unversioned DOI is tried too.
#'   \item **Server APIs** (`use_server_apis`): bioRxiv/medRxiv API, arXiv API
#'     (when an arXiv id is known) and OSF API (OSF Preprints, SocArXiv,
#'     PsyArXiv, ...).
#'   \item **ORCID** (if `orcid_works` is given): see [match_preprint_to_orcid()].
#'   \item **Title search** (`use_title_search`): a Crossref search for a
#'     non-preprint with a near-identical title, a shared author and a year no
#'     earlier than the preprint (only works if the title did not change).
#' }
#' Candidates from title matching (`"orcid_title"`, `"title_search"`) are
#' confirmed by author overlap in [replace_published_preprints()].
#'
#' @param p One row of [find_preprint_entries()] (a list or one-row data frame).
#' @param mailto Optional e-mail for Crossref.
#' @param cache Optional environment used by [crossref_cached()].
#' @param use_server_apis,use_title_search Enable steps 2 and 4.
#' @param title_min_similarity Minimum title similarity for step 4.
#' @param orcid_works Optional output of [parse_orcid_works()] for step 3.
#' @param orcid_min_similarity Minimum title similarity for step 3.
#' @param skip_remote If `TRUE`, only the ORCID checks are run (steps 1, 2 and
#'   4 are skipped), e.g. because they were run recently.
#' @return A list with `doi` (published DOI or `NA`), `source`
#'   (`"crossref_relation"`, `"biorxiv_api"`, `"arxiv_api"`, `"osf_api"`,
#'   `"orcid_group"`, `"orcid_title"`, `"title_search"` or `NA`), `similarity`
#'   (title similarity for title-based sources, else `NA`) and `checked` (the
#'   steps attempted, as a string).
find_published_version <- function(p, mailto = NULL, cache = NULL,
                                   use_server_apis = TRUE, use_title_search = TRUE,
                                   title_min_similarity = 0.95,
                                   orcid_works = NULL, orcid_min_similarity = 0.75,
                                   skip_remote = FALSE) {
  checked <- character()
  result  <- function(doi, source, similarity = NA_real_) {
    list(doi = doi, source = source, similarity = similarity,
         checked = paste(checked, collapse = ", "))
  }
  doi <- p$preprint_doi

  # 0. The user's own ORCID record: no request needed, so done first; a match
  #    by ORCID group is certain (title matches wait for step 3)
  m <- NULL
  if (!is.null(orcid_works)) {
    checked <- c(checked, "orcid")
    m <- match_preprint_to_orcid(p, orcid_works, orcid_min_similarity)
    if (!is.null(m) && m$how == "orcid_group") return(result(m$doi, m$how, m$similarity))
  }
  if (skip_remote) {
    if (!is.null(m)) return(result(m$doi, m$how, m$similarity))
    return(result(NA_character_, NA_character_))
  }

  # 1. Crossref relation (arXiv DOIs are DataCite DOIs, so skip those);
  #    try the unversioned DOI too (10.31235/osf.io/5mze4_v1 -> .../5mze4)
  if (!is.na(doi) && !grepl(PREPRINT_DOI_PATTERNS[["arXiv"]], doi)) {
    checked <- c(checked, "crossref")
    for (d in unique(c(doi, sub("_v\\d+$", "", doi)))) {
      pub <- preprint_relation_doi(crossref_cached(d, mailto, cache))
      if (!is.na(pub)) return(result(pub, "crossref_relation"))
    }
  }

  # 2. Server-specific APIs
  if (use_server_apis) {
    if (!is.na(doi) && grepl(PREPRINT_DOI_PATTERNS[["bioRxiv/medRxiv"]], doi, perl = TRUE)) {
      checked <- c(checked, "biorxiv_api")
      for (srv in c("biorxiv", "medrxiv")) {
        d <- fetch_biorxiv_published(doi, srv)
        if (!is.na(d) && !is_preprint_doi(d)) return(result(d, "biorxiv_api"))
      }
    }
    if (!is.na(p$arxiv_id)) {
      checked <- c(checked, "arxiv_api")
      d <- fetch_arxiv_published(p$arxiv_id)
      if (!is.na(d) && !is_preprint_doi(d)) return(result(d, "arxiv_api"))
    }
    if (!is.null(p$osf_id) && !is.na(p$osf_id)) {
      checked <- c(checked, "osf_api")
      d <- fetch_osf_published(p$osf_id)
      if (!is.na(d) && !is_preprint_doi(d)) return(result(d, "osf_api"))
    }
  }

  # 3. A similar title on the user's ORCID record (found in step 0)
  if (!is.null(m)) return(result(m$doi, m$how, m$similarity))

  # 4. Crossref title search for a non-preprint version
  if (use_title_search && !is.na(p$title)) {
    checked <- c(checked, "title_search")
    it <- search_crossref_by_title(
      clean_text(gsub("[{}]", "", p$title)),
      author_family  = bib_author_families(p$author),
      mailto         = mailto,
      min_similarity = title_min_similarity,
      exclude_types  = "posted-content",
      min_year       = p$year)
    d <- normalize_doi(scalar_chr(it[["DOI"]]))
    if (!is.na(d) && !is_preprint_doi(d)) {
      if (!is.null(cache)) assign(d, it, envir = cache)   # no need to re-fetch
      return(result(d, "title_search"))
    }
  }
  result(NA_character_, NA_character_)
}

#' Replace preprints in a .bib file by their published versions
#'
#' Covers bioRxiv/medRxiv, arXiv, EarthArXiv, OSF/PsyArXiv/SocArXiv, Research
#' Square, Copernicus Discussions, SSRN and the other servers in
#' `PREPRINT_DOI_PATTERNS`, plus (with `check_unknown_dois`) any DOI that
#' Crossref types as a preprint. Preprints are found with
#' [find_preprint_entries()], their published versions with
#' [find_published_version()]. Then each preprint is:
#' \itemize{
#'   \item **removed** if the published version is already in the bib (same
#'     DOI, or near-identical title in a non-preprint entry);
#'   \item **replaced** by a full Crossref record if the published version is
#'     not in the bib yet;
#'   \item **kept** if no published version was found.
#' }
#'
#' Before any change is written, a time-stamped copy of the original file is
#' made with [backup_bib_file()] (unless `backup = FALSE`). After writing, the
#' file is re-read and the number of entries checked; if it is not what was
#' expected, the backup is restored and an error raised. With `dry_run = TRUE` nothing is written and no
#' backup is made: you only get the plan.
#'
#' Removing or re-keying an entry breaks `\cite{}` commands that use the old
#' key. By default replacements keep the preprint's key, and the `plan` reports
#' for removed preprints which key now represents the work (`new_key`).
#'
#' @param bib_path Path to the .bib file.
#' @param mailto Optional e-mail for the Crossref polite pool.
#' @param keep_key If `TRUE` (default) a replacement entry keeps the preprint's
#'   citation key; if `FALSE` a new key is generated with [make_cite_key()].
#' @param orcid Optional ORCID iD. Strongly recommended: the published version
#'   of a preprint is usually listed on ORCID even when no preprint server links
#'   to it (see [match_preprint_to_orcid()]).
#' @param orcid_works Optional output of [parse_orcid_works()], instead of `orcid`
#'   (avoids downloading it again).
#' @param check_unknown_dois Query Crossref for every DOI not recognised as a
#'   preprint DOI, to catch servers missing from `PREPRINT_DOI_PATTERNS`
#'   (slower: ~2 min for 300 DOIs). See [find_preprint_entries()].
#' @param use_server_apis Use the bioRxiv and arXiv APIs.
#' @param use_title_search Look for published versions by title when no link
#'   exists. See [find_published_version()].
#' @param author_style,container_field Formatting of replacement entries; see
#'   [crossref_to_record()].
#' @param title_threshold Title similarity used to detect a published version
#'   already in the bib under a different/missing DOI.
#' @param orcid_min_similarity Title similarity (see [title_similarity()]) for
#'   matching a preprint to an ORCID work.
#' @param min_author_overlap Candidates found by title (ORCID or Crossref
#'   search) are accepted only if at least this share of the preprint's authors
#'   appear on the published record (see [author_overlap()]).
#' @param recheck_days A preprint whose `orcidsync-checked` date is less than
#'   this many days old is only matched against ORCID; Crossref, the preprint
#'   servers and the title search are skipped. `0` re-checks everything.
#' @param record_checks If `TRUE`, cache results in the bib: the Crossref type
#'   of DOIs looked up in the sweep (`orcidsync-type`) and the date of the last
#'   remote check on preprints still unpublished (`orcidsync-checked`).
#' @param backup_dir Folder for the backup copy (default: next to the bib file).
#' @param dry_run If `TRUE`, report what would change without touching anything.
#' @param backup If `FALSE`, no backup is made here and nothing is restored on
#'   failure: for callers that took a backup themselves (as
#'   [update_bib_from_orcid()] does).
#' @return Invisibly, a list with `plan` (one row per preprint: `key`, `server`,
#'   `preprint_doi`, `action`, `published_doi`, `new_key`, `source`, `note`),
#'   `backup` (path of the backup, `NA` if none was needed or `dry_run`),
#'   `new_entries` (text of the replacement entries) and `cache_updates` (text
#'   of the entries that only gain or update cache fields).
#' @examples
#' \dontrun{
#' # See what would happen first
#' chk <- replace_published_preprints("andrea_manica_pubs.bib",
#'                                    orcid = "0000-0003-1895-450X", dry_run = TRUE)
#' chk$plan
#' # Then do it (a time-stamped backup is created automatically)
#' res <- replace_published_preprints("andrea_manica_pubs.bib", mailto = "you@cam.ac.uk",
#'                                    orcid = "0000-0003-1895-450X")
#' }
replace_published_preprints <- function(bib_path, mailto = NULL, keep_key = TRUE,
                                        orcid = NULL, orcid_works = NULL,
                                        check_unknown_dois = TRUE,
                                        use_server_apis = TRUE,
                                        use_title_search = TRUE,
                                        author_style = "initials",
                                        container_field = "journal",
                                        title_threshold = 0.9,
                                        orcid_min_similarity = 0.75,
                                        min_author_overlap = 0.5,
                                        recheck_days = 30,
                                        record_checks = TRUE,
                                        backup_dir = dirname(bib_path),
                                        dry_run = FALSE, backup = TRUE) {
  cache <- new.env()                     # one Crossref request per DOI per run
  bib <- read_bib_entries(bib_path)

  # ORCID works: the most reliable source for the user's own papers
  if (is.null(orcid_works) && !is.null(orcid)) {
    orcid_works <- tryCatch(parse_orcid_works(fetch_orcid_works(orcid, mailto)),
      error = function(e) {
        warning("Could not read ORCID (", conditionMessage(e),
                "); continuing without it.", call. = FALSE)
        NULL
      })
    if (!is.null(orcid_works)) log_msg("ORCID lists ",
                                   length(unique(orcid_works$group_id)), " works.")
  }
  pre <- find_preprint_entries(bib, check_unknown_dois, mailto, cache)
  log_msg("Found ", nrow(pre), " preprint entries in ", bib_path,
      if (nrow(pre)) paste0(" (", paste(names(table(pre$server)), table(pre$server),
                                        sep = ": ", collapse = ", "), ")"), ".")

  empty_plan <- data.frame(key = character(), server = character(),
                           preprint_doi = character(), action = character(),
                           published_doi = character(), new_key = character(),
                           source = character(), note = character(),
                           stringsAsFactors = FALSE)

  # Entries that are NOT preprints: the only valid "published versions" already
  # in the bib (a preprint's own title must not count as a match).
  published_pool <- bib[setdiff(seq_len(nrow(bib)), pre$row), c("key", "doi", "title_norm")]
  today          <- Sys.Date()
  remote_checked <- character()  # keys of preprints checked remotely this run

  used_keys    <- bib$key
  added        <- character()    # published DOI -> key, for records added this run
  replacements <- character()    # key -> new entry text
  plan_rows    <- list()

  for (i in seq_len(nrow(pre))) {
    p <- pre[i, ]
    k <- p$key
    log_msg("  [", i, "/", nrow(pre), "] ", k, " (", p$server, ": ",
        if (is.na(p$preprint_doi)) "no DOI" else p$preprint_doi, ")")
    row <- list(key = k, server = p$server, preprint_doi = p$preprint_doi,
                action = "keep", published_doi = NA_character_,
                new_key = NA_character_, source = NA_character_, note = "")

    skip_remote <- !is.na(p$checked) && as.numeric(today - p$checked) < recheck_days
    if (!skip_remote) remote_checked <- c(remote_checked, k)
    pv <- find_published_version(p, mailto, cache, use_server_apis, use_title_search,
                                 orcid_works = orcid_works,
                                 orcid_min_similarity = orcid_min_similarity,
                                 skip_remote = skip_remote)

    # Title-based candidates must be confirmed: the record must exist, must not
    # itself be a preprint, and must share enough authors with the preprint.
    rejected <- NULL
    if (!is.na(pv$doi) && pv$source %in% c("orcid_title", "title_search")) {
      cmsg <- crossref_cached(pv$doi, mailto, cache)
      ov   <- if (is.null(cmsg)) NA_real_ else author_overlap(p$author, cmsg[["author"]])
      why  <- if (is.null(cmsg)) "not found in Crossref"
              else if (is_crossref_preprint(cmsg)) "it is itself a preprint"
              else if (is.na(ov) || ov < min_author_overlap)
                sprintf("only %.0f%% of authors shared", 100 * ifelse(is.na(ov), 0, ov))
              else NULL
      if (!is.null(why)) {
        rejected <- sprintf("Candidate %s (%s) rejected: %s", pv$doi, pv$source, why)
        pv$doi <- NA_character_
        pv$source <- NA_character_
      } else {
        pv$overlap <- ov
      }
    }
    row$published_doi <- pv$doi
    row$source        <- pv$source

    if (is.na(pv$doi)) {
      row$note <- if (!is.null(rejected)) rejected else if (skip_remote) {
        paste0("Not on ORCID; other checks skipped (last done ", p$checked, ")")
      } else if (nzchar(pv$checked)) {
        paste0("No published version found (checked: ", pv$checked, ")")
      } else "No DOI, arXiv id or title to check"

    } else if (pv$doi %in% stats::na.omit(published_pool$doi)) {
      # Published version already in the bib with the same DOI
      row$action  <- "remove"
      row$new_key <- published_pool$key[match(pv$doi, published_pool$doi)]
      row$note    <- "Published version already in bib (DOI match)"

    } else if (pv$doi %in% names(added)) {
      # Two preprint entries for the same paper: the first was replaced already
      row$action  <- "remove"
      row$new_key <- added[[pv$doi]]
      row$note    <- "Duplicate preprint; published version added for another entry"

    } else {
      msg <- crossref_cached(pv$doi, mailto, cache)
      if (is.null(msg)) {
        row$note <- "Published DOI could not be retrieved from Crossref; kept preprint"
      } else if (is_crossref_preprint(msg)) {
        row$note <- paste0("Linked DOI ", pv$doi, " is itself a preprint; kept")
        row$published_doi <- NA_character_
      } else {
        rec <- crossref_to_record(msg, author_style, container_field)
        tm  <- best_title_match(normalize_title(rec$title), published_pool$title_norm,
                                published_pool$key)

        if (!is.na(tm$similarity) && tm$similarity >= title_threshold) {
          row$action  <- "remove"
          row$new_key <- tm$key
          row$note    <- sprintf("Published version already in bib (title match %.2f)",
                                 tm$similarity)
        } else {
          new_key <- if (keep_key) k else
            make_cite_key(rec$first_family, rec$year, rec$title, setdiff(used_keys, k))
          used_keys <- c(used_keys, new_key)
          replacements[[k]] <- format_bibtex_entry(new_key, rec$bibtype, rec$fields)
          added[[pv$doi]]   <- new_key
          row$action  <- "replace"
          row$new_key <- new_key
          cont <- rec$fields[names(rec$fields) %in% c("journal", "booktitle")]
          row$note <- paste0("Replaced by published version",
                             if (length(cont) && !is.na(cont[1])) paste0(" (", cont[1], ")") else "")
        }
      }
    }
    if (row$action != "keep" && pv$source %in% c("orcid_title", "title_search")) {
      row$note <- sprintf("%s [found by %s: title similarity %.2f, %.0f%% authors shared; please verify]",
                          row$note, sub("_", " ", pv$source),
                          ifelse(is.na(pv$similarity), 1, pv$similarity), 100 * pv$overlap)
    }
    plan_rows[[i]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }
  plan <- if (length(plan_rows)) do.call(rbind, plan_rows) else empty_plan

  remove_keys <- plan$key[plan$action == "remove"]

  # ---- Cache fields for entries that are otherwise left as they are ----------
  cache_updates <- character()
  if (record_checks) {
    learned <- attr(pre, "learned_types")
    kept    <- intersect(remote_checked, plan$key[plan$action == "keep"])
    for (k in setdiff(union(names(learned), kept), c(remove_keys, names(replacements)))) {
      entry <- bib$raw[match(k, bib$key)]
      if (k %in% names(learned)) entry <- set_bib_field(entry, TYPE_FIELD, learned[[k]])
      if (k %in% kept)           entry <- set_bib_field(entry, CHECKED_FIELD, format(today))
      cache_updates[[k]] <- entry
    }
  }
  log_msg("Plan: ", sum(plan$action == "replace"), " to replace, ",
      length(remove_keys), " to remove, ", sum(plan$action == "keep"), " kept; ",
      length(cache_updates), " entries get updated cache fields.")

  out <- list(plan = plan, backup = NA_character_, new_entries = replacements,
              cache_updates = cache_updates)
  if (length(remove_keys) == 0L && length(replacements) == 0L &&
      length(cache_updates) == 0L) {
    log_msg("Nothing to change; bib file untouched.")
    return(invisible(out))
  }
  if (dry_run) {
    log_msg("Dry run: no changes written.")
    return(invisible(out))
  }

  # ---- Backup FIRST (unless the caller made one), then modify, then verify ---
  if (backup) {
    out$backup <- backup_bib_file(bib_path, backup_dir)
    log_msg("Backup written to ", out$backup)
  }

  rewrite_bib_entries(bib_path, remove_keys, c(replacements, cache_updates))
  verify_entry_count(bib_path, nrow(bib) - length(remove_keys), out$backup)
  n_now <- nrow(bib) - length(remove_keys)
  log_msg("Updated ", bib_path, ": ", nrow(bib), " -> ", n_now, " entries.")
  invisible(out)
}
