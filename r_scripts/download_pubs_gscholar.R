# Strategy
# download all publications of a google scholar profile
# check if any are new (not in the existing bibtex file)
# if they are, get additional information (i.e. all authors)
# save the new publications as a bibtex file



# get all publications
pubs <- scholar::get_publications("v8V058QAAAAJ")

library(dplyr)
# function to get all authors of a set of publications downloaded with get_publications
# from https://github.com/jkeirstead/scholar/issues/122
get_pubs_all_authors <- function(pubs, author_id, delay = 0.8) {
  df1 <- pubs %>%
    dplyr::mutate(id = author_id)
  df2 <- df1 %>%
    dplyr::filter(stringr::str_detect(author, "\\.\\.\\."))
  browser()
  df3 <- df2 %>%
    dplyr::mutate(complete_authors = purrr::map2(id, pubid, scholar::get_complete_authors, delay = 0.8, initials = TRUE))
  df4 <- df3 %>%
    dplyr::select(pubid, complete_authors) %>%
    dplyr::mutate(complete_authors = base::unlist(complete_authors))
  df5 <- dplyr::left_join(df1, df4,
                          by = dplyr::join_by(pubid)) %>%
    dplyr::mutate(authors = dplyr::case_when(is.na(complete_authors) ~ author, .default = complete_authors)) %>%
    dplyr::select(-author, -complete_authors) |>
    dplyr::rename(author = authors) |>
    dplyr::relocate(title, author)
  return(df5)
}

# get all authors of the publications
pubs_all <- get_pubs_all_authors(pubs, author_id = "v8V058QAAAAJ")

# save these as a bibtex file
# Convert to BibTeX format
# TODO this fails as, if we don't have a journal, we get an error

library(RefManageR)
bib_entries <- lapply(1:nrow(pubs_all), function(i) {
  with(pubs_all[i, ], bibentry(bibtype = "article",
                           key = paste("scholar", i, sep = "_"),
                           author = author,
                           title = title,
                           journal = journal,
                           year = year))#,
                           #cid=cid,
                           #pubid=pubid,
                           #cites=cites))
})

# Create a BibTeX file
WriteBib(bib_entries, file = "publications.bib")
