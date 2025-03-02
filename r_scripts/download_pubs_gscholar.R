# define author_id on google scholar
author_id <- "v8V058QAAAAJ"
author_name_surname <- "andrea_manica"

# get functions
source("./r_scripts/papers_functions.R")
library(dplyr)

# check if we already have a download of the publications
pubs_csv <- file.path("./people/",paste0(author_name_surname, "_pubs.csv"))
if (file.exists(pubs_csv)){
  pubs <- read.csv(pubs_csv, stringsAsFactors = FALSE)
} else {
  pubs <- NULL
}

# download all publications of a Google scholar profile
# NOTE that sort_by = "year" should NOT be used, as it is buggy for profiles with many publications
new_pubs <- scholar::get_publications(author_id)

# merge new and old publications, only adding new pubs that are not already in pubs
if (!is.null(pubs)){
  # use anti_join to get only the new publications
  new_pubs <- dplyr::anti_join(new_pubs, pubs, by = "pubid")
  pubs <- dplyr::bind_rows(pubs, new_pubs)
} else {
  pubs <- new_pubs
}

# for any publication that does not have all authors, complete the list
pubs_all <- pubs_get_all_authors(pubs, author_id = author_id)

# for any publication that does not have a url, get the url
pubs_all <- pubs_get_all_urls(pubs_all, author_id = author_id)

# save the resulting data.frame
write.csv(pubs_all, pubs_csv, row.names = FALSE)

