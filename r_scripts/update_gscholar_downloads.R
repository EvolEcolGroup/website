suppressPackageStartupMessages(library(dplyr))
source("./r_scripts/papers_functions.R")
source("./r_scripts/update_people_metadata.R")

################################################################################
## query google scholar to update publications for every group member which is still part of the group
################################################################################

# loop over group members and, for those who have a gscholar_id, update their publications
for (i in seq_len(nrow(qmd_data))){
  if (!is.na(qmd_data$gscholar_id[i])){
    # read the group publications already available
    group_pubs <- read.csv("papers/group_pubs.csv", stringsAsFactors = FALSE)
    author_id <- qmd_data$gscholar_id[i]
    # get the full list of publications
    new_pubs <- scholar::get_publications(author_id)
    if (!inherits(new_pubs, "data.frame") || nrow(new_pubs) == 0){
      # message(paste0("No publications found for ", qmd_data$papers_name[i], " (", author_id, ")"))
      next
    }
    # read their previous publications (if they exist)
    if (file.exists(qmd_data$file_csv[i])){
      old_pubs <- read.csv(qmd_data$file_csv[i], stringsAsFactors = FALSE)
      # find the new publications
      new_pubs <- new_pubs[!new_pubs$pubid %in% old_pubs$pubid,]
      # the cid column can end up formatted in strange way, cast to character
      old_pubs$cid <- as.character(old_pubs$cid)
      new_pubs$cid <- as.character(new_pubs$cid)
    } else { # if there are no previous publications, create an empty data.frame
      old_pubs <- new_pubs[0,]
    }
    # if there are any new publications
    if (nrow(new_pubs) > 0){
      # get full info for the new publications
      new_pubs <- pubs_get_all_authors(new_pubs, author_id = author_id)
      # get url
      new_pubs <- pubs_get_all_urls(new_pubs, author_id = author_id)
      # add the new publications to the old ones
      all_pubs <- dplyr::bind_rows(old_pubs, new_pubs)
      # save the new publications
      write.csv(all_pubs, qmd_data$file_csv[i], row.names = FALSE)
      # if any of the publications matches the year range
      new_pubs <- dplyr::filter(new_pubs, year >= qmd_data$group_start[i] & year <= qmd_data$papers_end[i])
      # and they dont' exist in group publications (based on title)
      new_pubs <- dplyr::anti_join(new_pubs, group_pubs, by = "title")
      if (nrow(new_pubs) > 0){
        # the cid column can end up formatted in strange way, cast to character
        old_pubs$cid <- as.character(old_pubs$cid)
        new_pubs$cid <- as.character(new_pubs$cid)
        group_pubs <- dplyr::bind_rows(group_pubs, new_pubs)
        # update the group publications csv
        write.csv(group_pubs, "papers/group_pubs.csv", row.names = FALSE)
      }
    }
  }
}
