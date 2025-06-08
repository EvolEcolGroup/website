suppressPackageStartupMessages(library(dplyr))
source("./r_scripts/papers_functions.R")
################################################################################
## collect all metadata about group members present and past
################################################################################

# get a list of qmd files from the people directory
qmd_files <- list.files("people", pattern = ".qmd$", full.names = TRUE)
# remove the template file
qmd_files <- qmd_files[-grep("template", qmd_files)]
# create a data.frame to store the information
qmd_data <- data.frame(file_csv = paste0(tools::file_path_sans_ext(qmd_files),"_pubs.csv"),
                       gscholar_id = rep(NA, length(qmd_files)),
                       group_start = rep(NA, length(qmd_files)),
                       papers_name =rep(NA, length(qmd_files)))
# loop over the files and extract the appropriate metadata
for (i in seq_len(length(qmd_files))){
  # read the file
  lines <- readLines(qmd_files[i])
  get_meta_field <- function(field, lines){
    line_with_meta <- grep(field, lines)
    if (length(line_with_meta) == 0) {return(NA)}
    field <- lines[grep(field, lines)]
    field <- gsub(paste0(field, ": "), "", field)
    field <- gsub('.*"(.*)".*', '\\1', field)
    return(field)
  }
  # get the gscholar_id
  qmd_data$gscholar_id[i] <- get_meta_field("^gscholar_id", lines)
  qmd_data$group_start[i] <- get_meta_field("^group_start", lines)
  qmd_data$papers_name[i] <- get_meta_field("^papers_name", lines)
}

# Now add cosupervised and alumni
cosup_yaml <- yaml::read_yaml("people/cosupervised.yaml")
cosup_df <- df <- do.call(dplyr::bind_rows, lapply(cosup_yaml, as.data.frame))
alumni_yaml <- yaml::read_yaml("people/alumni/alumni.yaml")
alumni_df <- df <- do.call(dplyr::bind_rows, lapply(alumni_yaml, as.data.frame))

# combine the data
qmd_data <- dplyr::bind_rows(qmd_data, cosup_df, alumni_df)

# if missing papers_end, use this year
qmd_data$papers_end[is.na(qmd_data$papers_end)] <- 2025 # ideally do this dynamically

# if missing group_start, use the min of group_start
# this is a hack until all group_start dates have been filled
# TODO fill in all group_start in the actual documents
qmd_data$group_start[is.na(qmd_data$group_start)] <- min(qmd_data$group_start, na.rm = TRUE)
# save the metadata for later use
write.csv(qmd_data,"./papers/people_metadata.csv")

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
