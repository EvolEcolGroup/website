suppressPackageStartupMessages(library(dplyr))

# get a list of qmd files from the people directory
qmd_files <- list.files("people", pattern = ".qmd$", full.names = TRUE)
# remove the template file
qmd_files <- qmd_files[-grep("template", qmd_files)]
# create a data.frame to store the information
qmd_data <- data.frame(file_csv = paste0(tools::file_path_sans_ext(qmd_files),"_pubs.csv"),
                       gscholar_id = rep(NA, length(qmd_files)),
                       eeg_start = rep(NA, length(qmd_files)),
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
  qmd_data$gscholar_id[i] <- get_meta_field("gscholar_id", lines)
  qmd_data$eeg_start[i] <- get_meta_field("eeg_start", lines)
  qmd_data$papers_name[i] <- get_meta_field("papers_name", lines)
}

# Now add cosupervised and alumni
cosup_yaml <- yaml::read_yaml("people/cosupervised.yaml")
cosup_df <- df <- do.call(dplyr::bind_rows, lapply(cosup_yaml, as.data.frame))
alumni_yaml <- yaml::read_yaml("people/alumni/alumni.yaml")
alumni_df <- df <- do.call(dplyr::bind_rows, lapply(alumni_yaml, as.data.frame))

# combine the data
qmd_data <- dplyr::bind_rows(qmd_data, cosup_df, alumni_df)

# if missing eeg_end, use this year
qmd_data$eeg_end[is.na(qmd_data$eeg_end)] <- 2025 # ideally do this dynamically

# if missing eeg_start, use the min of eeg_start
# this is a hack until all eeg_start dates have been filled
# TODO fill in all eeg_start
qmd_data$eeg_start[is.na(qmd_data$eeg_start)] <- min(qmd_data$eeg_start, na.rm = TRUE)

# read the group publications already available
group_pubs <- read.csv("papers/group_pubs.csv", stringsAsFactors = FALSE)
# TODO as a temporary hack, read in the first 10 lines of Andrea's papers
andrea_pubs <- read.csv("people/andrea_manica_pubs.csv", stringsAsFactors = FALSE, nrows = 10)
