suppressPackageStartupMessages(library(dplyr))
#' Get all authors of a set of publications downloaded with get_publications
#' 
#' This function is a modified version of the one described in
#'  https://github.com/jkeirstead/scholar/issues/122
#' 
#' @param pubs A data frame of publications downloaded with get_publications
#' @param author_id The author id of the publications
#' @param delay The delay between requests
#' @return A data frame with all authors of the publications
#' 

pubs_get_all_authors <- function(pubs, author_id, delay = 0.8) {
  # make sure that we don't have duplicated pubids
  pubs_unique <- dplyr::distinct(pubs, pubid, .keep_all = TRUE)
  df1 <- pubs_unique %>%
    dplyr::mutate(id = author_id)
  df2 <- df1 %>%
    dplyr::filter(stringr::str_detect(author, "\\.\\.\\."))
  # if there were no authors with ellipsis, return the original data frame
  if (df2 %>% nrow() == 0) {
    return(df1)
  }
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


#' Get download URL for a set of publications downloaded with get_publications
#' 
#' @param pubs A data frame of publications downloaded with get_publications
#' @param author_id The author id of the publications
#' @param delay The delay between requests
#' @return A data frame with all authors of the publications


pubs_get_all_urls <- function(pubs, author_id, delay = 0.8) {
  # if the url column does not exist yet, add it and set all to NA
  if (!"url" %in% colnames(pubs)) {
    pubs$url <- NA
  }
  # TODO get indeces of lines that have na for url
  min_delay = delay - 0.5
  max_delay = delay + 0.5
  if (min_delay < 0) 
    min_delay <- 0
  if (delay == 0) 
    max_delay <- 0
  for (i in 1:nrow(pubs)) {
    if (is.na(pubs$url[i])) {
      delay <- sample(seq(min_delay, max_delay, by = 0.001), 
                      1)
      Sys.sleep(delay)
      pubs$url[i] <- scholar::get_publication_url(author_id, pubs$pubid[i])
    }
  }
  return(pubs)
}

#' Format publications for a markdown/quarto file
#' 
#' This function is a modified version of scholar::format_publication. TODO author.name
#' should allow for a data.frame of multiple authors with a start and end year for
#' highlighting
#' 
#' @param pubs A data frame of publications downloaded with get_publications
#' @param author.name The name of the author to highlight
#' @return A character vector with the formatted publications
#' 
pubs_format_publications <- function (pubs, author.name = NULL) 
{
  pubs2 <- pubs %>% strsplit(x = .$author, split = ",")
  pubs$author <- lapply(pubs2, function(x) {
    x <- scholar:::swap_initials(x)
    x[length(x)] <- paste0("& ", x[length(x)])
    x <- paste0(x, collapse = ", ")
    ifelse(startsWith(x, "& "), sub("& ", "", x), x)
  })

  res <- pubs %>% 
    arrange(desc(.data$year)) %>% 
    mutate(journal = paste0("*", .data$journal, "*"), Publications = paste0(.data$author, 
                                 " (", .data$year, "). ", .data$title, ". ", .data$journal, 
                                 ". ", .data$number)) %>% pull(.data$Publications)
  author.name2 <- scholar:::swap_initials(author.name)  
  if (is.null(author.name2)) 
    return(res)
  gsub(author.name2, paste0("**", author.name2, "**"), res)
}

#' Function to format a publication list
#' 
#' Take a data.frame with information on publications and, optionally, a data.frame
#' of authors to highlight, and return a fully formatted bibliography
#' 
#' @param pubs a data.frame as produced by `download_pubs_gscholar`
#' @param authors_metadata either a single string (for a single author), or 
#' a data.frame of authors details. The key column is papers_name (which can
#' consist of multiple names, separated by a ;, in the format AN Other)
#' @param with_url_link if TRUE, include a link to the publication
#' @returns a full bibliography as a single string

format_publication_list <- function(pubs, authors_metadata, with_url_link = TRUE) {
  pubs2 <- pubs %>% strsplit(x = .$author, split = ",")
  pubs$author <- lapply(pubs2, function(x) {
    x <- scholar:::swap_initials(x)
    x[length(x)] <- paste0("& ", x[length(x)])
    x <- paste0(x, collapse = ", ")
    ifelse(startsWith(x, "& "), sub("& ", "", x), x)
  })
  # now highlight the authors
  # if we have a single author
  if (inherits(authors_metadata, "character")){
    author.name2 <- scholar:::swap_initials(authors_metadata)
    pubs$author <- gsub(author.name2, paste0("**", author.name2, "**"), pubs$author)
  } else { # if we have multiple authors
    # if we have multiple names in the same line, create new lines with unique names
    semicolon_index <- grep(";", authors_metadata$papers_name)
    # if there is a semicolon in the papers_name, split the string by semicolon and create multiple rows for each author
    if (length(semicolon_index) > 0) {
      authors_metadata <- authors_metadata %>%
        mutate(papers_name = strsplit(papers_name, ";")) %>%
        tidyr::unnest(papers_name)
    }
    # remove leading and trailing whitespaces
    authors_metadata$papers_name <- trimws(authors_metadata$papers_name)    

    for (i in seq_len(nrow(authors_metadata))){ # for each author
      if (!is.na(authors_metadata$papers_name[i])){ # skip if there is no name for this group member
        author.name2 <- scholar:::swap_initials(authors_metadata$papers_name[i])
        start_year <- as.integer(authors_metadata$group_start[i])
        end_year <- as.integer(authors_metadata$papers_end[i])
        pubs$author[(pubs$year >= start_year) & (pubs$year <= end_year)] <-
          gsub(author.name2, paste0("**", author.name2, "**"),pubs$author[(pubs$year >= start_year) & (pubs$year <= end_year)], fixed = TRUE)
      }
      
    }
  }
  # format link
  if (with_url_link){
    pubs$url_link <- ifelse(is.na(pubs$url), "", paste0('[{{< fa link>}}]("', pubs$url, '")'))
  } else {
    pubs$url_link <- ""
  }

  res <- pubs %>% arrange(desc(.data$year)) %>% mutate(journal = paste0("*", 
                                                                        .data$journal, "*"), Publications = paste0(.data$author, 
                                                                                                                   " (", .data$year, "). ", .data$title, ". ", .data$journal, 
                                                                                                                   ". ", .data$number,.data$url_link)) %>% pull(.data$Publications)
  return(res)
}



