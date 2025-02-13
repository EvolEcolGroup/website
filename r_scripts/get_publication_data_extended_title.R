get_publication_data_extended_title <- function(id, pub_id, flush = FALSE) {
  # ensure tidy_id
  id <- scholar:::tidy_id(id)
  #debug
  #id <- "K6EVDoYAAAAJ"
  #pub_id <- "HIFyuExEbWQC"
  
  ## Define the cache path
  cache.dir <- file.path(tempdir(), "r-scholar")
  R.cache::setCacheRootPath(cache.dir)
  
  ## Clear the cache if requested
  if (flush) R.cache::saveCache(NULL, key=list(id, pub_id, "data"))
  
  ## Check if we've cached it already
  data <- R.cache::loadCache(list(id, pub_id, "data"))
  
  site <- getOption("scholar_site")
  
  ## If not, get the data and save it to cache
  if (is.null(data)) {
    
    url_template <- paste0(site, "/citations?view_op=view_citation&hl=en&user=%s&citation_for_view=%s")
    url <- sprintf(url_template, id, paste0(id,":",pub_id))
    
    page <- scholar:::get_scholar_resp(url)
    if (is.null(page)) return(NA)
    
    page <- page %>% rvest::read_html()
    
    title <- page %>% rvest::html_nodes(xpath="//a[@class='gsc_oci_title_link']") %>% rvest::html_text()
    
    fields <- page %>% rvest::html_nodes(xpath="//div[@class='gsc_oci_field']") %>% rvest::html_text()
    field_num <- stringr::str_which(fields, "Publication date")
    data_fields <- page %>% rvest::html_nodes(xpath="//div[@class='gsc_oci_value']") %>% rvest::html_text()
    
    names(data_fields) <- fields
    data <- as.data.frame(t(data_fields))
    data$Title <- title
  }
  
  return(data)
  
}
