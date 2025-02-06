# create a word cloud from publication titles
# using https://semba-blog.netlify.app/11/05/2019/wordclouds-plotting-with-ggwordcloud-package-in-r/
# as a template
library(ggwordcloud)
library(tm)
pubs <- read.csv("./people/andrea_manica_pubs.csv")

ggplot(pubs, aes(label = title)) +
  geom_text_wordcloud() +
  theme_minimal()

# create a corpus object
pubs_corpus = pubs$title  %>% 
  tm::VectorSource()%>% 
  tm::Corpus()

# clean it up
pubs_corpus_clean =  pubs_corpus %>% 
  tm_map(FUN = content_transformer(tolower)) %>% # Convert the text to lower case
  tm_map(FUN = removeNumbers) %>% # Remove numbers
  tm_map(removeWords, stopwords("english")) %>% # Remove english common stopwords
#  tm_map(removeWords, c("will", "let", "ring")) %>%   # Remove words 
  tm_map(removePunctuation) %>%   # Remove punctuations
  tm_map(stripWhitespace)   # Eliminate extra white spaces

# create word frequency table (this needs fixing, as it uses the first column as freq, which is not)
# We need to compute rowsums for the matrix that we obtain from the TermDocumentMatrix
# TODO fix bug
pubs_corpus_clean_tb=  pubs_corpus_clean %>% 
  tm::TermDocumentMatrix() %>% 
  as.matrix() %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  dplyr::rename(word = 1, freq = 2) %>%
  dplyr::arrange(desc(freq))

# And now plot it
ggplot(data = pubs_corpus_clean_tb, 
       aes(label = word, size = freq, col = as.character(freq))) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 14)+
  scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()
