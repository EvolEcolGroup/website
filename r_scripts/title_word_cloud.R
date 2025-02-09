# create a word cloud from publication titles
# following: https://remybeugnon.netlify.app/post/extract-my-scientific-profile-with-r/

pub <- read.csv("./people/andrea_manica_pubs.csv")

library("tm")
library("SnowballC")
library("wordcloud2")
library(dplyr)

# List of words to remove
list.stop = c('affect', 'effects', 'using')

docs <- Corpus(VectorSource(pub$title))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- docs %>%
  tm_map(., toSpace, "/") %>%
  tm_map(., toSpace, "@") %>%
  tm_map(., toSpace, "\\|") %>%
  tm_map(., content_transformer(tolower)) %>%
  tm_map(., removeNumbers) %>%
  tm_map(., removeWords, stopwords("english")) %>%
  tm_map(., removeWords, list.stop) %>%
  tm_map(., removePunctuation) %>%
  tm_map(., stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

col.pal = colorRampPalette(colors = c('brown','Darkgreen'))

# plot it with worldcloud2
wordcloud2(data = d, 
           size = 1, 
           color = col.pal(10),
           widgetsize = c(1018, 400),)

# # And now plot it
# library(ggwordcloud)
# 
# ggplot(data = d, 
#        aes(label = word, size = freq, col = as.character(freq))) + 
#   geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
#                       grid_size = 1, eccentricity = .9)+
#   scale_size_area(max_size = 14)+
#   scale_color_brewer(palette = "Paired", direction = -1)+
#   theme_void()

