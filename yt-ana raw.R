
--------------------- Red dislikes overlaid on green? -------------

# The data points follow logarithmic curves, which may suggest that the first viewers are the type to react: subscribers and those who seek out that content.
# Videos with higher view counts have a smaller proportion of viewer reactions, which may that the additional viewers are people outside of the target audience, who are less likely to react.
 
ggplot(data=yt) + geom_jitter(mapping = aes(x = views, y = likes), color="green") + geom_jitter(mapping = aes(x = views, y = dislikes), color="red")
#ggplot(data=yt)
+ geom_jitter(mapping = aes(x = views, y = comment_count, color="blue"))

# Q: Which categories are the most popular?
# A: 1) Entertainment, 2) Music, 3) How-to & Style, 4) Comedy, 5) People & Blogs
# Entertainment is the most popular by far (7281)
yt_by_category <- group_by(yt, category_id) %>% summarise (category_count = n()) %>% select(category_id,category_count)
yt_by_category %>% arrange(desc(category_count))
yt_top_cats <- yt_by_category %>% arrange(desc(category_count)) %>% .[1:5,] %>% rbind(c(0, other_categories$other_count))


# Q: Do controversial videos (high Dislike:Like ratio) get more comments?
# A: As D:L approaches ~1 (which is relatively high), comments increase drastically. These videos probably generate the most controversy and argument.
# However, videos with D:L > 2 seem to taper off in comment count. This may suggest that most viewers universally dislike the content, dislike it, and move on.
# Let's zoom up on D:L between 1-5.
filter(yt, comment_count > 10000) %>% ggplot + geom_jitter(mapping = aes(x = dislikes/likes, y = comment_count))

# Q: Does having a long description increase your views?
# A: No. The most viewed videos seem to have a description of < 1000 characters. 
ggplot(data=yt) + geom_point(mapping = aes(x=nchar(description), y=views))

# Q: Does having a long description correlate with less views?
# A: No. Videos with more descriptions longer than 3000 characters sometimes achieve up to 10M views.
filter(yt, nchar(description) > 3000) %>% ggplot + geom_point(mapping = aes(x=nchar(description), y=views))


------------------^^ ignore: --------------
  --------------- to do: ------------------


# -------------------------------------

set.seed(23)
# Wordcloud helper func
prep_wordcloud <- function(wordlist) {
  corpus <- Corpus(VectorSource(list(wordlist)))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  data.frame(word = names(v),freq=v)
}

# What are the most common words in the titles of trending videos?
cloud_titles <- prep_wordcloud(yt$title)
wordcloud(words = cloud_titles$word, freq = cloud_titles$freq,
          max.words=100, random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))

# What are the most common tags in trending videos?
cloud_tags <- prep_wordcloud(yt$tags)
wordcloud(words = cloud_tags$word, freq = cloud_tags$freq, scale=c(3,.25),
          max.words=100, random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))