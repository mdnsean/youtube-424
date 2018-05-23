
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

# ----------------------------------------------------------
# Q: How quickly do videos start to trend?
yt <- mutate(yt,publish_date=as.Date(publish_time), trending_as_date=as.Date(trending_date, "%y.%d.%m"))
yt <- mutate(yt, days_to_trend=as.integer(trending_as_date - publish_date))

# A: Most videos start trending after about a week, although a handful take up to a few years.

yt %>% ggplot + geom_density(mapping=(aes(x=log10(days_to_trend+1))), adjust=2) + geom_vline(aes(xintercept=log10(7+1)), color="blue", linetype="dashed") + annotate("text", x=1, y=.25, label="7 days", angle=90, color="blue")

# Close up of videos that trend within a week; a few videos trend the same day they are uploaded.
yt %>% filter(days_to_trend <= 7) %>% ggplot + geom_bar(mapping=(aes(x=days_to_trend)))

# Q: How long do they keep trending?
yt <- mutate(yt, days_trending = as.integer(as.Date("2018-04-24", "%Y-%m-%d") - trending_as_date))
yt %>% ggplot + geom_histogram(mapping=aes(x=days_trending, alpha=..count..), binwidth=10, center=5, color="black") + scale_x_continuous(breaks=seq(0,180,20)) + theme(legend.position="none")

# Q: What about the videos trending today?
# A: Some videos uploaded in the early 2000s are still trending today, 
# but the vast majority of today's trending videos were uploaded within the past 12 months.
ggplot(yt) + geom_freqpoly(mapping=aes(x=publish_time)) +
  scale_x_datetime(limits = c(as.POSIXct("2005-01-01"), as.POSIXct("2016-12-31")))

ggplot(yt) + geom_freqpoly(mapping=aes(x=publish_time)) +
  scale_x_datetime(date_breaks="2 months", date_labels="%b", limits = c(as.POSIXct("2017-04-25"), as.POSIXct("2018-04-25"))) + labs(x="Uploaded in 2018")

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