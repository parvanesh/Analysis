library(tidyverse)
library(tidytext)
library(twitteR)
library(dplyr)
library(wordcloud)

#set your account configuration
setup_twitter_oauth(consumer_key, 
                    consumer_secret,
                    access_token, 
                    access_secret)

#search for GDPR tweets , the latest 5000 and searching the tweets in english
tweets <- searchTwitter("GDPR", lang = "en" , n = 5000) %>%
  twListToDF() %>%
  select(id, text)

#remove dupliated tweets
tweets <- subset(tweets, !duplicated(tweets$text))
#convert tweets to token
tokens <- tweets %>% unnest_tokens(word, text)
#remove stop words
tokens <- tokens %>% dplyr::anti_join(stop_words)
#remove other stop words
tokens = tokens[!(tokens$word %in% c("http","https","to","t.co","rt","amp","gdpr",
                                     c(0:9),"it's","doesn't","t.c","ed","00a0","00be","00b8",
                                     "00b7","00bd","00b4","00bc")),]
#plot the frequent words
tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill="blue") +
  xlab(NULL) + coord_flip() + ggtitle("Most common words") + ylab("Word frequency")

#extract bing sentiment and merge it with tweets
sentiment_bing <- tokens %>%
  inner_join(get_sentiments("bing"))

#get top freqent sentiment words 
res = sentiment_bing %>%
  dplyr::group_by(sentiment) %>%
  dplyr::count(word) %>%
  dplyr::filter(n > 5 & !(word %in% c("cloud","plot")))

#plot the sentiment graph
ggplot(aes(reorder(word, n),n, fill = sentiment), data = res) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  ggtitle("Sentiments of tweets")

# get nrc dictionary and merge it with tweets
sentiment_nrc <- tokens %>%
  inner_join(get_sentiments("nrc"))

#plot sentiment graph
res <- sentiment_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count))
ggplot(aes(sentiment, word_count),data =res) +
  geom_col() +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Word frequency") +
  ggtitle("NRC sentiment of tweets") +
  coord_flip()
#create world cloud
tokens %>%
  dplyr::anti_join(stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n, max.words = 100))

