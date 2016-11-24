library(tidytext)
library(scales)
library(topicmodels)
library(tidyverse)
library(sentimentr)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(reshape2)
library(knitr)
library(broom)
library(pander)
data(stop_words)
nrc <- get_sentiments("nrc")

# Read data, adjust encoding
data_raw <- read_csv("data/df_cb_main.csv") %>%
  mutate(Review = iconv(Review, "ASCII", "UTF-8"))

# Tokenize, remove stop words
text_tidy <- data_raw %>%
  transmute(GameTitle = `Game Title`, Review) %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words, by = "word")

# Text cloud - all
text_count <- text_tidy %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 200, colors = brewer.pal(8, "Dark2"), random.order = FALSE))

## Tf_idf
text_Tfidf <- text_tidy %>%
group_by(GameTitle) %>%
count(word, sort = TRUE) %>%
bind_tf_idf(word, GameTitle, n) %>%
ungroup() %>%
distinct(word, .keep_all = TRUE) %>%
arrange(tf_idf) %>%
mutate(word = factor(word, levels = unique(word)))

ggplot(text_Tfidf[1 : 20,], aes(word, tf_idf)) +
geom_bar(alpha = 0.8, stat = "identity") +
labs(title = "Highest tf-idf words in All Game Reviews",
x = NULL, y = "tf-idf") +
coord_flip()

# Sentiment by frequency
text_sent <- text_tidy %>%
inner_join(nrc, by = "word") %>%
filter(sentiment != "positive", sentiment != "negative")

reorder_bysize <- function(x) {
factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(text_sent, aes(reorder_bysize(sentiment))) +
geom_bar(fill = rep(c("salmon", "skyblue"), 4)) +
labs(title = "Sentiment Intensity (by sentiment word count)",
x = "Sentment",
y = "Sentiment word count")

# Sentiment words by frequency
text_sentCount <- text_sent %>%
group_by(sentiment) %>%
count(word) %>%
top_n(5, n)

# Positive emotional words
text_sentCountPo <- text_tidy %>%
count(word, sort = TRUE) %>%
inner_join(nrc, by = "word") %>%
filter(sentiment %in% c("anticipation", "joy", "surprise", "trust")) %>%
acast(word ~ sentiment, value.var = "n", fill = 0) %>%
comparison.cloud(title.size = 1.5, max.words = 200)

# Negative emotional words
text_sentCountNe <- text_tidy %>%
count(word, sort = TRUE) %>%
inner_join(nrc, by = "word") %>%
filter(sentiment %in% c("anger", "disgust", "fear", "sadness")) %>%
acast(word ~ sentiment, value.var = "n", fill = 0) %>%
comparison.cloud(title.size = 1.5, max.words = 200)

# High frequency word sieve
wordSieve = c("game", "games", "game's", "play", "feel", "time", "experience", "makes")

# Apply DTM model
text_dtm <- text_tidy %>%
filter(!word %in% wordSieve) %>%
group_by(GameTitle) %>%
count(word)

model_dtm <- text_dtm %>%
cast_dtm(GameTitle, word, n) %>%
LDA(k = 4, control = list(seed = 1234)) %>%
tidytext:::tidy.LDA()

# Critical terms by topic 
top_terms <- model_dtm %>%
group_by(topic) %>%
top_n(10, beta) %>%
ungroup() %>%
arrange(topic, -beta)

top_terms %>%
mutate(term = reorder(term, beta)) %>%
ggplot(aes(term, beta, fill = factor(topic))) +
geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
facet_wrap(~ topic, scales = "free") +
coord_flip() +
labs(title = "Critical Terms (Words) by Topic",
x = "Term (Word)",
y = "Beta")

model_dtmGamma <- text_dtm %>%
cast_dtm(GameTitle, word, n) %>%
LDA(k = 4, control = list(seed = 1234)) %>%
tidytext:::tidy.LDA(matrix = "gamma") %>%
group_by(topic) %>%
top_n(10, gamma)

pander(filter(model_dtmGamma, topic == 1))
pander(filter(model_dtmGamma, topic == 2))
pander(filter(model_dtmGamma, topic == 3))
pander(filter(model_dtmGamma, topic == 4))
