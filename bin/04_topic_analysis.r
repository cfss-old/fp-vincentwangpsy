library(scales)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(feather)
library(broom)
library(pander)

# Read in data
text_tidy <- read_feather("./data/text_tidy.feather")
text_tfidf <- read_feather("./data/text_tfidf.feather")
text_raw <- read_feather("./data/text_raw.feather")
genre_tidy <- read_feather("./data/genre_tidy.feather")


# Transform into DocumentTermMatrix form
text_dtm <- text_tidy %>%
  group_by(GameTitle) %>%
  count(word) %>%
  cast_dtm(GameTitle, word, n)


# LDA model for categorization
  # Apply LDA model
  model_lda <- text_dtm %>%
    LDA(k = 4, control = list(seed = 2016))
  
  # Critical terms by topic 
  top_terms <- model_lda %>%
    tidytext:::tidy.LDA() %>%
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
  
  # Games by topic
  games_lda <- model_lda %>%
    tidytext:::tidy.LDA(matrix = "gamma") %>%
    group_by(document) %>%
    top_n(1, wt = gamma) %>%
    ungroup()
  
  ggplot(games_lda, aes(topic)) +
    geom_bar() +
    labs(title = "Topic Distribution",
         x = "Topics",
         y = "Frequency")
  
  pander(arrange(top_n(filter(games_lda, topic == 1), 10, gamma), -gamma))
  pander(arrange(top_n(filter(games_lda, topic == 2), 10, gamma), -gamma))
  pander(arrange(top_n(filter(games_lda, topic == 3), 10, gamma), -gamma))
  pander(arrange(top_n(filter(games_lda, topic == 4), 10, gamma), -gamma))
  
  