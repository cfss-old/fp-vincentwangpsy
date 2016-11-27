library(tidytext)
library(tidyverse)
library(SnowballC)
library(feather)

# Load stop word dictionery
data(stop_words)


# Main dataset
  # Read data, adjust encoding, remove duplicate data
  text_raw <- read_csv("./data/df_cb_main.csv") %>%
    mutate(Review = iconv(Review, "ASCII", "UTF-8")) %>%
    transmute(GameTitle = `Game Title`, Review, GSScore = `GS Score`, ESRB, AuthorName = `Author Name`) %>%
    arrange(GameTitle, GSScore) %>%
    distinct(GameTitle, .keep_all = TRUE)
  
  # Tokenize, remove stop words, stem words
  text_tidy <- data_raw %>%
    unnest_tokens(word, Review) %>%
    anti_join(stop_words, by = "word") %>%
    mutate(word = wordStem(word))
  
  # Generate tfidf index
  text_tfidf <- text_tidy %>%
    group_by(GameTitle) %>%
    count(word, sort = TRUE) %>%
    bind_tf_idf(word, GameTitle, n)
  
  # Save in feather file
  write_feather(text_tidy, "./data/text_tidy.feather")
  write_feather(text_tfidf, "./data/text_tfidf.feather")
  
  
# Genre dataset
  # Read data, remove duplicate data'
  genre_tidy <- read_csv("./data/df_cb_genre.csv") %>%
    transmute(GameTitle = `Game Title`, Genre) %>%
    distinct(GameTitle, Genre, .keep_all = TRUE)
  
  # Save in feather file
  write_feather(genre_tidy, "./data/genre_tidy.feather")
