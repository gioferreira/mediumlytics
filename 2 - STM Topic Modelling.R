# Topic Modelling

source('src/utils/utils.R')
library(stm)
library(tidyverse)
library(skimr)
library(lubridate)

posts_tbl_processed <- read_csv("saved_data/posts_tbl_processed_20180930.csv")

posts_txts <- posts_tbl_processed %>%
  select(id,
         user_id,
         first_published_at,
         text) %>%
  mutate(first_published_at = floor_date(first_published_at, "days"),
         day_published = as.integer((first_published_at + days(1)) - min(first_published_at))) %>%
  select(-first_published_at) %>%
  arrange_vars(c("day_published" = 3))

stop_words <- read_lines("saved_data/final_stopwords.txt")
stop_words_total <- unique(c(tm::stopwords('en'),
                             tm::stopwords('SMART'),
                             tm::stopwords('pt'),
                             stop_words))

processed_txts <- textProcessor(posts_txts$text, 
                                metadata = posts_txts,
                                removestopwords = FALSE,
                                stem = FALSE,
                                language = "pt",
                                customstopwords = stop_words_total)

out <- prepDocuments(processed_txts$documents, processed_txts$vocab, processed_txts$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta