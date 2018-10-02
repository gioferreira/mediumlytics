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

out <- prepDocuments(processed_txts$documents, processed_txts$vocab, processed_txts$meta, lower.thresh = 25)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Use spectral init with k = 0 to search for the ideal K
spectral_init <- stm(documents = docs,
                     vocab = vocab,
                     K = 0,
                     prevalence = ~ user_id + s(day_published),
                     data = meta,
                     init.type = "Spectral")


# Use Tidy Text Method to model stm with different Ks and choosing the best one
plan(cluster)

many_models_20181002 <- data_frame(K = seq(6, 70, 2)) %>%
  mutate(topic_model = future_map(K,
                                  ~stm(documents = docs,
                                       vocab = vocab,
                                       K = .,
                                       prevalence = ~ user_id + s(day_published),
                                       data = meta, 
                                       verbose = TRUE,
                                       init.type = "Spectral"),
                                  .progress = TRUE))

saveRDS(many_models_20181002, "saved_data/many_models_20181002.rds")

