# Topic Modelling

# Loading Stuff ####
source('src/utils/utils.R')
library(stm)
library(tidyverse)
library(skimr)
library(lubridate)
library(furrr)
library(tidytext)
library(scales)
library(ggthemes)

# Process Texts ####
# Load the final output of the 0 - Scrape Medium script
posts_tbl_processed <- read_rds("saved_data/posts_tbl_processed_20190824.rds")
posts_txts <- posts_tbl_processed %>%
  select(id,
         user_id,
         first_published_at,
         tag_1,
         text,
         word_count) %>%
  arrange(first_published_at) %>%
  mutate(day_published = first_published_at - min(.$first_published_at)) %>%
  mutate(first_published_at = floor_date(first_published_at, "days"),
         day_published = round(as.numeric(day_published, "days")) + 1) %>%
  filter(!is.na(tag_1),
         nchar(as.character(tag_1)) > 3,
         word_count >= 20) %>%
  select(-first_published_at, -word_count) %>%
  arrange_vars(c("day_published" = 3))

stop_words <- read_lines("saved_data/final_stopwords.txt")
stop_words_total <- unique(c(tm::stopwords('en'),
                             tm::stopwords('SMART'),
                             tm::stopwords('pt'),
                             stop_words))

# # Process Texts or Load Pre Processed
# processed_txts <- textProcessor(posts_txts$text,
#                                 metadata = posts_txts,
#                                 removestopwords = FALSE,
#                                 stem = FALSE,
#                                 language = "pt",
#                                 customstopwords = stop_words_total)
# 
# write_rds(processed_txts, "saved_data/processed_txts_20190825.rds")

processed_txts <- read_rds("saved_data/processed_txts_20190825.rds")

out <- prepDocuments(processed_txts$documents, 
                     processed_txts$vocab, 
                     processed_txts$meta, 
                     lower.thresh = 25)



docs <- out$documents
vocab <- out$vocab
meta <- out$meta



# Search for the Ideal K ####
# # Use spectral init with k = 0 to search for the ideal K
# spectral_init <- stm(documents = docs,
#                      vocab = vocab,
#                      K = 0,
#                      prevalence = ~ user_id + tag_1 + s(day_published),
#                      data = meta,
#                      init.type = "Spectral",
#                      gamma.prior = "L1")
# 
# saveRDS(spectral_init, "saved_data/spectral_init_k0_20190825_add_tag1.rds")

spectral_init <- read_rds("saved_data/spectral_init_k0_20190825_add_tag1.rds")

spectral_init

# Spectral Init converged to a 66 topics model suggesting that we will settle on a high number
# That's expected given the diversity of the magazine

# Use Tidy Text Method to model stm with different Ks and choosing the best one
# # Re renuning wigh tag as meta
K <- c(5:10, seq(12, 20, 2), seq(24, 48, 4), seq(50, 70, 2))
# 
# plan(cluster)
# 
# many_models <- data_frame(K = K) %>%
#   mutate(topic_model = future_map(K,
#                                   ~stm(documents = docs,
#                                        vocab = vocab,
#                                        K = .,
#                                        prevalence = ~ user_id + tag_1 + s(day_published),
#                                        data = meta,
#                                        verbose = TRUE,
#                                        init.type = "Spectral",
#                                        gamma.prior = "L1"),
#                                   .progress = TRUE))
# 
# saveRDS(many_models, "saved_data/many_models_20190825_add_tag1.rds")