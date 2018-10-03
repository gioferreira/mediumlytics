# Topic Modelling

source('src/utils/utils.R')
library(stm)
library(tidyverse)
library(skimr)
library(lubridate)
library(furrr)

posts_tbl_processed <- read_csv("saved_data/posts_tbl_processed_20181002.csv")

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

saveRDS(spectral_init, "saved_data/spectral_init_k0_20181002.rds")

# Use Tidy Text Method to model stm with different Ks and choosing the best one
plan(cluster)

many_models_20181002 <- data_frame(K = seq(2, 74, 2)) %>%
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

# Many Models Evaluation
many_models_20181002 <- readRDS("saved_data/many_models_20181002.rds")
heldout <- make.heldout(documents = docs,
                        vocab = vocab)

k_result <- many_models_20181002 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, docs),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, docs),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  filter(K <= 42) %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "Searching for the magical K: Range 2 to 42")

ggsave("plots/many_models_20181002-2to42.pdf",
       width = 19.2,
       height = 10.8,
       units = "cm")

# Comment on this: https://github.com/bstewart/stm/issues/152
# read: http://www.periodicos.letras.ufmg.br/index.php/relin/article/view/8916/8803
# read: https://estudogeral.sib.uc.pt/bitstream/10316/35724/1/Semantic%20Topic%20Modelling.pdf
