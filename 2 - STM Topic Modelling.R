# Topic Modelling

source('src/utils/utils.R')
library(stm)
library(tidyverse)
library(skimr)
library(lubridate)
library(furrr)
library(tidytext)
library(scales)
library(ggthemes)

posts_tbl_processed <- read_csv("saved_data/posts_tbl_processed_20181002.csv")

# 2018 10 04 Retrain the STM including Tag_1 as Meta

posts_txts <- posts_tbl_processed %>%
  select(id,
         user_id,
         first_published_at,
         tag_1,
         text) %>%
  filter(!is.na(tag_1)) %>%
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

out <- prepDocuments(processed_txts$documents, 
                     processed_txts$vocab, 
                     processed_txts$meta, 
                     lower.thresh = 25)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# # Use spectral init with k = 0 to search for the ideal K
# spectral_init <- stm(documents = docs,
#                      vocab = vocab,
#                      K = 0,
#                      prevalence = ~ user_id + tag_1 + s(day_published),
#                      data = meta,
#                      init.type = "Spectral",
#                      gamma.prior = "L1")
# 
# saveRDS(spectral_init, "saved_data/spectral_init_k0_20181004_add_tag1.rds")


spectral_init <- read_rds("saved_data/spectral_init_k0_20181004_add_tag1.rds")
# Use Tidy Text Method to model stm with different Ks and choosing the best one
# Re renuning wigh tag as meta
plan(multiprocess)

many_models_20181004 <- data_frame(K = seq(2, 74, 2)) %>%
  mutate(topic_model = future_map(K,
                                  ~stm(documents = docs,
                                       vocab = vocab,
                                       K = .,
                                       prevalence = ~ user_id + tag_1 + s(day_published),
                                       data = meta,
                                       verbose = TRUE,
                                       init.type = "Spectral",
                                       gamma.prior = "L1"),
                                  .progress = TRUE))

saveRDS(many_models_20181004, "saved_data/many_models_20181004_add_tag1.rds")

# Many Models Evaluation
many_models_20181004 <- readRDS("saved_data/many_models_20181004_add_tag1.rds")
heldout <- make.heldout(documents = docs,
                        vocab = vocab)

k_result <- many_models_20181004 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, docs),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, docs),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result, "saved_data/k_result_many_models_20181004_add_tag1.rds")
k_result <- read_rds("saved_data/k_result_many_models_20181004_add_tag1.rds")

k_result %>%
  # filter(K !=46,
  #        K != 66,
  #        K != 72) %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  # coord_cartesian(ylim = 2.1) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "Searching for the magical K")

ggsave("plots/many_models_20181004.pdf",
       width = 19.2,
       height = 10.8,
       units = "cm")

# Follow up evaluation using many_models_20181002 with 22, 26, 34, 44, 54 topics.
k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(22, 26, 34, 54)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

## Extracting topic_model with 22 & 54 topics 
topic_model_22 <- k_result %>% 
  filter(K == 22) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model_54 <- k_result %>% 
  filter(K == 54) %>% 
  pull(topic_model) %>% 
  .[[1]]

## Exploring the topic models

topic_model <- spectral_init

td_beta <- tidy(topic_model)
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(docs))

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

labelTopics(topic_model, c(1, 6, 5, 3, 21, 7))
findThoughts(model = topic_model, texts = meta$user_id, topics = c(1, 6, 3, 7))

# Comment on this: https://github.com/bstewart/stm/issues/152
# read: http://www.periodicos.letras.ufmg.br/index.php/relin/article/view/8916/8803
# read: https://estudogeral.sib.uc.pt/bitstream/10316/35724/1/Semantic%20Topic%20Modelling.pdf
# read: https://github.com/bstewart/stm/issues/159
