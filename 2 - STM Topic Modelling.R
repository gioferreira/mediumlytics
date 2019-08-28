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
# library(tidylog)

posts_tbl_processed <- read_rds("saved_data/posts_tbl_processed_20190824.rds")


# Process Texts ####
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

# 2019 08 25 Retrained the STM including Tag_1 as Meta ####

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

# Many Models Evaluation ####
many_models <- readRDS("saved_data/many_models_20190825_add_tag1.rds")
heldout <- make.heldout(documents = docs,
                        vocab = vocab)

# k_result <- many_models %>%
#   mutate(exclusivity = map(topic_model, exclusivity),
#          semantic_coherence = map(topic_model, semanticCoherence, docs),
#          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
#          residual = map(topic_model, checkResiduals, docs),
#          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
#          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
#          lbound = bound + lfact,
#          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
# 
# saveRDS(k_result, "saved_data/k_result_many_models_20190825_add_tag1.rds")
k_result <- read_rds("saved_data/k_result_many_models_20190825_add_tag1.rds")

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
  scale_x_continuous(breaks = K) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "Searching for the magical K")

ggsave("plots/many_models_20190825.pdf",
       width = 19.2*1.8,
       height = 10.8*1.8,
       units = "cm")

# Follow up evaluation using many_models_20190826 with 7, 12, 18, 36, 44, 52 ,62 topics. ####
# and then 7, 18, 36, 44, 52
# and then 18, 36, 44, 52
# and then 18, 36, 52
k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(7, 36)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_tableau() +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for every topic",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

## Extracting topic_model with 7, 36
topic_model_7 <- k_result %>% 
  filter(K == 7) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model_36 <- k_result %>% 
  filter(K == 36) %>% 
  pull(topic_model) %>% 
  .[[1]]

# Exploring the topic models ####

topic_model <- topic_model_7

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

labelTopics(topic_model, c(7, 1, 12, 17, 4, 2))
findThoughts(model = topic_model, texts = meta$id, topics = c(7, 1, 12, 17, 4, 2))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       # subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))


gamma_terms %>%
  # top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.001, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.1),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 36 topics by prevalence in New Order corpus",
       subtitle = "With the top words that contribute to each topic")

gamma_terms %>%
  # top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.01, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.5),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 7 topics by prevalence in New Order corpus",
       subtitle = "With the top words that contribute to each topic")

# Comment on this: https://github.com/bstewart/stm/issues/152
# read: http://www.periodicos.letras.ufmg.br/index.php/relin/article/view/8916/8803
# read: https://estudogeral.sib.uc.pt/bitstream/10316/35724/1/Semantic%20Topic%20Modelling.pdf
# read: https://github.com/bstewart/stm/issues/159
