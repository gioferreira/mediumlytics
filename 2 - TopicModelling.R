# Topic Modelling

source('src/utils/utils.R')

library(tidyverse)
library(magrittr)
library(skimr)
library(ggthemes)
# library(gridExtra) #pra tabela
library(lubridate)
library(scales)
# library(gghighlight)
library(tidytext)

library(stm)
library(furrr)
library(parallel)
library(doParallel)
library(quanteda)

posts_tbl_processed <- read_csv("saved_data/posts_tbl_processed_20180930.csv")


posts_txts <- posts_tbl_processed %>%
  select(id, text)

stop_words <- read_lines("saved_data/final_stopwords.txt")
stop_words_total <- unique(c(tm::stopwords('en'),
                             tm::stopwords('SMART'),
                             tm::stopwords('pt'),
                             stop_words))

stop_words_total <- as_tibble(stop_words_total)

names(stop_words_total) <- "word"

tidy_posts_txts <- posts_txts %>%
  unnest_tokens(word, text, token = "words") %>%
  anti_join(stop_words_total) %>%
  filter(!str_detect(word, "[0-9]+"))

sparse_posts_txts <- tidy_posts_txts %>%
  count(id, word) %>%
  cast_sparse(id, word, n)

# # Many Models 6
# ## Playing with control parameter for stm
# 
# plan(cluster)
# 
# control <- list("nits" = 500,
#                 "burnin" = 50)
# 
# many_models6 <- data_frame(K = seq(2, 50, 2)) %>%
#   mutate(topic_model = future_map(K, 
#                                   ~stm(sparse_posts_txts, 
#                                        K = ., 
#                                        verbose = TRUE,
#                                        init.type = "LDA",
#                                        control = control), 
#                                   .progress = TRUE))
# 
# saveRDS(many_models6, "saved_data/many_models6_20181001b.rds")
# 
# many_models6 <- readRDS("saved_data/many_models6_20181001b.rds")
# heldout <- make.heldout(sparse_posts_txts)
# 
# k_result <- many_models6 %>%
#   mutate(exclusivity = map(topic_model, exclusivity),
#          semantic_coherence = map(topic_model, semanticCoherence, sparse_posts_txts),
#          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
#          residual = map(topic_model, checkResiduals, sparse_posts_txts),
#          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
#          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
#          lbound = bound + lfact,
#          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
# 
# k_result %>%
#   transmute(K,
#             `Lower bound` = lbound,
#             Residuals = map_dbl(residual, "dispersion"),
#             `Semantic coherence` = map_dbl(semantic_coherence, mean),
#             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
#   gather(Metric, Value, -K) %>%
#   ggplot(aes(K, Value, color = Metric)) +
#   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
#   facet_wrap(~Metric, scales = "free_y") +
#   labs(x = "K (number of topics)",
#        y = NULL,
#        title = "Model diagnostics by number of topics",
#        subtitle = "Searching for the magical K")
# 
# ggsave("plots/many_models6b.pdf",
#        width = 19.2,
#        height = 10.8,
#        units = "cm")
# 
# ## Follow up evaluation using Many_Models6b with 4, 10, 18, 34, 38, 46 topics.
# 
# k_result %>%
#   select(K, exclusivity, semantic_coherence) %>%
#   filter(K %in% c(18, 24, 34, 38, 46)) %>%
#   unnest() %>%
#   mutate(K = as.factor(K)) %>%
#   ggplot(aes(semantic_coherence, exclusivity, color = K)) +
#   geom_point(size = 2, alpha = 0.7) +
#   labs(x = "Semantic coherence",
#        y = "Exclusivity",
#        title = "Comparing exclusivity and semantic coherence",
#        subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
# 
# saveRDS(k_result, "saved_data/k_result_models6_20181001b.rds")

## Extracting topic_model with 34 topics from 
k_result <- readRDS("saved_data/k_result_models6_20181001b.rds")

topic_model <- k_result %>% 
  filter(K == 34) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model

## Exploring the topic model

td_beta <- tidy(topic_model)
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(sparse_posts_txts))

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
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence",
       subtitle = "With the top words that contribute to each topic")


gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

#
# Many Models 7 - init.type = "Spectral", K = 0 

control <- list("nits" = 500,
                "burnin" = 50)

many_models7 <- stm(sparse_posts_txts, 
                    init.type = "Spectral",
                    K = 0, 
                    verbose = TRUE)

saveRDS(many_models7, "saved_data/many_models7_20181001.rds")

many_models7 <- readRDS("saved_data/many_models7_20181001.rds")

## Exploring the topic model
topic_model <- many_models7

td_beta <- tidy(topic_model)
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(sparse_posts_txts))

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
  top_n(60, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 2.5,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence",
       subtitle = "With the top words that contribute to each topic")


gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))
