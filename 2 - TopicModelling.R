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

stop_words <- read_csv("saved_data/final_stopwords.txt", col_names = FALSE)
names(stop_words) <- "word"

tidy_posts_txts <- posts_txts %>%
  unnest_tokens(word, text, token = "words") %>%
  anti_join(stop_words)

sparse_posts_txts <- tidy_posts_txts %>%
  count(id, word) %>%
  cast_sparse(id, word, n)

# dfm_posts_txts <- tidy_posts_txts %>%
#   count(id, word, sort = TRUE) %>%
#   cast_dfm(id, word, n)

plan(cluster)

many_models5 <- data_frame(K = seq(2, 50, 2)) %>%
  mutate(topic_model = future_map(K, 
                                  ~stm(sparse_posts_txts, 
                                       K = ., 
                                       verbose = FALSE,
                                       init.type = "LDA"), 
                                  .progress = TRUE))

saveRDS(many_models5, "saved_data/many_models5_20180930.rds")

heldout <- make.heldout(sparse_posts_txts)

k_result <- many_models5 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, sparse_posts_txts),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, sparse_posts_txts),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
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
       subtitle = "Searching for the magical K")

ggsave("plots/many_models5.pdf",
       width = 19.2,
       height = 10.8,
       units = "cm")


# Many Models 6
## Playing with control parameter for stm

plan(cluster)

control <- list("nits" = 500,
                "burnin" = 50)

many_models6 <- data_frame(K = seq(2, 50, 2)) %>%
  mutate(topic_model = future_map(K, 
                                  ~stm(sparse_posts_txts, 
                                       K = ., 
                                       verbose = TRUE,
                                       init.type = "LDA",
                                       control = control), 
                                  .progress = TRUE))

saveRDS(many_models6, "saved_data/many_models6_20181001.rds")

heldout <- make.heldout(sparse_posts_txts)

k_result <- many_models6 %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, sparse_posts_txts),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, sparse_posts_txts),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
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
       subtitle = "Searching for the magical K")

ggsave("plots/many_models6.pdf",
       width = 19.2,
       height = 10.8,
       units = "cm")

## Follow up evaluation using Many_Models6 with 2, 10, 16, 22 topics.

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(2, 10, 16, 22)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")
