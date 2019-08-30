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
# The processing removed 249 texts probably most from not having any Tag (which I'll use for meta)
# or for having a word count of 19 or less

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

# Use Tidy Text Method to model STM with different Ks and choose the best one
# I'm using STM prevalence parameter with user_id + tag + s(day_published) as meta
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
# Search for the good/appropriate/"best" number of topics using Julia Silge's method
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
       subtitle = "Searching for the magical K") +
  theme_tufte() +
  theme(
    panel.grid.major.x = element_line(
      linetype = "dotted",
      color = "grey"))

ggsave("plots/many_models_20190825.png",
       width = 19.2*1.8,
       height = 10.8*1.8,
       units = "cm")

# Follow up evaluation using many_models_20190826 with 7, 12, 18, 36, 44, 52 ,62 topics. ####
# Based on the semantic coherence, residuals and held-out likelihood I choose the above topics
# to further investigate using  semantic coherece against exclusivity
# Acording to Silge, held-out likelihood must be high, residuals low and semantic coherence high
# Semantic coherence is maximized when the most probable words in a given topic frequently 
# co-occur together, and it’s a metric that correlates well with human judgment of topic quality.
# Having high semantic coherence is relatively easy, though, if you only have a few topics 
# dominated by very common words, so you want to look at both semantic coherence and exclusivity 
# of words to topics.
# and then 7, 18, 36, 44, 52
# and then 7, 18, 36, 52
# and then 7, 18, 36

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(7, 18, 36)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_tableau() +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for every topic",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

# Extracting topic_models with 7, 18, 36 for an in depth analysis ####
topic_model_7 <- many_models %>% 
  filter(K == 7) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model_18 <- many_models %>% 
  filter(K == 18) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model_36 <- many_models %>% 
  filter(K == 36) %>% 
  pull(topic_model) %>% 
  .[[1]]


# Exploring the topic models ####
# I ran the code below on each one of the 3 models
topic_model <- topic_model_36

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
# Kable for plot below
# gamma_terms %>%
#   select(topic, gamma, terms) %>%
#   kable(digits = 3,
#         col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

gamma_terms %>%
  # top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3.2, color = rgb(.3, .3, .3)) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.095),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence in the New Order corpus",
       subtitle = "With the top words that contribute to each topic")

ggsave("plots/topics_by_prevalence.png",
       width = 19.2*1.8,
       height = 10.8*1.8,
       units = "cm")

td_beta_plot <- td_beta %>%
  group_by(topic) %>%
  top_n(10, wt = beta)  %>%
  ungroup() %>%
  arrange(topic, beta) %>%
  mutate(order = row_number())

td_beta_plot %>% 
  ggplot(aes(order, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_x_continuous(breaks = td_beta_plot$order,
                     labels = td_beta_plot$term,
                     expand = c(0,0)) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 0.05, by =.01), 
                     labels = percent_format(accuracy = 1)) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 7),
        panel.grid.major.x = element_line(
          linetype = "dotted",
          color = "grey")) +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic for 36 topics model",
       subtitle = "Different words are associated with different topics")

ggsave("plots/word_probability_by_topic.png",
       width = 25,
       height = 20,
       units = "cm")
# Labelling Topics in the chosen model ####
# After settling for the 36 topics model, let's try to label them
labelTopics(topic_model)
# Using STM findThoughts(to extract IDs of examples of each topic)
id_list <- findThoughts(model = topic_model_36, texts = as.character(meta$id), n = 5)

# Check links/slug for example of each topic to help labelling

# Given an URL TBL (generated on the Scrape Medium Script and a "id_list" outputed from 
# findThoughs() the function below recover the links)
extract_links <- function(id_list, url_tbl) {
  docs <- id_list$docs
  process_doc <- function(doc, ...) {
    out <- enframe(doc, name = NULL, value = "id") %>%
      left_join(url_tbl) %>%
      select(-unique_slug)
    return(out)
  }
  out <- map(docs, process_doc, url_tbl = url_tbl)
  return(out)
}

url_tbl <- read_rds("saved_data/url_tbl_20190824.rds")
links <- extract_links(id_list, url_tbl)

# Based on the labelTopics() and on reading a couple of articles from each topic
# these are the labels/names for each topic:

topic_labels <- tribble(
  ~topic, ~label,
  1, "Gente-Vida-Cotidiano",
  2, "Cinema-Oscars",
  3, "Cronica-Sociedade-Comportamento",
  4, "Relacionamentos-Sentimentos-Babaca-Cansei",
  5, "Literatura-Ficção-Quadrinhos",
  6, "Consciência-Espiritualidade-Meditação",
  7, "Relacionamentos-Amor",
  8, "Redação-Trendr-New_Order-Letter",
  9, "Dinheiro-Consumo",
  10, "Novela-TV",
  11, "Facebook-Redes_Sociais-Conteúdo-Serviços_Digitais",
  12, "Política_Nacional-Presidente",
  13, "Ensino-Educação-Aprendizagem",
  14, "Religão-Moral-Socieadde-Crenças",
  15, "Instagram-Redes_Sociais-Internet",
  16, "Escrita-Texto-Língua_Portuguesa",
  17, "Rio_de_Janeiro-Polícia-Carnaval-Violência",
  18, "Sociedade-Capitalismo-Economia-Filosofia",
  19, "Vida_Adulta-Felicidade-Resiliência-Bolha",
  20, "Rock-Música-Rap",
  21, "Séries",
  22, "Design-Arte-Publicidade-Mmouranet",
  23, "New_Order-Cassio-Temporadas-Diversidade-Cultura",
  24, "Rapidinhas-Tecnologia",
  25, "Fake_News-Informação-Notícia",
  26, "Vida-Universo-Morte-Estrelas-Céu",
  27, "Dor_Emocional-Depressão-Ansiedade-Dor",
  28, "Emprego-Trabalho-Refroma_da_Previdência-Empresa",
  29, "Arte-Cultura-Museus-Espaços_Culturais",
  30, "Casa-Família-Mãe-Pai-Filhos",
  31, "Violência-Homem-Mulher-Estupro-Corpo",
  32, "Democracia-Política-Corrupção",
  33, "Genêro-Feminismo-Gay-Lésbica-Machismo",
  34, "Ciência-Tecnologia-Nasa-Pesquisa",
  35, "Filme-Heróis-Batman-Marvel-Franquias",
  36, "Futebol-Atletas-Esporte-Time"
)
# These topic labels were hand created using common words, slug information and some reading.
# Making new Plots using the labels ####

gamma_terms %>%
  mutate(topic = as.character(topic)) %>%
  left_join(topic_labels %>% 
              mutate(topic = paste("Topic", topic))) %>% 
  mutate(topic = as_factor(topic),
         topic = fct_reorder(topic, gamma)) %>% 
  ggplot(aes(topic, gamma, label = label, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3, color = rgb(.3, .3, .3)) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.095),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  labs(x = NULL, y = expression(gamma),
       title = "Named Topics by prevalence in New Order corpus",
       subtitle = "With Topic Labels")

ggsave("plots/named_topics_by_prevalence.png",
       width = 25,
       height = 20,
       units = "cm")


# Extract TD Gamma to Merge with posts_tbl_processed ####

posts_tbl_topics <- posts_tbl_processed %>%
  left_join(
    td_gamma %>% 
      group_by(document) %>% 
      arrange(document, -gamma) %>% 
      top_n(1, gamma) %>% 
      select(-gamma) %>% 
      rename("prevalent_topic" = "topic") %>% 
      bind_cols(meta %>% 
                  select(id)) %>% 
      ungroup() %>% 
      left_join(
        td_gamma %>% 
          spread(topic, gamma) %>% 
          rename_at(vars(num_range("", 1:36)), ~ str_replace(., "^", "Topic_"))
      ) %>% 
      select(-document) %>% 
      arrange_vars(c("id" = 1))
  )

write_rds(posts_tbl_topics, "saved_data/posts_tbl_topics-20190829.rds")
  


  
# References ####  
# Comment on this: https://github.com/bstewart/stm/issues/152
# read: http://www.periodicos.letras.ufmg.br/index.php/relin/article/view/8916/8803
# read: https://estudogeral.sib.uc.pt/bitstream/10316/35724/1/Semantic%20Topic%20Modelling.pdf
# read: https://github.com/bstewart/stm/issues/159
# https://juliasilge.com/blog/evaluating-stm/
# https://juliasilge.com/blog/sherlock-holmes-stm/