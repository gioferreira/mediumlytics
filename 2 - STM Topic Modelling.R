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
posts_tbl_processed_path <- paste0("saved_data/posts_tbl_processed_",
                                   gsub("-", "", today()),
                                   ".rds")
posts_tbl_processed <- read_rds(posts_tbl_processed_path)

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

# Process Texts or Load Pre Processed
processed_txts_path <- paste0("saved_data/processed_txts_",
                              gsub("-", "", today()),
                              ".rds")

if (!file.exists(processed_txts_path)) {
  processed_txts <- textProcessor(posts_txts$text,
                                  metadata = posts_txts,
                                  removestopwords = FALSE,
                                  stem = FALSE,
                                  language = "pt",
                                  customstopwords = stop_words_total)
  
  write_rds(processed_txts, processed_txts_path)
} else {
  processed_txts <- read_rds(processed_txts_path)  
}

# The processing removed some texts probably most from not having any Tag (which I'll use for meta)
# or for having a word count of 19 or less

out <- prepDocuments(processed_txts$documents, 
                     processed_txts$vocab, 
                     processed_txts$meta, 
                     lower.thresh = 25)



docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# Search for the Ideal K ####
# Use spectral init with k = 0 to search for the ideal K
spectral_init_path <- paste0("saved_data/spectral_init_k0_",
                             gsub("-", "", today()),
                             ".rds")
if (!file.exists(spectral_init_path)) {
  spectral_init <- stm(documents = docs,
                       vocab = vocab,
                       K = 0,
                       prevalence = ~ user_id + tag_1 + s(day_published),
                       data = meta,
                       init.type = "Spectral",
                       gamma.prior = "L1")
  
  saveRDS(spectral_init, spectral_init_path)
} else {
  spectral_init <- read_rds(spectral_init_path)  
}

spectral_init

# Spectral Init converged to a 62 topics model suggesting that we will settle on a high number
# That's expected given the diversity of the magazine

# Many Models method from Tidy Text ####
# Use Tidy Text Method to model STM with different Ks and choose the best one
# I'm using STM prevalence parameter with user_id + tag + s(day_published) as meta
many_models_path <- paste0("saved_data/many_models_",
                           gsub("-", "", today()),
                           ".rds")

if (!file.exists(many_models_path)) {
  K <- c(seq(4, 16, 4), seq(20, 45, 1), seq(46, 82, 4))
  
  no_cores <- availableCores() - 1
  plan("multiprocess", workers = no_cores)
  many_models <- data_frame(K = K) %>%
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
  
  
  saveRDS(many_models, many_models_path)
} else {
  many_models <- readRDS(many_models_path)  
}

# Many Models Evaluation ####
# Search for the good/appropriate/"best" number of topics using Julia Silge's method
heldout <- make.heldout(documents = docs,
                        vocab = vocab)

k_result_path <- paste0("saved_data/k_result_many_models_",
                        gsub("-", "", today()),
                        ".rds")

if (!file.exists(k_result_path)) {
  k_result <- many_models %>%
    mutate(exclusivity = map(topic_model, exclusivity),
           semantic_coherence = map(topic_model, semanticCoherence, docs),
           eval_heldout = map(topic_model, eval.heldout, heldout$missing),
           residual = map(topic_model, checkResiduals, docs),
           bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
           lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
           lbound = bound + lfact,
           iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
  
  saveRDS(k_result, k_result_path)  
} else {
  k_result <- read_rds(k_result_path)  
}

many_models_plot <- k_result %>%
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
      color = "grey"),
    axis.text.x  = element_text(
      angle = 90,
      vjust = .5
    ))

many_models_plot_path <- paste0("plots/many_models_",
                           gsub("-", "", today()),
                           ".png")

ggsave(many_models_plot_path,
       plot = many_models_plot,
       width = 29,
       height = 21,
       units = "cm",
       scale = 1.5)

# Follow up evaluation using many_models_20200427 with 12, 24, 31, 35 topics. ####
# Based on the semantic coherence, residuals and held-out likelihood I choose the above topics
# to further investigate using  semantic coherece against exclusivity
# Acording to Silge, held-out likelihood must be high, residuals low and semantic coherence high
# Semantic coherence is maximized when the most probable words in a given topic frequently 
# co-occur together, and it’s a metric that correlates well with human judgment of topic quality.
# Having high semantic coherence is relatively easy, though, if you only have a few topics 
# dominated by very common words, so you want to look at both semantic coherence and exclusivity 
# of words to topics.


k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(12, 24, 31, 35)) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_tableau() +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence for every topic",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

# Extracting topic_models with 12, 24 for an in depth analysis ####
topic_model_12 <- many_models %>% 
  filter(K == 12) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model_24 <- many_models %>% 
  filter(K == 24) %>% 
  pull(topic_model) %>% 
  .[[1]]

# Exploring the topic models ####
# I ran the code below on each one of the 2 models
topic_model <- topic_model_24

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
  unnest(cols = c(terms))

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
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3.2, color = rgb(.3, .3, .3)) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.11),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Tópicos por prevalência no conjunto de textos da New Order",
       subtitle = "Com as palavras que mais contribuem para cada tópico")

ggsave("plots/09-topics_by_prevalence.png",
       width = 29,
       height = 21,
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
  facet_wrap(~ topic, ncol = 4, scales = "free_y") +
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
       title = "Palavras com maior probabilidade de aparecer em cada tópico")
       # subtitle = "Different words are associated with different topics")

ggsave("plots/10-word_probability_by_topic 24 topics.png",
       width = 29,
       height = 21,
       units = "cm")

# Labelling Topics in the chosen model ####
# After settling for the 25 topics model, let's try to label them

labels <- labelTopics(topic_model, n = 10)
# Using STM findThoughts(to extract IDs of examples of each topic)
id_list <- findThoughts(model = topic_model, texts = as.character(meta$id), n = 5)

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

url_tbl <- read_rds("saved_data/url_tbl_20200531.rds")
links <- extract_links(id_list, url_tbl)

# Based on the labelTopics() and on reading a couple of articles from each topic
# these are the labels/names for each topic:

topic_labels <- tribble(
  ~topic, ~label,
  1, "Gente-Vida-Cotidiano",
  2, "Filme-Cinema",
  3, "Saude_Mental-Ansiedade-Yoga-Psiquiatra",
  4, "Literatura-Filosofia-Ficção-Quadrinhos",
  5, "Pessoa-Opinião-Empatia",
  6, "Universo-Vida-Ciência-Religião",
  7, "Felicidade-Meditação-Emoções-Bem_Estar",
  8, "Saudade-Família-Casa-Amor-diCredico",
  9, "Novela-TV-Séries",
  10, "Educação",
  11, "Política-Nacional",
  12, "Homem-Mulher-Gênero",
  13, "Sociedade-Cultura-Economia-Capitalismoo",
  14, "Rapidinhas-Tecnologia-Arte-Design",
  15, "Futebol-Times-Empresas",
  16, "Escrita-Narrativa-Língua_Portuguesa",
  17, "Internet-Redes_Sociais",
  18, "New_Order-Cassio-Revista-News_Letter",
  19, "Música-Cultura",
  20, "Amor-Relacionamentos",
  21, "Cidade-Rio-SP-Centros_Urbanos",
  22, "Design-Publicidade-Moda-Jogos",
  23, "Família-Pais-Filhos",
  24, "Notícias-Jornalismo-Fake_News"
) %>% 
  mutate(label = as_factor(label))

saveRDS(topic_labels, "saved_data/topic_labels-20200602.rds")
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
                     limits = c(0, 0.11),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  labs(x = NULL, y = expression(gamma),
       title = "Tópicos (Nomeados) ordenados por prevalência no conjunto de textos da New Order",
       subtitle = "Com as palavras que mais contribuem para cada tópico")

ggsave("plots/11-named_topics_by_prevalence.png",
       width = 25,
       height = 20,
       units = "cm")

library(GGally)
library(network)
library(sna)
library(ggrepel)
corr <- topicCorr(topic_model)
net <- network(corr$posadj, directed = FALSE)
my_rainbow <- scales::hue_pal()

set.seed(1312)
network_plot <- ggnet2(
  net,
  size = round(gamma_terms$gamma * 250) * 2,
  alpha = .2,
  edge.lty = "dotted",
  edge.color = "black",
  layout.exp = 0.3,
  color = my_rainbow(24)
) +
  geom_point(color = my_rainbow(24), size = 1) +
  geom_label_repel(
    aes(label = paste(topic_labels$topic, topic_labels$label, sep = " - ")),
    color = "white",
    fill = my_rainbow(24),
    fontface = "bold",
    size = 3,
    segment.alpha = 0,
    show.legend = FALSE
  ) +
  scale_fill_hue() +
  labs(title = "Rede de Correlação entre Tópicos") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16))
  

ggsave(
  "plots/13-topics_correlation_net.png",
  plot = network_plot,
  width = 25,
  height = 20,
  units = "cm",
  scale = 1.25
)
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
  ) %>% 
  left_join(topic_labels,
            by = c("prevalent_topic" = "topic"))


posts_tbl_topics_path <- paste0("saved_data/posts_tbl_topics_",
                                   gsub("-", "", today()),
                                   ".rds")

if (!file.exists(posts_tbl_topics_path)) {
  
  write_rds(posts_tbl_topics, posts_tbl_topics_path)
} else {
  processed_txts <- read_rds(posts_tbl_topics_path)  
}


# References ####  
# Comment on this: https://github.com/bstewart/stm/issues/152
# read: http://www.periodicos.letras.ufmg.br/index.php/relin/article/view/8916/8803
# read: https://estudogeral.sib.uc.pt/bitstream/10316/35724/1/Semantic%20Topic%20Modelling.pdf
# read: https://github.com/bstewart/stm/issues/159
# https://juliasilge.com/blog/evaluating-stm/
# https://juliasilge.com/blog/sherlock-holmes-stm/