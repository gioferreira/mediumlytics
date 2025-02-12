# Post 1
# Word2Vec Tags

# Loading Thigs ####
source('src/utils/utils.R')
library(magrittr)
library(tidyverse)
# devtools::install_github("elbersb/tidylog")
# library(tidylog)
library(skimr)
library(ggthemes)
library(lubridate)
library(scales)
library(tidytext)
library(h2o)
library(Rtsne)
library(dbscan)
library(ggwordcloud)
library(RColorBrewer)
library(ggrepel)

# palette <- c("#a0e85b", "#d725a3", "#36e515", "#7f2157", "#7ee8c0", "#fe1d66", "#5c922f", "#7835d3", "#e9d737", "#1c4bb4", "#f39450", "#154e56", "#e4ccf1", "#76480d", "#48b6ea", "#ae3028", "#fd92fa", "#115205", "#628df2", "#dbc58e", "#FFe85b", "#aFeFFb")

palette <-
  c(
    "#E5164B",
    "#39B24B",
    "#D3B60D",
    "#4363D7",
    "#F38131",
    "#901DB3",
    "#40D3F4",
    "#EF32E4",
    "#8CAF1E",
    "#F29999",
    "#46978E",
    "#E995F4",
    "#9A6325",
    "#CCC27F",
    "#7F0100",
    "#77E894",
    "#808000",
    "#EA5F34",
    "#000175",
    "#089CA3",
    "#78768E",
    "#A5A49B",
    "#2B0202",
    "#615263",
    "#0F122D"
  )

posts_tbl_processed_path <-
  paste0("saved_data/posts_tbl_processed_",
         gsub("-", "", today()),
         ".rds")

posts_tbl_processed <- read_rds(posts_tbl_processed_path)

# # Sanity Check
posts_tbl_processed %>% skim()


# WordCloud & Rank ####
colors <- brewer.pal(9, "Greys")[5:8]

set.seed(80)
word_cloud_data <- posts_tbl_processed %>%
  select(num_range("tag_", 1:5)) %>%
  mutate_if(is.factor, as.character) %>%
  gather() %>%
  select(value) %>%
  rename(word = value) %>%
  count(word, sort = TRUE) %>%
  filter(!is.na(word),
         word != "") %>%
  top_n(50, n) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(75, 25)))
# mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))

word_cloud_data %>%
  ggplot(aes(
    label = word,
    size = n,
    angle = angle,
    color = cut_number(n, 4)
  )) +
  geom_text_wordcloud_area(
    area_corr_power = 1 / .75,
    eccentricity = .8,
    grid_margin = 1,
    show_boxes = FALSE,
    seed = 1234,
    family = "sans"
  ) +
  scale_size_area(max_size = 20) +
  scale_color_manual(values = colors) +
  theme_minimal()

ggsave(
  "plots/01-wordcloud.png",
  width = 21,
  height = 14.85,
  units = "cm",
  dpi = 300
)


word_cloud_data %>%
  ggplot(aes(x = reorder(word, n, sum), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Rank das 50 Tags Mais Utilizadas\n") +
  geom_text(aes(label = n),
            angle = 0,
            # position = "Left",
            size = 2.25,
            nudge_y = 20) +
  theme_tufte() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 7)
  )

ggsave(
  "plots/02-rank_tags.png",
  width = 21,
  height = 14.85,
  units = "cm",
  dpi = 300
)

# Start H2O for Word2Vec ####

h2o.init()

# Select tags from dataset, concatenate them in one 'phrase'
tags <- posts_tbl_processed %>%
  select(num_range("tag_", 1:5), id) %>%
  mutate_if(is.factor, as.character) %>%
  replace_na(list(
    tag_1 = "",
    tag_2 = "",
    tag_3 = "",
    tag_4 = "",
    tag_5 = ""
  )) %>%
  unite(tags, -id, sep = " ", remove = TRUE) %>%
  mutate(tags = str_squish(tags)) %>%
  as.h2o(destination_frame = "tags")

# Custom tokenize function highly commented
tokenize <- function(sentences) {
  tokenized <- h2o.tokenize(sentences, " ")
  # convert to lower case
  tokenized.lower <- h2o.tolower(tokenized)
  # # remove short words (less than 2 characters)
  # tokenized.lengths <- h2o.nchar(tokenized.lower)
  # tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  # # remove words that contain numbers
  # tokenized.words <- tokenized.filtered[h2o.grep("[0-9]", tokenized.filtered, invert = TRUE, output.logical = TRUE),]
  
  # remove stop words
  # tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% STOP_WORDS),]
}

words <- h2o.na_omit(tokenize(tags$tags))

# Model or Load Model
w2v.model <-
  h2o.word2vec(words,
               vec_size = 300,
               window_size = 3,
               epochs = 10000)
model_path <-
  h2o.saveModel(object = w2v.model,
                path = "saved_data",
                force = TRUE)

load_model_path <- "saved_data/Word2Vec_model_R_1590962148619_1"
w2v.model <- h2o.loadModel(path = load_model_path)

# Sanity Check
h2o.findSynonyms(w2v.model, "rapidinhas", count = 10)
h2o.findSynonyms(w2v.model, "coronavirus", count = 10)

word_embedings <- as_tibble(h2o.toFrame(w2v.model))

h2o.shutdown()


# PCA Model ####
# Exploratory only. Bad results.
#
# pca_model <- prcomp(as.matrix(word_embedings[,2:301]),
#                     center = TRUE,
#                     scale. = TRUE)
#
# plot(pca_model, type = "l")
#
# pca_result <- as_data_frame(pca_model$x[,1:2])
# words_pca <- bind_cols(as_data_frame(words), pca_result)
# names(words_pca) <- c("Word", "X", "Y")
#
#
# clusters <- hdbscan(pca_result,
#                     minPts = 5)
#
# words_pca$clhdb <- clusters$cluster
#
# clusters <-  hclust(dist(scale(pca_result)),
#                     method = "centroid")
#
# words_pca$clhclust <- factor(cutree(clusters, k = 20))
#
#
# words_pca %>%
#   ggplot(aes(x = X, y = Y, color = as.factor(clhclust))) +
#   # geom_point(size = .5 ) +
#   geom_text(aes(label = Word),
#             position=position_jitter(width = .1,
#                                      height = .1),
#             size = 3,
#             fontface = "bold",
#             alpha = 1,
#             show.legend = FALSE) +
#   theme_tufte() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_rect(fill = "black")) +
#   scale_color_manual(values = palette)
#
# # PCA Model gave bad results, it needed many PCs to explain anything meaningful


# # Tsne Model ####
tsne_model_path <-
  posts_tbl_path <- paste0("saved_data/tsne_model_",
                           gsub("-", "", today()),
                           ".rds")
if (!file.exists(tsne_model_path)) {
  tsne_model <- Rtsne(
    as.matrix(word_embedings[, 2:301]),
    initial_dims = 300,
    perplexity = 22,
    theta = .05,
    max_iter = 20000 * 2,
    eta = 10
  )
  saveRDS(tsne_model, tsne_model_path)
} else {
  tsne_model <- read_rds(tsne_model_path)
}

words <- as.data.frame(word_embedings$Word)
tsne_result <- as.data.frame(tsne_model$Y)
words_tsne <- bind_cols(words, tsne_result)
names(words_tsne) <- c("Word", "X", "Y")

# Cluster with hclust
clusters <-  hclust(dist(scale(tsne_result)),
                    method = "centroid")

words_tsne$clhclust <- factor(cutree(clusters, k = 25))


# Plot hclust
tsne_plot <- words_tsne %>%
  ggplot(aes(
    x = X,
    y = Y,
    color = as.factor(clhclust),
    label = Word
  )) +
  geom_point(
    alpha = .15,
    size = 1,
    fill = "white",
    show.legend = FALSE
  ) +
  geom_text_repel(
    size = 3,
    fontface = "bold",
    alpha = 1,
    segment.alpha = .15,
    min.segment.length = 1.25,
    show.legend = FALSE,
    seed = 1234
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  scale_color_manual(values = palette)

ggsave(
  "plots/06-tsne.png",
  plot = tsne_plot,
  width = 21,
  height = 14.85,
  units = "cm",
  dpi = 300,
  scale = 2.1
)

# Rank per Group ####
rank_per_group <- words_tsne %>%
  left_join(
    posts_tbl_processed %>% # Get Count
      select(num_range("tag_", 1:5)) %>%
      gather() %>%
      select(value) %>%
      rename(word = value) %>%
      count(word, sort = TRUE) %>%
      filter(!is.na(word),
             word != ""),
    by = c("Word" = "word")
  ) %>%
  rename("Count" = "n") %>%
  select(Word, clhclust, Count) %>%
  filter(!is.na(Word)) %>%
  group_by(clhclust) %>%
  top_n(5, wt = Count) %>%
  ungroup() %>%
  arrange(clhclust, Count) %>%
  mutate(Order = row_number())

rank_tags_plot <- rank_per_group %>%
  ggplot(aes(
    x = Order,
    y = Count,
    fill = as.factor(clhclust)
  )) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(vars(clhclust),
             ncol = 5,
             scales = "free",
             drop = TRUE) +
  scale_x_continuous(
    breaks = rank_per_group$Order,
    labels = rank_per_group$Word,
    expand = c(0, 0)
  ) +
  coord_flip() +
  ggtitle("Rank das Tags Mais Utilizadas por Grupo\n") +
  theme_tufte() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = .5,
      size = 7
    ),
    axis.text.y = element_text(size = 8),
    axis.title = element_blank()
  ) +
  scale_fill_manual(values = palette)

ggsave(
  "plots/07-rank_tags2.png",
  plot = rank_tags_plot,
  width = 21,
  height = 14.85,
  units = "cm",
  dpi = 300,
  scale = 1.3
)

# Cover art ####
# Here I use count or frequency of tags to set size/alpha after some normalization
# and transformation
cover_plot_data <- words_tsne %>%
  left_join(
    posts_tbl_processed %>% # Get Count
      select(num_range("tag_", 1:5)) %>%
      gather() %>%
      select(value) %>%
      rename(word = value) %>%
      count(word, sort = TRUE) %>%
      filter(!is.na(word),
             word != ""),
    by = c("Word" = "word")
  ) %>%
  rename("Count" = "n") %>%
  arrange(desc(Count)) %>%
  # filter(Count <= 52) %>% #Q3 + (IQR * 1,5) = 25 +((25-7)*1,5)
  mutate(
    "Count_Norm" = (.$Count - min(.$Count,
                                  na.rm = TRUE)) / (max(.$Count,
                                                        na.rm = TRUE) -
                                                      min(.$Count,
                                                          na.rm = TRUE)),
    "Size" = ((Count_Norm ^ (1 / 8)) * 2) + 1,
    # "Size" = 1000000,
    "Alpha" = Count_Norm ^ (3 / 4) + .25
  )
cover_plot_data %>% skim()
cover_plot <- cover_plot_data %>%
  ggplot(aes(
    x = X,
    y = Y,
    color = as.factor(clhclust)
  )) +
  geom_text(
    aes(
      label = Word,
      size = Size,
      alpha = Alpha
    ),
    position = position_jitter(width = .1,
                               height = .1),
    fontface = "bold",
    show.legend = FALSE
  ) +
  theme_tufte() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "black", color = "white")
  ) +
  scale_color_manual(values = palette)

ggsave(
  "plots/00-cover.png",
  plot = cover_plot,
  width = 21,
  height = 14.85,
  units = "cm",
  dpi = 300,
  scale = 1.3
)

# Exploratory ####
tags_tbl <- as_tibble(tags)


editoria_13 <- tags_tbl %>%
  filter(
    str_detect(
      tags,
      "(?=.*t02neworder)|(?=.*saúde-mental)|(?=.*depressão)|(?=.*ansiedade)"
    )
  ) %>%
  pull(id)

posts_tbl_processed %>%
  filter(id %in% editoria_13) %>%
  select(id, total_clap_count, recommends) %>%
  arrange(desc(recommends, total_clap_count))

editoria_23 <- tags_tbl %>%
  filter(str_detect(tags, "(?=.*precisamosfalarsobrehiv)|(?=.*aids)|(?=.*hiv)")) %>%
  pull(id)

posts_tbl_processed %>%
  filter(id %in% editoria_23) %>%
  select(id, total_clap_count, recommends) %>%
  arrange(desc(recommends, total_clap_count))

editoria_3 <- tags_tbl %>%
  filter(str_detect(
    tags,
    "(?=.*whatsapp)|(?=.*rapidinhas)|(?=.*fotografia)|(?=.*tech)"
  )) %>%
  pull(id)

posts_tbl_processed %>%
  filter(id %in% editoria_3) %>%
  select(id, total_clap_count, recommends) %>%
  arrange(desc(recommends, total_clap_count))

# Things that helped ####
# https://github.com/h2oai/h2o-3/blob/master/h2o-r/demos/rdemo.word2vec.craigslistjobtitles.R
# https://www.r-bloggers.com/playing-with-dimensions-from-clustering-pca-t-sne-to-carl-sagan/