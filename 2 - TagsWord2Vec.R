# Post 2
# Word2Vec Tags

# From: https://github.com/h2oai/h2o-3/blob/master/h2o-r/demos/rdemo.word2vec.craigslistjobtitles.R

source('src/utils/utils.R')

library(magrittr)
library(tidyverse)
library(skimr)
library(ggthemes)
library(lubridate)
library(scales)
library(tidytext)
library(wordcloud)
library(h2o)
library(Rtsne)
library(dbscan)

palheta <- c("#a0e85b", "#d725a3", "#36e515", "#7f2157", "#7ee8c0", "#fe1d66", "#5c922f", "#7835d3", "#e9d737", "#1c4bb4", "#f39450", "#154e56", "#e4ccf1", "#76480d", "#48b6ea", "#ae3028", "#fd92fa", "#115205", "#628df2", "#dbc58e")

posts_tbl_processed <- read_rds("saved_data/posts_tbl_processed_20190101.rds")

posts_tbl_processed %>% skim()

h2o.init()

tags <- posts_tbl_processed %>%
  select(num_range("tag_", 1:5)) %>%
  mutate_if(is.factor, as.character) %>%
  replace_na(list(tag_1 = "",
                  tag_2 = "",
                  tag_3 = "",
                  tag_4 = "",
                  tag_5 = "")) %>%
  unite(tags, sep= " ", remove = TRUE) %>%
  mutate(tags = str_squish(tags)) %>%
  as.h2o(destination_frame = "tags")

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

w2v.model <- h2o.word2vec(words, vec_size = 300, window_size = 3, epochs = 10000)

model_path <- h2o.saveModel(object = w2v.model, path = "saved_data", force = TRUE)

load_model_path <- "saved_data/Word2Vec_model_R_1548102206983_3"

w2v.model <- h2o.loadModel(path = load_model_path)

print(h2o.findSynonyms(w2v.model, "os-dez-mandamentos", count = 5))

word_embedings <- h2o.toFrame(w2v.model)

tsne_model <- Rtsne(as.matrix(word_embedings[,2:101]),
                    initial_dims = 300,
                    perplexity = 24,
                    theta = .05,
                    max_iter = 20000*2,
                    eta = 5)

saveRDS(tsne_model, "saved_data/tsne_model-20190121-c.rds")

tsne_model <- read_rds("saved_data/tsne_model-20190121-c.rds")

words <- as.data.frame(word_embedings$Word)
tsne_result <- as.data.frame(tsne_model$Y)
words_tsne <- bind_cols(words, tsne_result)  
names(words_tsne) <- c("Word", "X", "Y")

clusters <- hdbscan(tsne_result, 
                    minPts = 5)
words_tsne$clhdb <- clusters$cluster

clusters <-  hclust(dist(scale(tsne_result)),
                    method = "ward.D2")

words_tsne$clhclust <- factor(cutree(clusters, k = 20))


words_tsne %>%
  ggplot(aes(x = X, y = Y, color = as.factor(clhclust))) +
  # geom_point(size = .5 ) +
  geom_text(aes(label = Word),
            position=position_jitter(width = .1,
                                     height = .1),
            size = 3,
            fontface = "bold",
            alpha = 1,
            show.legend = FALSE) +
  theme_tufte() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "black")) +
  scale_color_manual(values = palheta)
  





# scale_color_brewer(type = "qual", palette = "Paired")



