# Post 1
# Análise Descritiva

source('src/utils/utils.R')

library(gridExtra) #pra tabela
library(magrittr)
library(tidyverse)
library(skimr)
library(ggthemes)
library(lubridate)
library(scales)
library(gghighlight)


posts_tbl_processed <- read_rds("saved_data/posts_tbl_processed_20190101.rds")

posts_tbl_processed %>% skim()

################################################
# Total de posts por mês
posts_tbl_processed %>%
  group_by(day_published = as_date(floor_date(first_published_at, "1 month"))) %>%
  tally() %>% 
  ggplot(mapping = aes(x = day_published, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n, angle = 90), size = 2.75, nudge_y = 4.5, check_overlap = TRUE) +
  ggtitle("Total de posts por mês\n") +
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%Y"),
               expand = expand_scale(mult = c(0.01, 0.02))) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave("plots/01-Total-by-month.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Autores Únicos por mês
posts_tbl_processed %>%
  group_by(day_published = as_date(floor_date(first_published_at, "1 month"))) %>%
  summarise(count = n_distinct(user_id)) %>%
  ggplot(mapping = aes(x = day_published, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count, angle = 90), size = 2.75, nudge_y = 2, check_overlap = TRUE) +
  ggtitle("Autores únicos por mês\n") +
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%Y"),
               expand = expand_scale(mult = c(0.01, 0.02))) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = .5))

ggsave("plots/02-Authors-by-month.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)


# Média de posts por autor
posts_tbl_processed %>%
  group_by(day_published = as_date(floor_date(first_published_at, "1 month"))) %>%
  summarise(total_posts = n(),
            unique_authors = n_distinct(user_id),
            posts_per_author = total_posts / unique_authors) %>%
  gather(key, value, -day_published) %>%
  filter(key == "posts_per_author") %>%
  ggplot(mapping = aes(x = day_published, y = value)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value, digits = 2), angle = 90), 
            size = 2.75, 
            nudge_y = .18, 
            check_overlap = TRUE) +
  ggtitle("Média de Posts por Autorn") +
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%Y"),
               expand = expand_scale(mult = c(0.01, 0.02))) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = .5))

# Uso de Tags: Tags Count Hist, Tag mais Frequente

# Image Count hist
posts_tbl_processed %>%
  ggplot(aes(x = image_count)) +
  geom_histogram(bins = max(posts_tbl_processed$image_count)) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$image_count), 
                                  by = 5)) +
  labs(title = "Distribuição do Número de Imagens por Post",
       subtitle = "Esse gráfico mostra que a maioria dos posts tem uma única imagem") +
  theme_tufte() +
  theme(axis.title = element_blank())

# Links Count hist
# Curiosamente essa segue uma Zipf Distribution
posts_tbl_processed %>%
  ggplot(aes(x = links_count)) +
  geom_histogram(bins = max(posts_tbl_processed$links_count)) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$links_count), 
                                  by = 5)) +
  labs(title = "Distribuição do Número de Links por Post") +
  theme_tufte() +
  theme(axis.title = element_blank())

# Word Count
posts_tbl_processed %>% #filter(word_count>500 & word_count < 1250)
  ggplot(aes(x = word_count)) +
  geom_histogram(bins = 41) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$word_count), 
                                  by = 250)) +
  labs(title = "Distribuição do Número de Palavras por Post") +
  theme_tufte() +
  theme(axis.title = element_blank(),
  axis.text.x = element_text(angle = 90, hjust = 1))
  # gghighlight(word_count >= 778)

# Recommends Hist e explicação (unique clappers)
posts_tbl_processed %>% #filter(word_count>500 & word_count < 1250)
  ggplot(aes(x = recommends)) +
  geom_histogram(bins = 35) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$recommends), 
                                  by = 75)) +
  labs(title = "Distribuição de Recommends por Post") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
# Total Clap Count Hist
# Responses Created Count Hist


  
