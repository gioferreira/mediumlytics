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
library(tidytext)
library(wordcloud)


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
  ggtitle("Média de Posts por Autor\n") +
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%Y"),
               expand = expand_scale(mult = c(0.01, 0.02))) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = .5))

ggsave("plots/03-Posts-per-author.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Word Count
posts_tbl_processed %>% #filter(word_count>500 & word_count < 1250)
  ggplot(aes(x = word_count)) +
  geom_histogram(binwidth = 250) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$word_count), 
                                  by = 250)) +
  labs(title = "Distribuição do Número de Palavras por Texto\n") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("plots/04-word_count_hist.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Idioma
posts_tbl_processed %>%
  group_by(detected_language) %>% 
  tally() %>%
  mutate(detected_language = fct_recode(detected_language,
                                        "Português" = "pt",
                                        "Desconhecido" = "un",
                                        "Inglês" = "en",
                                        "Francês" = "fr"),
         detected_language = fct_reorder(detected_language,
                                         n)) %>%
  ggplot(aes(x = detected_language, y = n)) +
  geom_col() +
  geom_text(aes(label = n), 
            size = 2.75, 
            nudge_y = 20, 
            check_overlap = TRUE,
            hjust = 0) +
  coord_flip() +
  labs(title = "Idiomas dos Textos\n") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5))

ggsave("plots/05-languages.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Image Count hist
posts_tbl_processed %>%
  ggplot(aes(x = image_count)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$image_count), 
                                  by = 5)) +
  labs(title = "Distribuição do Número de Imagens por Texto\n") +
  theme_tufte() +
  theme(axis.title = element_blank())

ggsave("plots/06-image_count.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Links Count
posts_tbl_processed %>%
  ggplot(aes(x = links_count)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$links_count), 
                                  by = 3)) +
  labs(title = "Distribuição do Número de Links por Texto\n") +
  theme_tufte() +
  theme(axis.title = element_blank())

ggsave("plots/07-links_count.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Total Clap Count Hist
posts_tbl_processed %>%
  ggplot(aes(x = total_clap_count)) +
  geom_histogram(binwidth = 50) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$total_clap_count), 
                                  by = 500)) +
  labs(title = "Distribuição do Número de Palmas por Texto\n") +
  theme_tufte() +
  theme(axis.title = element_blank())

ggsave("plots/08-total_clap_count.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Total Clap Count Hist filtered
posts_tbl_processed %>%
  filter(total_clap_count <= 167) %>%
  ggplot(aes(x = total_clap_count)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$total_clap_count), 
                                  by = 100)) +
  labs(title = "Distribuição do Número de Palmas por Texto",
       subtitle = "Considerando apenas textos com menos de 500 Palmas") +
  theme_tufte() +
  theme(axis.title = element_blank())

ggsave("plots/08b-total_clap_count_filtered.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

### total clap over time 
start_clap <- ymd("2017-08-01")

posts_tbl_processed %>%
  # filter(total_clap_count <= 167) %>%
  group_by(day_published = as_date(floor_date(first_published_at, "1 month"))) %>%
  summarise(mean_clap_count = mean(total_clap_count),
            sum_clap_count = sum(total_clap_count)) %>%
  gather(key, value, -day_published) %>%
  mutate(key = fct_recode(key,
                           "Média de Palmas" = "mean_clap_count",
                           "Soma das Palmas" = "sum_clap_count")) %>%
  ggplot(aes(x = day_published, y = value, linetype = key)) +
  geom_line(show.legend = FALSE) +
  facet_grid(rows = vars(key), scales = "free") +
  geom_vline(xintercept = start_clap) +
  labs(title = "Média e Soma da Palmas Agrupadas por Mês") +
  theme_tufte() +
  theme(axis.title = element_blank())

ggsave("plots/11-claps_over_time.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)


# Recommends Hist e explicação (unique clappers)
posts_tbl_processed %>% #filter(recommends <= 300) %>% #filter(word_count>500 & word_count < 1250)
  ggplot(aes(x = recommends)) +
  geom_histogram(binwidth = 50) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$recommends), 
                                  by = 500)) +
  labs(title = "Distribuição de \"Palmas Únicas\" por Post",
       subtitle = "Aqui considero o máximo de 1 palma por usuário") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("plots/12-recommends_full.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Recommends Hist Filtered p75
posts_tbl_processed %>% 
  filter(recommends <= 200) %>% 
  ggplot(aes(x = recommends)) +
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 
                                  200, 
                                  by = 5)) +
  labs(title = "Distribuição de \"Palmas Únicas\" por Post",
       subtitle = "Aqui considero o máximo de 1 palma por usuário, filtradas por recommends <= 200") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

ggsave("plots/12b-recommends_filtered.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)


# Responses Created Count Hist
posts_tbl_processed %>% 
  # filter(responses_created_count <= 200) %>% 
  ggplot(aes(x = responses_created_count)) +
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 
                                  max(posts_tbl_processed$responses_created_count), 
                                  by = 5)) +
  labs(title = "Distribuição de Respostas por Post") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

ggsave("plots/13-responses_full.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)

# Responses Created Count Hist Filtered
posts_tbl_processed %>% 
  filter(responses_created_count <= 25) %>%
  ggplot(aes(x = responses_created_count)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 
                                  25, 
                                  by = 1)) +
  labs(title = "Distribuição de Respostas por Post",
       subtitle = "Filtradas por respostas <= 25") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

ggsave("plots/13-responses_filtered.png",
       width = 21,
       height = 14.85,
       units = "cm",
       dpi = 300)
