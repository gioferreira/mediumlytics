# Post 1
# Análise Descritiva

source('src/utils/utils.R')

library(tidyverse)
library(magrittr)
library(skimr)
library(ggthemes)
library(gridExtra) #pra tabela
library(lubridate)
library(scales)
library(gghighlight)

# posts_tbl <- read_rds("saved_data/posts_tbl_20180710.rds")
# 
# max_mentioned_users <- max(posts_tbl$mentioned_users_count)
# names_mentioned_users_id <- c()
# for(i in 1:max_mentioned_users) names_mentioned_users_id[i] <- paste0("mentioned_user_id_", i)
# 
# posts_tbl_processed <- posts_tbl %>%
#   mutate(id = as_factor(id),
#          user_id = as_factor(user_id),
#          home_collection_id = as_factor(home_collection_id),
#          detected_language = as_factor(detected_language)) %>%
#   mutate(been_edited = ifelse((latest_published_at - first_published_at)>0, TRUE, FALSE)) %>%
#   rename(total_clap_count = totalClapCount,
#          tags = tags_unsplit,
#          mentioned_users_ids = mentioned_users_ids_unsplit) %>%
#   separate(tags,
#            into = c("tag_1", "tag_2", "tag_3", "tag_4", "tag_5"),
#            sep = ",",
#            convert = TRUE,
#            extra = "merge",
#            fill = "right") %>%
#   mutate(tag_1 = as_factor(tag_1),
#          tag_2 = as_factor(tag_2),
#          tag_3 = as_factor(tag_3),
#          tag_4 = as_factor(tag_4),
#          tag_5 = as_factor(tag_5)) %>%
#   select(-allow_responses,
#          -notify_followers,
#          -social_recommends_count) %>%
#   arrange_vars(c("been_edited" = 22))
# # SpareMatrix Mentioned_Users_Ids
# # separate(mentioned_users_ids,
# #          into = names_mentioned_users_id,
# #          sep = ",",
# #          convert = TRUE,
# #          fill = "right") %>%
# 
# 
# posts_tbl_processed %>% write_csv(path = "saved_data/posts_tbl_processed_20180928.csv")

posts_tbl_processed <- read_csv("saved_data/posts_tbl_processed_20180930.csv")

################################################
# Total de posts por mês
posts_tbl_processed %>%
  group_by(day_published = as_date(floor_date(first_published_at, "1 month"))) %>%
  filter(day_published > "2012-12-31",
         day_published < "2018-07-01") %>%
  tally() %>%
  # mutate(day_published = as_date(day_published)) %>%
  ggplot(mapping = aes(x = day_published, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n, angle = 90), size = 3.25, nudge_y = 4.5, check_overlap = TRUE) +
  ggtitle("Total de posts por mês") +
  scale_x_date(date_breaks = "3 months", labels = date_format("%b-%Y")) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))

# Autores Únicos por mês
posts_tbl_processed %>%
  group_by(day_published = as_date(floor_date(first_published_at, "1 month"))) %>%
  filter(day_published > "2012-12-31",
         day_published < "2018-07-01") %>%
  summarise(count = n_distinct(user_id)) %>%
  ggplot(mapping = aes(x = day_published, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count, angle = 90), size = 3.25, nudge_y = 2, check_overlap = TRUE) +
  ggtitle("Autores únicos por mês") +
  scale_x_date(date_breaks = "3 months", labels = date_format("%b-%Y")) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))


# Média de posts por autor
posts_tbl_processed %>%
  group_by(day_published = as_date(floor_date(first_published_at, "1 month"))) %>%
  filter(day_published > "2012-12-31",
         day_published < "2018-07-01") %>%
  summarise(total_posts = n(),
            unique_authors = n_distinct(user_id),
            posts_per_author = total_posts / unique_authors) %>%
  gather(key, value, -day_published) %>%
  filter(key == "posts_per_author") %>%
  ggplot(mapping = aes(x = day_published, y = value)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value, digits = 2), angle = 90), 
            size = 3.25, 
            nudge_y = .22, 
            check_overlap = TRUE) +
  ggtitle("Média de Posts por Autor") +
  scale_x_date(date_breaks = "3 months", labels = date_format("%b-%Y")) +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))

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


  
