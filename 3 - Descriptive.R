# Descrptive Analysis

# Loading Stuff ####
source('src/utils/utils.R')

library(tidyverse)
library(magrittr)
library(skimr)
library(lubridate)
library(scales)
library(tidytext)
library(wordcloud)
library(ggthemes)

posts_tbl <- read_rds("saved_data/posts_tbl_topics-20190829.rds")
topic_labels <- read_rds("saved_data/topic_labels.rds")


posts_tbl %<>% 
  rename("topic_label" = "label") %>% 
  filter(!is.na(topic_label)) %>% 
  mutate_if(is_character, list(~na_if(., "NA"))) %>% 
  rename(Topic_01 = Topic_1,
         Topic_02 = Topic_2,
         Topic_03 = Topic_3,
         Topic_04 = Topic_4,
         Topic_05 = Topic_5,
         Topic_06 = Topic_6,
         Topic_07 = Topic_7,
         Topic_08 = Topic_8,
         Topic_09 = Topic_9,)

posts_tbl %>% 
  skim()

add_topic_labels <- function(posts_tbl, topic_labels) {
  out <- posts_tbl %>% 
    mutate(prevalent_topic = paste("Topic", prevalent_topic)) %>%
    inner_join(
      topic_labels %>%
        mutate(topic = paste("Topic", topic)),
      by = c("prevalent_topic" = "topic")) %>% 
    mutate(prevalent_topic = as_factor(prevalent_topic))
  return(out)
}

posts_tbl %>% 
  # filter(first_published_at <= "2019-08-01") %>%
  group_by(
    day_published = as_date(floor_date(first_published_at, "3 months")),
    topic_label) %>%
  tally() %>% 
  ggplot(aes(x = day_published, y = n, color = topic_label)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(facets = vars(topic_label)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y") +
  theme_tufte(ticks = FALSE) +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 13),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45,
                               hjust = 1),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_line(linetype = "dotted",
                                      color = "grey")) +
  labs(
    x = NULL, y = NULL,
    title = "Count of posts by prevalent topic over time",
    subtitle = "Grouped at each 3 months") +
  # geom_smooth(
  #   show.legend = FALSE,
  #   se = TRUE,
  #   linetype = "dotted",
  #   size = .5,
  #   color = "black",
  #   alpha = .2) +
  coord_cartesian(ylim = c(0,35)) +
  geom_hline(
    yintercept = 0,
    # linetype = "dotted",
    color = "grey")





# NOTES ####
posts_tbl %>% 
  select(id, prevalent_topic, topic_label) %>% 
  group_by(prevalent_topic, topic_label) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(prevalent_topic = paste("Topic", prevalent_topic),
         prevalent_topic = fct_reorder(prevalent_topic, n)) %>% 
  ggplot(aes(prevalent_topic, n,label = topic_label, fill = prevalent_topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 1, size = 3, color = rgb(.3, .3, .3)) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 300)) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  labs(x = NULL, y = expression("Count"),
       title = "Prevalent Topic by Count in New Order corpus",
       subtitle = "With Topic Labels")

# O que estou achando curioso aqui é a alteração de posição de alguns.
# Talvez eu precise entender melhor a métrica de prevalência original 
# Para explicar por que isso acontece...eu acho que o esperado era ser igual
# Mas a aparência geral se mantém, acho que está tudo certo.