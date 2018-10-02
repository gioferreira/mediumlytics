# Topic Modelling

source('src/utils/utils.R')
library(stm)
library(tidyverse)
library(skimr)
library(lubridate)

posts_tbl_processed <- read_csv("saved_data/posts_tbl_processed_20180930.csv")

posts_txts <- posts_tbl_processed %>%
  select(id,
         user_id,
         first_published_at,
         text) %>%
  mutate(first_published_at = floor_date(first_published_at, "days"),
         day_published = as.integer((first_published_at + days(1)) - min(first_published_at))) %>%
  select(-first_published_at) %>%
  arrange_vars(c("day_published" = 3))

