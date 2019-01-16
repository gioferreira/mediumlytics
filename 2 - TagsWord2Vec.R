# Post 2
# Word2Vec Tags

source('src/utils/utils.R')

library(magrittr)
library(tidyverse)
library(skimr)
library(ggthemes)
library(lubridate)
library(scales)
library(tidytext)
library(wordcloud)
library(h2o0)


posts_tbl_processed <- read_rds("saved_data/posts_tbl_processed_20190101.rds")

posts_tbl_processed %>% skim()
