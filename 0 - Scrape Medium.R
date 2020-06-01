library(tidyverse)
library(magrittr)
library(skimr)
source('src/scrapeMedium.R')
source('src/utils/utils.R')


# Generate URL_TBL, a list of all Posts URL's from the publication ####
starting_url <- 'https://medium.com/neworder/latest'
scrape_url <- "https://medium.com/neworder"

first_req <- make_first_req(starting_url)
content_json <- extract_content(first_req)

url_tbl_path <- paste0("saved_data/url_tbl_",
                       gsub("-", "", today()),
                       ".rds")

if (!file.exists(url_tbl_path)) {
  url_tbl <- parse_first(content_json, scrape_url = scrape_url)
  url_tbl <- rbind(url_tbl, scrape_more(content_json, scrape_url))
  write_rds(url_tbl, url_tbl_path)
} else {
  (url_tbl <- read_rds(url_tbl_path))
}

# Download missing jsons to folder "jsons" ####
# Last Download 2020 05 31
# I used only the id + scrapeurl for the download json URL
# But I could use the real url with this "remove accents" line:
# gsub("(`|\\'|~|\\^)", "",iconv(url_tbl$url[[i]], to="ASCII//TRANSLIT"))


url_tbl <- download_jsons(url_tbl,
                          scrape_url,
                          "jsons")


# Load each JSON to a list ####
json_list_path <- paste0("saved_data/json_list_",
                         gsub("-", "", today()),
                         ".rds")

if (!file.exists(json_list_path)) {
  json_list <- create_json_list("jsons")
  write_rds(json_list, json_list_path)
} else {
  json_list <- read_rds(json_list_path)
}

# Process all JSONs to extract data and create tbl ####
posts_tbl_path <- paste0("saved_data/posts_tbl_",
                         gsub("-", "", today()),
                         ".rds")

if (!file.exists(posts_tbl_path)) {
  # posts_list <- process_json_list(json_list)
  # posts_list <- par_process_json_list(json_list)
  # Best Approach is using Furrr
  posts_list <- furrr_process_json_list(json_list)
  posts_tbl <- as_tibble(do.call(rbind, posts_list))
  write_rds(posts_tbl, posts_tbl_path)
} else {
  posts_tbl <- read_rds(posts_tbl_path)
}


# Initial Pre-Process ####

posts_tbl_processed_path <-
  paste0("saved_data/posts_tbl_processed_",
         gsub("-", "", today()),
         ".rds")

if (!file.exists(posts_tbl_processed_path)) {
  # max_mentioned_users <- max(posts_tbl$mentioned_users_count)
  # names_mentioned_users_id <- c()
  # for (i in 1:max_mentioned_users) names_mentioned_users_id[i] <- paste0("mentioned_user_id_", i)
  posts_tbl_processed <- posts_tbl %>%
    mutate(
      id = as_factor(id),
      user_id = as_factor(user_id),
      home_collection_id = as_factor(home_collection_id),
      detected_language = as_factor(detected_language)
    ) %>%
    mutate(been_edited = ifelse((
      latest_published_at - first_published_at
    ) > 0, TRUE, FALSE)) %>%
    rename(
      total_clap_count = totalClapCount,
      tags = tags_unsplit,
      mentioned_users_ids = mentioned_users_ids_unsplit
    ) %>%
    separate(
      tags,
      into = c("tag_1", "tag_2", "tag_3", "tag_4", "tag_5"),
      sep = ",",
      convert = TRUE,
      extra = "merge",
      fill = "right"
    ) %>%
    mutate(
      tag_1 = as_factor(tag_1),
      tag_2 = as_factor(tag_2),
      tag_3 = as_factor(tag_3),
      tag_4 = as_factor(tag_4),
      tag_5 = as_factor(tag_5)
    ) %>%
    select(-allow_responses,
           -notify_followers,
           -social_recommends_count) %>%
    arrange_vars(c("been_edited" = 22))
  # SpareMatrix Mentioned_Users_Ids
  # separate(mentioned_users_ids,
  #          into = names_mentioned_users_id,
  #          sep = ",",
  #          convert = TRUE,
  #          fill = "right") %>%
  write_rds(posts_tbl_processed, posts_tbl_processed_path)
} else {
  posts_tbl_processed <- read_rds(posts_tbl_processed_path)  
}

# First Exploration ####
skim(posts_tbl_processed)
