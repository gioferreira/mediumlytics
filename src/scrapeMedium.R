#Scrape Medium
library(tidyverse)
library(httr)
library(jsonlite)
library(data.tree)
library(tokenizers)
library(doParallel)
library(lubridate)
library(furrr)

make_first_req <- function(starting_url,
                           limit = 50){
  body <- paste0('{"limit":', limit,'}')
  first_req <- GET(url = starting_url,
                   config = c(add_headers("x-xsrf-token" = "2",
                                          "content-type" = "application/json",
                                          "accept" = "application/json"), 
                              accept_json()),
                   body = body)
  return(first_req)
  
}

extract_content <- function(first_req){
  content_json <- fromJSON(substring(rawToChar(first_req$content), 17))
  return(content_json)
}

parse_first <- function(content_json, 
                        scrape_url = "https://medium.com/neworder"){
  url_tbl <- tibble(id = character(),
                    unique_slug = character(),
                    url = character())
  if (length(content_json$payload$posts$id) != 0){
    for (i in 1:length(content_json$payload$posts$id)){
      url_tbl[i, 'id'] <- content_json$payload$posts$id[[i]]
      url_tbl[i, 'unique_slug'] <- content_json$payload$posts$uniqueSlug[[i]]
    }
    url_tbl$url <- NA
    url_tbl$url[url_tbl$unique_slug != ''] <- paste(scrape_url, url_tbl$unique_slug[url_tbl$unique_slug != ''], sep="/")
    url_tbl$url[url_tbl$unique_slug == ''] <- paste(scrape_url, url_tbl$id[url_tbl$unique_slug == ''], sep ="/")
  }
  return(url_tbl)
}

parse_more <- function(json_more, 
                       scrape_url){
  url_tbl <- tibble(id = character(),
                    unique_slug = character(),
                    url = character())
  if (length(json_more$payload$value$id) != 0){
    for (i in 1:length(json_more$payload$value$id)){
      url_tbl[i, 'id'] <- json_more$payload$value$id[[i]]
      url_tbl[i, 'unique_slug'] <- json_more$payload$value$uniqueSlug[[i]]
    }
    url_tbl$url <- NA
    url_tbl$url[url_tbl$unique_slug != ''] <- paste(scrape_url, url_tbl$unique_slug[url_tbl$unique_slug != ''], sep="/")
    url_tbl$url[url_tbl$unique_slug == ''] <- paste(scrape_url, url_tbl$id[url_tbl$unique_slug == ''], sep ="/")
  }
  return(url_tbl)
}

scrape_more <- function(content_json,
                        ...){
  url_tbl <- tibble(id = character(),
                    unique_slug = character(),
                    url = character())
  if(content_json$success == TRUE){
    paging <- content_json$payload$paging$'next'$to
    collection_id <- content_json$payload$collection$id
    collection_slug <- content_json$payload$collection$slug
  } else{
    stop(content_json$error)
  }
  url_load_more <- paste0('https://medium.com/', collection_slug, "/load-more?sortBy=latest")
  repeat{
    req_more <- POST(url = url_load_more,
                     config = c(add_headers("origin" = scrape_url,
                                            "accept-encoding" = "gzip, deflate, br",
                                            "accept-language" = "en-US,en;q=0.8,pt;q=0.6",
                                            "x-obvious-cid" = "web",
                                            "x-client-date" = "1499524261784",
                                            "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36",
                                            "x-xsrf-token" = "1",
                                            "content-type" = "application/json",
                                            "accept" = "application/json",
                                            "referer" = paste0(scrape_url, "/latest"),
                                            "authority" = "medium.com"), 
                                accept_json()),
                     body = paste0('{"limit":50,"to":', paging, '}')
    )
    json_more <- extract_content(req_more)
    if(json_more$success == TRUE){
      url_tbl <- rbind(url_tbl, parse_more(json_more, "https://medium.com/neworder"))
      paging <- json_more$payload$paging$'next'$to
      message(length(json_more$payload$value$id), " links adicionados ao Scrape")
    } else{
      message(json_more$error)
      break
    }
  }
  return(url_tbl)
}

download_jsons <- function(url_tbl,
                           scrape_url,
                           save_folder = 'jsons'){
  if (!dir.exists(save_folder)){
    dir.create(save_folder)
  }
  url_tbl$downloaded <- FALSE
  expire <- ymd_hms(Sys.time()) + years(1)
  expires <- paste0(as.character(wday(expire, label = TRUE)),
                    ", ",
                    day(expire),
                    " ",
                    as.character(month(expire, label = TRUE)),
                    " ",
                    year(expire),
                    " ",
                    hour(expire),
                    ":",
                    minute(expire),
                    ":",
                    second(expire),
                    " ",
                    tz(expire))
  for (i in 1:length(url_tbl$url)) {
    file_name <- paste0(url_tbl$id[[i]], ".json")
    full_path <- paste(save_folder, file_name, sep = "/")
    if (!file.exists(full_path)){
      h_post <- handle(as.character(url_tbl$url[[i]]))
      doc.raw <- GET(url = paste( scrape_url, url_tbl$id[i], sep = "/" ),
                     config = c(add_headers("origin" = scrape_url,
                                            "accept-encoding" = "gzip, deflate, br",
                                            "accept-language" = "en-US,en;q=0.8,pt;q=0.6",
                                            "x-obvious-cid" = "web",
                                            "x-client-date" = "1499524261784",
                                            "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36",
                                            "x-xsrf-token" = "1",
                                            "content-type" = "application/json",
                                            "accept" = "application/json",
                                            "referer" = paste0(scrape_url, "/latest"),
                                            "authority" = "medium.com"), 
                                set_cookies("__cfduid" = "d530a315067ebf6b508cf0f6a5f081a541532467146",
                                            "path" = "/",
                                            "domain" = ".medium.com",
                                            "uid" = "lo_t4tgb4BNFfk4",
                                            "Domain" = ".medium.com",
                                            "sid" = "1:H9nLnIx+ymDAPVe0fniJDhd2k8to6DkKfNisQTH40RUF7I8GvUFDEkmGgQ/6XjrR",
                                            "expires" = expires),
                                accept_json()),
                     encode="multipart",
                     handle=h_post)
      if(doc.raw$status_code == 200){
        json.raw <- fromJSON(substring(rawToChar(doc.raw$content), 17))
        write(toJSON(json.raw), file = full_path)
        url_tbl$downloaded[[i]] <- TRUE
        message(paste(i, "of", length(url_tbl$url), "json files saved"))
      } else {
        message(paste(url_tbl$url[[i]], "returned status:", doc.raw$status_code))
      }
    } else {
      url_tbl$downloaded[[i]] <- TRUE
    }
  }
  return(url_tbl)
}

create_json_list <- function(folder){
  files <- dir(folder, full.names = TRUE)
  json_list <- map(files, fromJSON, simplifyDataFrame = FALSE)
  return(json_list)
}

is_list_vec <-  function(json_list_element){
  lists_vec <- logical()
  for (i in 1:length(json_list_element)){
    ifelse(is_list(json_list_element[[i]]), lists_vec[i] <- FALSE, lists_vec[i] <- TRUE)
  }
  return(lists_vec)
}

make_date <- function(x){
  date <- as.POSIXct(x/1000, origin = "1970-01-01")
  return(date)
}

contains_string <- function(node,
                             string){
  if (string %in% node$fields) return(TRUE)
  else return(FALSE)
}

contains_stringYN <- function(node,
                              stringYes,
                              stringNo){
  if ((stringYes %in% node$fields) && !(stringNo %in% node$fields)) return(TRUE)
  else return(FALSE)
}

contains_stringYY <- function(node,
                              stringA,
                              stringB){
  if ((stringA %in% node$fields) && (stringB %in% node$fields)) return(TRUE)
  else return(FALSE)
}

remove_empty <- function(x){
  if (is_list(x) && length(x) > 0) {
    lapply(x, remove_empty)
  } else if (is_character(x) && nchar(x) == 0) {
    return(0)
  } else return(x)
}

process_json <- function(json_list_element){
  if (json_list_element$success == TRUE){
    clean_json <- remove_empty(json_list_element)
    suppressWarnings(
      value_node <- as.Node(clean_json$payload)
      ) 
    post_tbl <- tibble(id                          = FindNode( value_node, 'value' )$id,
                       user_id                     = FindNode( value_node, 'SocialStats' )$Get( 'userId', 
                                                                                                filterFun = isLeaf ),
                       users_followed_by_count     = FindNode( value_node, 'SocialStats' )$Get( 'usersFollowedByCount', 
                                                                                                filterFun = isLeaf ),
                       users_followed_count        = FindNode( value_node, 'SocialStats' )$Get( 'usersFollowedCount', 
                                                                                                filterFun = isLeaf ),
                       home_collection_id          = FindNode( value_node, 'value' )$homeCollectionId,
                       title                       = FindNode( value_node, 'value' )$title,
                       title_word_count            = ifelse( (is_character(FindNode( value_node, 'value' )$title)),
                                                             length( tokenize_words( FindNode( value_node, 'value' )$title)[[1]] ), 0),
                       subtitle                    = FindNode( value_node, 'virtuals' )$subtitle,
                       subtitle_word_count         = ifelse( (is_character(FindNode( value_node, 'virtuals' )$subtitle)),
                                                             length( tokenize_words( FindNode( value_node, 'virtuals' )$subtitle)[[1]] ), 0),
                       word_count                  = FindNode( value_node, 'virtuals' )$wordCount,
                       image_count                 = FindNode( value_node, 'virtuals' )$imageCount,
                       allow_notes                 = FindNode( value_node, 'virtuals' )$allowNotes,
                       links_count                 = FindNode( value_node, 'entries' )$count,
                       section_count               = FindNode( value_node, 'virtuals' )$sectionCount,
                       reading_time                = FindNode( value_node, 'virtuals' )$readingTime,
                       allow_responses             = FindNode( value_node, 'value' )$allowResponses,
                       notify_followers            = FindNode( value_node, 'value' )$notifyFollowers,
                       notify_twitter              = FindNode( value_node, 'value' )$notifyTwitter,
                       notify_facebook             = FindNode( value_node, 'value' )$notifyFacebook,
                       is_series                   = FindNode( value_node, 'value' )$isSeries,
                       detected_language           = FindNode( value_node, 'value' )$detectedLanguage,
                       first_published_at          = make_date( FindNode( value_node, 'value' )$firstPublishedAt ),
                       latest_published_at         = make_date( FindNode( value_node, 'value' )$latestPublishedAt ),
                       paragraphs_count            = FindNode( value_node, 'paragraphs' )$count,
                       paragraphs_type_unsplit     = paste(FindNode( value_node, 'paragraphs' )$Get( 'type',
                                                                                                     filterFun = function(x) {
                                                                                                       contains_stringYN(x, 
                                                                                                                         stringYes = 'type',
                                                                                                                         stringNo = 'end')
                                                                                                     }), 
                                                           collapse = ','),
                       text                        = paste(FindNode( value_node, 'paragraphs' )$Get( 'text',
                                                                                                     filterFun = function(x) {
                                                                                                       contains_string(x,
                                                                                                                       string = 'text')
                                                                                                     }), 
                                                           collapse = '\n'),
                       tags_count                  = FindNode( value_node, 'tags')$count,
                       tags_unsplit                = paste(FindNode( value_node, 'tags' )$Get( 'slug', 
                                                                                               filterFun = function(x) {
                                                                                                 contains_string( x, string = 'slug')
                                                                                               }), 
                                                           collapse = ","),
                       tags_post_count_unsplit     = paste(FindNode( value_node, 'tags' )$Get( 'postCount', 
                                                                                               filterFun = function(x) {
                                                                                                 contains_stringYY( x, stringA = 'postCount', 
                                                                                                                    stringB = 'slug')
                                                                                               }), 
                                                           collapse = ","),
                       tag_follower_count_unsplit  = paste(FindNode( value_node, 'tags' )$Get( 'followerCount', 
                                                                                               filterFun = function(x) {
                                                                                                 contains_stringYY( x, stringA = 'postCount', stringB = 'followerCount')
                                                                                               }), 
                                                           collapse = ","),
                       mentioned_users_count       = FindNode( value_node, 'mentionedUsers' )$count,
                       mentioned_users_ids_unsplit = paste(FindNode( value_node, 'mentionedUsers' )$Get( 'userId', 
                                                                                                         filterFun = isLeaf), 
                                                           collapse = ","),
                       social_recommends_count     = FindNode( value_node, 'virtuals')$socialRecommendsCount,
                       responses_created_count     = FindNode( value_node, 'virtuals')$responsesCreatedCount,
                       recommends                  = FindNode( value_node, 'virtuals')$recommends,
                       totalClapCount              = FindNode( value_node, 'virtuals')$totalClapCount
                       )
  
  return(post_tbl)
  }
}

process_json_list <- function(json_list){
  posts_tbl <- map(json_list, process_json)
  return(posts_tbl)
}

par_process_json_list <- function(json_list){
  no_cores <- detectCores() - 1
  registerDoParallel(cores = no_cores)
  cl <- makeCluster(no_cores)
  posts_tbl <- foreach(i = 1:length(json_list),
                       .export = c("remove_empty", "process_json_list"),
                       .packages = c("tidyverse", "data.tree", "tokenizers")) %dopar%
    process_json(json_list[[i]])
  stopCluster(cl)
  return(posts_tbl)
}

furrr_process_json_list <- function(json_list){
  plan(multiprocess)
  
  posts_tbl <- future_map(json_list, process_json, .progress = TRUE)
  
  return(posts_tbl)
  
}