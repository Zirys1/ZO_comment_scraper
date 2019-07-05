# Extracting comments (and information) given a profile

library(tidytext)
library(dplyr)
library(lubridate)
library(rvest)
library(xlsx)

url <- "https://profile.zeit.de/2483981"

#### Get the links to get all comments based on base url ####
get_page_urls <- function(url){
  html_document <- NULL
  try(html_document <- read_html(url))
  
  #### Strip number of comments ####
  # make xpath conditional on whether there are "Redaktionsempfehlungen" or not
  n_comments_xpath <- ".user-profile__statistics+ .user-profile__statistics"
  n_comments <- html_document %>%
    html_node(css = n_comments_xpath) %>%
    html_text(trim = T) %>% 
    gsub("\\ .*$", "", ., perl = T) %>%   # delete everything after including whitespace
    gsub(".", "", ., fixed = T) %>% # to use dots as commas (because its a German site)
    as.numeric(.)
  
  if(is.na(n_comments)){
    n_comments_xpath <- ".user-profile__statistics"
    n_comments <- html_document %>%
      html_node(css = n_comments_xpath) %>%
      html_text(trim = T) %>% 
      gsub("\\ .*$", "", ., perl = T) %>%   # delete everything after including whitespace
      gsub(".", "", ., fixed = T) %>% # to use dots as commas (because its a German site)
      as.numeric(.)
  }
  
  ## Create urls based on the number of comments and the fact that max. 10 are on each page ##
  n_pages <- ceiling(n_comments/10)
  pages <- seq(1, n_pages, 1)
  
  urls <- paste0(url, "?p=", pages )
  return(urls)
}


scrape_zo_article <- function(url) {
  html_document <- NULL
  try(html_document <- read_html(url))
  if(!exists("html_document") | is.null(html_document)){
    
    article <- data.frame(
      url = url,
      user_name = NA,
      n_comments = NA,
      title_comment = NA,
      date_comment = NA,
      link_comment = NA,
      comment = NA,
      fail = 1
    )
    
    return(article)
  }
  
  
  
  #### Strip number of comments ####
  n_comments_xpath <- ".user-profile__statistics+ .user-profile__statistics"
  n_comments <- html_document %>%
    html_node(css = n_comments_xpath) %>%
    html_text(trim = T) %>% 
    gsub("\\ .*$", "", ., perl = T) %>%   # delete everything after including whitespace
    gsub(".", "", ., fixed = T) %>% # to use dots as commas (because its a German site)
    as.numeric(.)
  
  if(is.na(n_comments)){
    n_comments_xpath <- ".user-profile__statistics"
    n_comments <- html_document %>%
      html_node(css = n_comments_xpath) %>%
      html_text(trim = T) %>% 
      gsub("\\ .*$", "", ., perl = T) %>%   # delete everything after including whitespace
      gsub(".", "", ., fixed = T) %>% # to use dots as commas (because its a German site)
      as.numeric(.)
  }
  
  
  #### Strip username ####
  user_name_xpath <- "title"
  user_name <- html_document %>%
    html_node(css = user_name_xpath) %>%
    html_text(trim = T) %>% 
    gsub("\\|.*$", "", ., perl = T) %>%  # delete everything after including |
    gsub("[ \t]+$", "", .) %>%  # strip trailing whitespace
    as.character(.)
  
  
  ### Strip headers of comments ####   
  title_comment_xpath <- "h3 .user-comment__link , .user-comment__kicker"
  title_comment <- html_document %>%
    html_nodes(css = title_comment_xpath) %>%
    html_text(trim = T) %>%
    paste0(sep = "\n")
  # delete every second to avoid doubling
  n <- length(title_comment) 
  title_comment <- title_comment[seq(1, n, 2)]
  title_comment <- as.character(title_comment)
  
  
  #### Strip date of comments ####
  date_comment_xpath <- ".user-comment__date"
  date_comment <- html_document %>%
    html_nodes(css = date_comment_xpath) %>%
    html_text(trim = T) %>%
    paste0(sep = "\n") %>% 
    gsub("\n", "", .)
  
  
  # deal with things like "two days ago"
  #  if(length(grep("Tag", date_comment1)) != 0){
  #    if(grep("vor 2 Tagen", date_comment1)){
  #      r <- today() - 2
  #      d_sub1 <- date_comment1[grep("vor 2 Tagen", date_comment1)]
  #      d_sub2 <- date_comment1[-grep("vor 2 Tagen", date_comment1)]
  #      d_sub1 <- r
  #      d_sub2 <- dmy(d_sub2)
  #      date_comment <- c(d_sub1, d_sub2)
  #    }
  #    if(grep("vor 1 Tag", date_comment1)){
  #      r <- today() - 1
  #      d_sub1 <- date_comment1[grep("vor 1 Tag", date_comment1)]
  #      d_sub2 <- date_comment1[-grep("vor 1 Tag", date_comment1)]
  #      d_sub1 <- r
  #      d_sub2 <- dmy(d_sub2)
  #      date_comment <- c(d_sub1, d_sub2)
  #    }
  #  }
  #  if(length(grep("Minute", date_comment1)) != 0){
  #      r <- today()
  #      d_sub1 <- date_comment1[grep("Minute", date_comment1)]
  #      d_sub2 <- date_comment1[-grep("Minute", date_comment1)]
  #      d_sub1 <- r
  #      d_sub2 <- dmy(d_sub2)
  #      date_comment <- c(d_sub1, d_sub2)
  #    
  #  }
  #  if(length(grep("Minute", date_comment1)) == 0 & length(grep("Tag", date_comment1)) == 0){
  #    date_comment <- dmy(date_comment1)
  #    }
  
  ####  Strip link of comment ####
  link_comment_xpath <- "h3 .user-comment__link"
  link_comment <- html_document %>%
    html_nodes(css = link_comment_xpath) %>%
    html_attr('href') 
  
  
  #### Strip comment text ####
  comment_xpath <- ".user-comment__text"
  comment <- html_document %>%
    html_nodes(css = comment_xpath) %>%
    html_text(trim = T) %>% 
    as.character(.)
  
  url1  <- rep(url, length(title_comment))
  n_comments <- rep(n_comments, length(title_comment)) 
  user_name <- rep(user_name, length(title_comment)) 
  
  
  article <- data.frame(
    url = url1,
    user_name = user_name,
    n_comments = n_comments,
    title_comment = title_comment,
    date_comment = date_comment,
    link_comment = link_comment,
    comment = comment,
    fail = 0
  )
  
  return(article)
  
}

links <- get_page_urls(url)
articles <- data.frame()
for (i in 1:length(links[1:20])) {
  cat("Downloading", i, "of", length(links), "URL:", links[i], "\n")
  Sys.sleep(5)
  if((i %% 10) == 0){
    message("taking a break")
    Sys.sleep(10)
  }
  article <- scrape_zo_article(links[i])
  
  articles <- rbind(articles, article) 
}
