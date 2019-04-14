## Getting Social Media Content from TX25
library(twitteR)

twitter_api_key <- "KB6l8VvI55feXS2OMuhBFHzQG"
twitter_api_secret <- "VNV26E12wzIT18mOZkWpIc8HkzMFE719mrCn4fnLoKRkK1o2d0"
twitter_access_token <- "432138255-Fq1KA00FKusSmuXNCY2n3jnirvuU3DjhMXHlminw"
twitter_access_secret <- "iYWbMOyLF6hLZ73inEoycud37eo7Ph6dKIl0kz2lHBk9b"

## Getting Data from Twitter
result <- setup_twitter_oauth(twitter_api_key, twitter_api_secret)


library(tidyverse) ## Data Wraggling
library(rvest)     ## Passing HTML/XML content
library(stringr)   ## String Manipulation
library(rebus)     ## Regular Expression
library(lubridate) ## Time Format manipulation
library(dplyr)     ## Data Manipulation


## Agency Lead Generation from Clutch
clutch_services <- replicate(356, "https://clutch.co/it-services?page=")
pages <- c(0:355)

targets <- data.frame("link"=clutch_services, "index"=c(0:355))
targets <- mutate(targets, url = paste0(link, index))

page_html <- read_html(targets$url[1])
xpath_listing <- '//*[@id="directory"]/div/div[2]/div/div[1]/div'
xpath_profile <- '//*[@id="directory"]/div/div[2]/div/div[1]/div/ul/li[1]/div/div[2]/ul/li[2]/a'


it_agencies <- data.frame(name=character(), url=character(), website=character(), email=character())

get_name <- function(html,index){
  print("Geting Name")
  name_xpath <- paste0('//*[@id="directory"]/div/div[2]/div/div[1]/div/ul/li[',index,']/div/div[1]/div[1]/div/h3/span/a')
  selected_node <- html_node(html, xpath=name_xpath)
  name <- html_text(selected_node, trim=FALSE)
  print(name)
  return(name)
}

profile_link <- function(html, index){
  print("Getting Link")
  print(index)
  link_xpath <-  paste0('//*[@id="directory"]/div/div[2]/div/div[1]/div/ul/li[',index,']/div/div[2]/ul/li[2]/a')
  selected_node <- html_node(html, xpath=link_xpath)
  link <- html_attr(selected_node, "href")
  link <- paste0('https://clutch.co', link)
  print(link)
  return(link)
}

get_website <- function(html, index){
  print("GETTING WEBSITE")
  link_xpath <-  paste0('//*[@id="directory"]/div/div[2]/div/div[1]/div/ul/li[',index,']/div/div[2]/ul/li[1]/a')
  selected_node <- html_node(html, xpath=link_xpath)
  website <- html_attr(selected_node, "href")
  return(website)
}

website_contact_email <- function(website){
  print("GETTING EMAIL")
  url <- as.character(website)
  print(url)
  session <- html_session(url)
  #html <- read_html(website)
  selected_node <- html_nodes(session,css='a[href^="mailto"]')
  email <- html_attr(selected_node, "href")
  email <- substr(email, 8, nchar(email))
  return(email)
}

clutch_contact_email <- function(url){
  html <- read_html(url)
  email_xpath <- '//*[@id="block-system-main"]/div/div[1]/ul/li[2]/div[2]/div[2]/div[1]/div/div'
  selected_node <- html_nodes(html, xpath=email_xpath)
  text <- html_text(selected_node)
  print(text)
  semicolon <- regexpr(";", text)[1]
  semicolons <- gregexpr(";", text)
  positions <- semicolons[[1]]
  first <- positions[1]
  last <- positions[length(positions)]
  email <- substr(text, 18, semicolon-2)
  mailto <- regexpr("mailto:", text)
  elements <- strsplit(email, "#")
  swap <- substr(text, mailto+8, last)
  print(swap)
  coordinates <- gregexpr("]", swap)
  coordinates <- coordinates[[1]]
  print("MY COORDINATES")
  print(coordinates) 
  print("MY ELEMENT")
  print(elements)
  emails <- c()
  for(coordinate in coordinates){
    print(coordinate)
    secret <- substring(swap, coordinate-1, coordinate-1)
    secret <- as.numeric(secret) + 1
    emails <- c(emails, elements[[1]][as.numeric(secret)])
  }
  precious_email <- paste(emails, collapse="")
  return(precious_email)
}

extract_info <- function(link, index){
  html <- read_html(link)
  name <- get_name(html, index)
  profile <- profile_link(html,index)
  website <- get_website(html,index)
  print(name)
  print(profile)
  print(website)
  return(data.frame(name=name, url=profile, website=website))
}

new_batch <- function(index){
  batch <- data.frame(name=character(), url=character(), website=character())
  for(link in targets$url){
    info <- extract_info(link, index)
    batch <- rbind(batch, info)
  }
  return(batch)
}

## Getting from Clutch
acquire <- function(index){
  batch <- new_batch(index)
  for (i in c(1:356)){
    print(i)
    url <- as.character(batch$url[i])
    try(
      batch$clutch_email[i] <- clutch_contact_email(url)
    )
  }
  name <- paste0(index, '_batch.csv')
  write.csv(batch, name)
}


## Getting from Website
for (i in c(1:356)){
  print(i)
  website <- it_agencies$website[i]
  try(
    it_agencies$email[i] <- contact_email(website)
  )
}