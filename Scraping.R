## Getting Social Media Content from TX25
library(twitteR)

twitter_api_key <- "KB6l8VvI55feXS2OMuhBFHzQG"
twitter_api_secret <- "VNV26E12wzIT18mOZkWpIc8HkzMFE719mrCn4fnLoKRkK1o2d0"
twitter_access_token <- "432138255-Fq1KA00FKusSmuXNCY2n3jnirvuU3DjhMXHlminw"
twitter_access_secret <- "iYWbMOyLF6hLZ73inEoycud37eo7Ph6dKIl0kz2lHBk9b"


library(tidyverse) ## Data Wraggling
library(rvest)     ## Passing HTML/XML content
library(stringr)   ## String Manipulation
library(rebus)     ## Regular Expression
library(lubridate) ## Time Format manipulation
library(dplyr)     ## Data Manipulation

## Getting Data from Twitter
result <- setup_twitter_oauth(twitter_api_key, twitter_api_secret)


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

contact_email <- function(website){
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

extract_info <- function(link){
  html <- read_html(link)
  name <- get_name(html, 1)
  profile <- profile_link(html,1)
  website <- get_website(html,1)
  # tryCatch({
  #   message("trying to get email")
  #   email <- contact_email(website)
  #   return(email)
  # }, error=function(cond){
  #   message(cond)
  #   email <- "no@email.com"
  #   return(email)
  # })
  print(name)
  print(profile)
  print(website)
  #print(email)
  return(data.frame(name=name, url=profile, website=website))
}

for(link in targets$url){
  print(link)
  info <- extract_info(link)
  it_agencies <- rbind(it_agencies, info)
}

for (i in c(1:359)){
  print(i)
  website <- it_agencies$website[i]
  try(
    it_agencies$email[i] <- contact_email(website)
  )
}






## Web Scraping from Trust Pilot