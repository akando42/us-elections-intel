## Import Important Library
library(httr)
library(plumber)
library(rvest)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyverse)

validation_key <- "pubkey-511bebebd999a57e2bcc4d85f86f0233"
username <- "troy@topflightapps.com"

base_url <- "https://api.mailgun.net/v3/"
api_key <- "key-f8c4ea5af0feb19eb1dabd9733976b79"
domain <- "influencerwiz.com"

# Checking Authentication
mailgun_authenticate <- function(domain, api_key){
  authentication_url <- paste0(base_url,"domains/",domain,"/credentials")
  r <- GET(authentication_url, authenticate("api",api_key))
  response <- content(r, "parsed")
  return(response$items[[1]])
}

# Send Email
mailgun_send <- function(from, to, subject, html_body){
  sending_url  <- paste0("https://api.mailgun.net/v3/",domain,"/messages")
  r <- POST(
    sending_url, 
    authenticate("api", api_key), 
    from="postmaster@infinitylab.world",
    body = list(
      from=from,
      to=to, 
      subject=subject,
      html=html_body
    ),
    encode = "form", 
    form_string = list(
      html='<html>HTML version of the body</html>' 
    )
  )
  message(r)
  response <- content(r, "parsed")
  return(response$items[[1]])
}
# Sample
# mailgun_send("postmaster@influencerwiz.com", "troy@quarry.team","HTML template", email_html)

# Track Email Sent, Open for ALl Receipients
mailgun_broad_track <- function(domain, begin){
  tracking_url <- paste0("https://api.mailgun.net/v3/",domain, "/events")
  r <- GET(
    tracking_url,
    authenticate("api",api_key),
    query = list(
      begin=begin,
      ascending='yes',
      limit=300,
      pretty='yes'
    )
  )
  response <- content(r, "parsed")
  message(response)
  logs <- response$items
  Recipient <- character()
  Timestamp <- numeric()
  Status <- character()
  email_number <- length(logs)
  for(i in 1:length(logs)){
    Recipient[i]<- logs[[i]]$recipient
    Timestamp[i] <- logs[[i]]$timestamp
    Status[i] <- logs[[i]]$event
  }
  tracking_data <- data.frame(Recipient, Timestamp, Status, stringsAsFactors = FALSE)
  tracking_data <- mutate(tracking_data, Datetime = as_datetime(Timestamp))
  tracking_data$Hours <- format( tracking_data$Datetime, format="%H")
  tracking_data$Date <- format(tracking_data$Datetime, format="%d")
  return(tracking_data)
}

# Track Emal Sent, Delivered, Open and Click by Recipients
mailgun_targeted_track <- function(domain, recipient, begin){
  tracking_url <- paste0("https://api.mailgun.net/v3/",domain, "/events")
  r <- GET(
    tracking_url,
    authenticate("api",api_key),
    query = list(
      begin=begin,
      ascending='yes',
      limit=300,
      pretty='yes',
      recipient=recipient
    )
  )
  response <- content(r, "parsed")
  message(response)
  logs <- response$items
  Recipient <- character()
  Timestamp <- numeric()
  Status <- character()
  email_number <- length(logs)
  for(i in 1:length(logs)){
    Recipient[i]<- logs[[i]]$recipient
    Timestamp[i] <- logs[[i]]$timestamp
    Status[i] <- logs[[i]]$event
  }
  tracking_data <- data.frame(Recipient, Timestamp, Status, stringsAsFactors = FALSE)
  tracking_data <- mutate(tracking_data, Datetime = as_datetime(Timestamp))
  tracking_data$Hours <- format( tracking_data$Datetime, format="%H")
  tracking_data$Date <- format(tracking_data$Datetime, format="%d")
  return(tracking_data)
}
# Sample
# mailgun_targeted_track("influencerwiz.com", "troy@quarry.team", "Fri, 01 April 2019 17:00:00 -0000")

# Draw chart from stats collected
mailgun_hourly_chart <- function(data){
  start_hour <- min(data$Hours)
  end_hour <- max(data$Hours)
  email_chart_data <- data.frame(hour=numeric(), received=numeric(), opened=numeric())
  for (i in start_hour:end_hour){
    print(class(i))
    email <- filter(data, data$Hours == i)
    tryCatch(received <- email %>% filter(email$Status =="accepted") %>% nrow(),error=function(){return(received <- 0)})
    tryCatch(opened <- email %>% filter(email$Status == "opened") %>% nrow(), error=function(){return(opened <- 0)})
    email_chart_data <- add_row(email_chart_data, hour=i, received=received, opened=opened)
  }
  hc <- highchart() %>% 
    hc_xAxis(categories = email_chart_data$hour) %>% 
    hc_add_series(name = "Opened", data = email_chart_data$opened) %>%
    hc_add_series(name = "Received", data = email_chart_data$received) %>% 
    hc_add_theme(hc_theme_chalk()) %>%
    hc_title(text="Yasware Email Stats")
  return(hc)
}

mailgun_daily_chart <- function(data){
  start_hour <- min(data$Date)
  end_hour <- max(data$Date)
  email_chart_data <- data.frame(hour=numeric(), received=numeric(), opened=numeric())
  for (i in start_hour:end_hour){
    print(class(i))
    email <- filter(data, data$Date == i)
    tryCatch(received <- email %>% filter(email$Status =="accepted") %>% nrow(),error=function(){return(received <- 0)})
    tryCatch(opened <- email %>% filter(email$Status == "opened") %>% nrow(), error=function(){return(opened <- 0)})
    email_chart_data <- add_row(email_chart_data, hour=i, received=received, opened=opened)
  }
  hc <- highchart() %>% 
    hc_xAxis(categories = email_chart_data$Date) %>% 
    hc_add_series(name = "Opened", data = email_chart_data$opened) %>%
    hc_add_series(name = "Received", data = email_chart_data$received) %>% 
    hc_add_theme(hc_theme_chalk()) %>%
    hc_title(text="Yasware Email Stats")
  return(hc)
}

# r <- plumb("Yasware.R")
# r$run(port=6969)
# authentication link
# https://api.mailgun.net/v3/domains/infinitylab.world/credentials
