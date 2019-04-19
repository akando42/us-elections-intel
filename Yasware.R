## Import Important Library
library(httr)
library(plumber)
library(rvest)
library(dplyr)
library(lubridate)

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

# Track Emal Sent, Delivered, Open and Click
mailgun_track <- function(domain, recipient, begin){
  tracking_url <- paste0("https://api.mailgun.net/v3/",domain, "/events")
  r <- GET(
    tracking_url,
    authenticate("api",api_key),
    query = list(
      begin=begin,
      ascending='yes',
      limit=100,
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
  tracking_data <- data.frame(Recipient, Time, Status, stringsAsFactors = FALSE)
  tracking_data <- mutate(tracking_data, Datetime = as_datetime(Time))
  return(tracking_data)
}
# Sample
# mailgun_track("influencerwiz.com", "troy@quarry.team", "Fri, 18 April 2019 17:00:00 -0000")








# r <- plumb("Yasware.R")
# r$run(port=6969)

# authentication link
# 
# https://api.mailgun.net/v3/domains/infinitylab.world/credentials
