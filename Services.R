# plumber.R
source("Services/Yasware.R")
source("Services/Agencies.R")

#* Plot a highcharter
#* @param campaign_domain Domain of the Campaign
#* @param begin Beginning Date of Tracking Dataset
#* @get /mailgun_chart
function(campaign_domain, begin){
    data <- mailgun_broad_track(campaign_domain, begin)
    mailgun_daily_chart(data)
}

#* Return the sum of two numbers
#* @param from Sender email address
#* @param to Receiver email address
#* @param subject Subject of the email
#* @param html_body Html email template
#* @post /mailgun_send
function(from, to, subject, html_body){
    mailgun_send(from, to, subject, html_body)
}