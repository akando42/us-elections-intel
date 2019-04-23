#* Authenticate into Mailgun
#* @param domain Domain to send Email from
#* @get /mailgun_auth
function(domain){
  mailgun_authenticate(domain)
}
