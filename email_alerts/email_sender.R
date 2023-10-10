## Script to produce and send the email alerts to those signed up
## Schoodic Institute at Acadia National Park


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
library(blastula)



#------------------------------------------------#
####         Create and send email            ####
#------------------------------------------------#

## Can run to see example of what the email will look like
# my_email_object <- render_email('outputs/emailtest.Rmd')
# print(my_email_object)


## Create the credentials key needed to send from my email
# create_smtp_creds_key(
#   id = "kmail",
#   user = "klima@schoodicinstitute.org",
#   provider = "gmail",
#   #overwrite = TRUE
# )

# create_smtp_creds_file(
#   file = "kmail",
#   user = "klima@schoodicinstitute.org",
#   provider = "gmail"
# )


## Produce and send the email
smtp_send(render_email('email_alerts/email_material.Rmd'),
          from = "klima@schoodicinstitute.org",
          to = c("klima@schoodicinstitute.org",),
                 # "nfisichelli@schoodicinstitute.org", 
                 # "Jesse_Wheeler@nps.gov", "hwebber@schoodicinstitute.org",
                 # "sbenz@schoodicinstitute.org", "cnadeau@schoodicinstitute.org", 
                 # "hmittelstaedt@schoodicinstitute.org", "cschmitt@schoodicinstitute.org",
                 # "sobrien@schoodicinstitute.org"),
          subject = "Acadia National Park Citizen Science Report",
          credentials = creds_file("email_alerts/kmail")
)




