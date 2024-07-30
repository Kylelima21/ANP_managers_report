## Full dataset creation for back referencing records of the watchlist species


#------------------------------------------------#
####        Packages/Custom Functions         ####
#------------------------------------------------#


source("database_creation/database_functions.R")




#------------------------------------------------#
####            Read in the Data              ####
#------------------------------------------------#

### Read in iNaturalist dataset downloaded on 20240715
## These data are already filter to Acadia, but we need to fix column headers
inat <- read.csv("database_creation/full_inat_obs_20240715.csv") %>% 
  rename_with(., ~gsub("_", ".", .)) 


### Read in eBird dataset downloaded on 20240715
## Clean and filter to Acadia
ebd1 <- tibble(read.delim("database_creation/ebd_US-ME_relMay-2024.txt", header = T, quote = "")) %>% 
  rename_with(., tolower)

ebd <- ebd1 %>% 
  select(-x) %>% 
  filter_nps(., "Acadia National Park", "latitude", "longitude")




#------------------------------------------------#
####        Filter and Compile Data           ####
#------------------------------------------------#

## Run the three function and combine rows for iNaturalist
inat_full <- bind_rows(inat %>% watchlist_inv(), inat %>% watchlist_te(),
                       inat %>% watchlist_rn())


## Run the three function and combine rows for eBird
ebd_full <- bind_rows(ebd %>% watchlist_inv(), ebd %>% watchlist_te(),
                       ebd %>% watchlist_rn())




#------------------------------------------------#
####               Export Data                ####
#------------------------------------------------#

## Write out the datasets
write.csv(inat_full, "email_alerts/www/datasets/inat_fulldata.csv", row.names = F)
write.csv(ebd_full, "email_alerts/www/datasets/ebd_fulldata.csv", row.names = F)






