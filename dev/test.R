# HEADER #######################################################################

## Project:
## Script:
## Purpose:
## Author: Brandon Rose, MD, MPH
## Email: thecodingdocs@gmail.com
## Notes:

# CLEAR ########################################################################

rm(list=ls(all.names = T))
cat("\014")
if(!is.null(dev.list())) dev.off()
graphics.off()

# FUNCTIONS ####################################################################
remotes::install_github("brandonerose/rosycontacts")
rstudioapi::restartSession()
library("rosycontacts")
library("tidyverse")
source("/Users/brandonrose/R/brose/drive_location.R")

# IMPORT #######################################################################


# EXPLORE ######################################################################


contacts_wide %>% rio::export(file.path(googledrive(),"all_contacts.csv"))
df5<-file.path(googledrive(F),"all_contacts.xlsx") %>% rio::import() %>% dplyr::mutate_all(~ ifelse(is.na(.), "", .))

df5$N <- paste0(df5$LASTNAME,";",df5$FIRSTNAME,";",df5$ADDITIONALNAME,";",df5$PREFIX,";",df5$SUFFIX)





df5$TEL_1 <- grab_after_symbol(df5$TEL_1,":")
df5$TEL_2 <- grab_after_symbol(df5$TEL_1,":")
df5$TEL_3 <- grab_after_symbol(df5$TEL_1,":")





df5<-df5
# TIDY #########################################################################


# TRANSFORM ####################################################################


# COMMUNICATE ##################################################################


# SCRAP ########################################################################
