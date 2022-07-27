##### Header #####
## Author: Robert McGuinn
## Start date: 20220718
## Submission Template


##### packages #####
library(bookdown)
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)
library(googlesheets4)

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### load NDB #####
## the data must be in the folder below
## get the latest from google drive
# setwd("C:/rworking/deepseatools/indata")
# indata <- read.csv("DSCRTP_NatDB_20220426-0.csv", header = TRUE)
# filt <- indata %>%
#   filter(Flag == "0", is.na(Phylum) == F)

## cleanup
# rm(indata)

##### load schema #####
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
# gs4_browse('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

##### checking #####
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(FieldDescription)
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(ValidValues)
# s$DataSubmissionTemplate
# names(s)
# names(x)
# class(s$FieldName)
# class(x$VariableName)
# setdiff(x$FieldName, s$FieldName)
#
# x <- s %>%
#   mutate(FieldName = str_replace(FieldName, "latitude", "Latitude")) %>%
#   mutate(FieldName = str_replace(FieldName, "longitude", "Longitude"))

##### create custom submission template #####
## pick out variables needed in the template
x <- s %>% dplyr::select( FieldOrder,
                          FieldName,
                          FieldDescription,
                          ValidValues,
                          DSCRTPFieldClass,
                          DSCRTPCategory,
                          DSCRTPGroup,
                          # PointHist,
                          # PointNew
                          PointProgram
                          # TransHist,
                          # TransNew,
                          # TransProgram,
                          # TrawlHist,
                          # TrawlNew,
                          # TrawlProgram
)

y <- x %>% filter(PointProgram == "R" |
                    PointProgram== "D")

z <- y %>% t()

View(z)

##### write submission template #####
# setwd("C:/rworking/deepseatools/indata")
# write_csv(x, "20220718-0_dscrtp_submission_template.csv")




