##### Header #####
## Author: Robert McGuinn
## Start date: 20220718
## purpose: generate a submission template from the data dictionary

##### packages #####
library(tidyverse)
library(googledrive)
library(googlesheets4)

##### authorizations #####
## Set authentication token to be stored in a folder called \.secrets``
options(gargle_oauth_cache = ".secrets")

## Authenticate manually
gs4_auth()

## If successful, the previous step stores a token file.
## Check that a file has been created with:
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")


##### load NDB #####
## the data must be in the folder below
## get the latest from google drive
setwd("C:/rworking/deepseatools/indata")
indata <- read.csv("DSCRTP_NatDB_20221213-0.csv", header = TRUE)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)
#
# ## cleanup
# # rm(indata)

##### check: browse to schema (manual: must put in correct ID) #####
## Google Drive location: https://drive.google.com/drive/folders/0B8lqJ4X0l6pTajJsa2t6bWRNRWc?resourcekey=0-QVD6rdiSnKeGQ4r7MSvwjQ&usp=drive_link
## see 'current' folder and 'archive' folder within that for earlier versions.
# gs4_browse('1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw')

##### load schema from Google Drive #####
s <- read_sheet('1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw')

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

##### create observations tab for submissions template #####
## pick out variables needed for the observations tab
x <- s %>%
  dplyr::select(PointHist,
                PointNew,
                PointProgram,
                TransHist,
                TransNew,
                TransProgram,
                TrawlHist,
                TrawlNew,
                TrawlProgram,
                FieldOrder,
                FieldDescription,
                ValidValues,
                FieldName
  )

y <- x %>% filter(PointHist == "R" |
                    PointNew == "R" |
                    PointProgram == "R" |
                    TransHist == "R" |
                    TransNew == "R" |
                    TransProgram== "R" |
                    TrawlHist== "R" |
                    TrawlNew == "R" |
                    TrawlProgram == "R" |
                    PointHist == "D" |
                    PointNew == "D" |
                    PointProgram == "D" |
                    TransHist == "D" |
                    TransNew == "D" |
                    TransProgram== "D" |
                    TrawlHist== "D" |
                    TrawlNew == "D" |
                    TrawlProgram == "D"
)

##### transpose the result and view it #####
z <- y %>% t() %>% View()

##### write observations tab of submission template #####
setwd("C:/rworking/deepseatools/indata")
write.csv(z, "20230405-0_dscrtp_submission_template_observation_table.csv")

##### create metadata tab for submissions template #####
## pick out variables needed for the metadata tab
x <- s %>%
  dplyr::select(FieldName,
                FieldDescription,
                ValidValues
                )

## make list of FieldNames for metadata tab
fieldlist <- c(
  "DataProvider",
  "DataContact",
  "Citation",
  "Repository",
  "Modified",
  "Reporter",
  "ReporterComments",
  "SurveyID",
  "Vessel",
  "VehicleName",
  "PI",
  "PIAffiliation",
  "SamplingEquipment",
  "DepthMethod",
  "NavType",
  "LocationAccuracy",
  "Purpose",
  "SurveyComments",
  "RecordType",
  "IdentifiedBy",
  "IdentificationQualifier",
  "IdentificationDate",
  "IdentificationComments")

## filter to the fieldlist above
y <- x %>% filter(FieldName %in% fieldlist)
## View(y)

## add space for data provider entry
y$Data_Provider_Add_Entries_Below <- ""
## View(y)

## fix the order the
y2 <- left_join(data.frame(FieldName = fieldlist),
                       y,
                       by = "FieldName")
## View(y2)

##### write metadata tab of submission template #####
setwd("C:/rworking/deepseatools/indata")
write.csv(y2, "20230405-0_dscrtp_submission_template_metadata_table.csv")




