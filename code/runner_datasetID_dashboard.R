##### Header #####
# Author: Robert P. McGuinn
# Date started: 2018-08-14
# Purpose: Execute RMarkdown documents on dashboards for each DatasetID.
#   4 groups: Cruise, Literature, Program, Repository, and then Repository-MBARI
# (because it is so huge it can't have an interactive map)

##### install packages #####
library(sf)
library(sp)
library(openxlsx)
library(tidyverse)
library(RColorBrewer)
library(googledrive)
library(rmarkdown)
library(knitr)
library(maps)
library(prettydoc)
library(httr)
library(jsonlite)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)

##### authorizations #####
# Set authentication token to be stored in a folder called \.secrets``
options(gargle_oauth_cache = ".secrets")

# Authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:

list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### set an option #####
options(lifecycle_disable_warnings = TRUE)

##### ***OR*** read current database from disk #####
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20230828-0.csv"
indata <- read.csv(filename,
                   encoding = "UTF-8",
                   header = TRUE,
                   stringsAsFactors = FALSE)
filt <- indata %>%
  filter(Flag == 0)

rm(indata)
rm(filename)


##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))

##### ***OPTIONAL download Google Sheet version of schema for use in R documents #####
# Register and download Google Sheet using googlesheets4::read_sheet
s <- read_sheet('1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw')

##### checking #####
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(FieldDescription)
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(ValidValues)

##### check #####
## occurrenceID==NOAA_DSCRTP:1178027,
## that had an NA value for "basisOfRecord".
## The rest of the dataset has a value of "PreservedSpecimen".
# filt %>% filter(CatalogNumber == 1178027) %>%
#   group_by(DatasetID, PI, Repository) %>%
#   summarize(n=n()) %>% View()

  ## Not valid eventDates for:
  ## occurrenceID==NOAA_DSCRTP:1178069 ; eventDate == 3.1973
  ## occurrenceID==NOAA_DSCRTP:1178068 ; eventDate == 3.1973
  ## occurrenceID==NOAA_DSCRTP:1178065 ; eventDate == 2010-0-27

# filt %>% filter(CatalogNumber == 1178065) %>%
#   group_by(DatasetID,
#            SampleID,
#            Repository,
#            ObservationDate,
#            ScientificName) %>%
#   summarize(n=n()) %>% View()

##### define database version (this variable called in RMD files) #####
version <- unique(filt$DatabaseVersion)
version <- as.character(version)

##### bring in datasetID key from local path #####
key <- read.xlsx("C:/rworking/deepseatools/indata/20240115-0_DatasetID_Key_DSCRTP.xlsx")

##### checking #####
setdiff(filt$DatasetID, key$DatasetID)
setdiff(key$DatasetID, filt$DatasetID)
#
filt_old %>% filter(grepl('Carreiro', DatasetID)) %>%
  pull(DatasetID) %>% unique() %>%
  View

key %>% filter(grepl('Carreiro', DatasetID)) %>%
  pull(DatasetID) %>% unique() %>%
  View

x <- setdiff(filt$DatasetID, key$DatasetID)
x <- filt %>% filter(DatasetID %in% x) %>%
  group_by(ObservationDate, DatasetID, RecordType, SamplingEquipment, VehicleName, DataProvider, Repository, Locality, SurveyComments, Citation, Vessel, WebSite) %>%
  summarize(n=n())
View(x)


x <- setdiff(filt_old$CatalogNumber, filt$CatalogNumber)
x <- filt_old %>% filter(CatalogNumber %in% x) %>%
  group_by(DatasetID) %>%
  summarize(n=n())
View(x)

filt_old %>% filter(grepl('Carreiro', DatasetID)) %>%
  group_by(DatasetID) %>% summarize(n=n()) %>%
  View

#
key %>% filter(grepl("EX-19-05", DatasetID)) %>% pull(DatasetID) %>% table()
#
# key %>%
#   filter(DatasetID == "NOAA_PC-11-05-L1") %>%
#   pull(abstract)
#
# x <- "NOAA_SH-22-09"
# x <- filt %>% filter(grepl(x, DatasetID)) %>%
#   group_by(Citation, CitationMaker, WebSite, Vessel, SurveyID, ObservationYear, ObservationDate, DatasetID) %>%
#   summarize(n=n())
# View(x)


# x <- setdiff(filt$DatasetID, key$DatasetID)
# x <- filt %>% filter(DatasetID %in% x) %>%
#   group_by(DatasetID) %>%
#   summarize(n=n())
# View(x)
#
# x <- "NOAA_CINMS_SW-19-06"
# x <- filt %>% filter(DatasetID == x) %>% pull(Locality) %>% unique()
# View(x)
#

#
# id <- "BOEM_Mid-Atlantic_Canyons"
# x <- paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/", id, ".html", sep = '')
# browseURL(x)
#


# View(x)
#
#
# rm(x)
#
# setdiff(indata$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, indata$DatasetID)

##### MANUAL STEP (in Google Drive) : go update the DatasetID key with the new values #####
# setdiff(filt$DatasetID, key$DatasetID)

##### checking #####
x <- filt %>%
  filter(DatasetID == "NOAA_RL-19-05") %>%
  group_by(Vessel, VehicleName, WebSite, SurveyID, ObservationDate, ObservationYear) %>%
  summarize(n=n())

x

x <- filt %>%
  filter(DatasetID == "NOAA_PC-16-05") %>% pull(Citation) %>% unique()

x

##### write the citation information #####

filt$CitationMaker <- paste(filt$DataProvider,'. ',
                            filt$ObservationYear,
                            ' to ',
                            filt$ObservationYear,
                            '. ',
                            'Coral or sponge occurrence observations submitted to the NOAA National Database for Deep Sea Corals (www.deepseacoraldata.noaa.gov)', '. ',
                            'DSCRTP Dataset ID: ', filt$DatasetID, '. ',
                            'DSCRTP Accession ID: ',filt$AccessionID, '. ',
                            'Record type: ', filt$RecordType, '. ',
                            'Vessel(s): ', filt$Vessel,'. ',
                            'Sampling vehicle: ', filt$VehicleName,'. ',
                            'Survey ID: ', filt$SurveyID,'. ',
                            'Principle investigator: ', filt$PI,'. ',
                            'Data contact: ', filt$DataContact,'. ',
                            'Reporter: ', filt$Reporter,'. ',
                            'Repository: ', filt$Repository,'. ',
                            'Web site [last accessed on YYYY-MM-DD]: ', filt$WebSite,'.',
                            sep = '')

##### checking: looking at the Citation #####

# 1] "NOAA_NEFSC_HB-15-04"
# [2] "NOAA_AFSC_Tiglax_D2017"
# [3] "NOAA_AFSC_Tiglax_SSL_2016"
# [4] "NOAA_SH-18-12_ROV-Transect"
# [5] "SOI_FK171005"
# [6] "ZSM"
# [7] "Choy et al, 2020"
# [8] "Lehnert & Stone 2016"
# [9] "RBCM"
# [10] "Watling 2015"
# [11] "Shen2019"
# [12] "TMAG"

x <- "De_Clippele_etal_2019"

y <- filt %>% filter(DatasetID == x) %>%
  group_by(VernacularNameCategory,
           gisMEOW,
           RecordType,
           Phylum,
           Vessel,
           VehicleName,
           SurveyID,
           ObservationYear,
           ObservationDate,
           Locality,
           DataProvider,
           Repository,
           Citation,
           IdentifiedBy, PI) %>%
  summarize(n=n()) %>%  View()

filt %>% filter(DatasetID == x) %>%
  pull(CitationMaker) %>%
  unique()

##### ***OR*** bring in new MANUALLY UPDATED datasetID key from local path #####
key <- read.xlsx("C:/rworking/deepseatools/indata/20230918-1_DatasetID_Key_DSCRTP.xlsx")

##### ***OR*** bring in new MANUALLY UPDATED datasetID key from Google Drive #####
## create a list of files (or single file) that meets title query (Manual change)

x <- drive_find(q = "name contains '20240115-0_DatasetID_Key_DSCRTP'") #

## browse to it
x %>% drive_browse()

# getting the id as a character string
y <- x$id

# this downloads the file to the specified path (manual change required)
dl <- drive_download(as_id(y),
                     path = "C:/rworking/deepseatools/indata/20240115-0_DatasetID_Key_DSCRTP.xlsx",
                     overwrite = TRUE)

## read the file into R as a data frame
key <- read.xlsx(dl$local_path)

##### ***OPTIONAL*** updating DatasetID key with new 'n' #####
## build a frequency table by DatasetID from new file
x <- filt %>% group_by(DatasetID) %>% summarize(n=n())

# strip 'n' from existing key
names(key)
y <- key[,1:8]

x$DatasetID <- as.character(x$DatasetID)
y$DatasetID <- as.character(y$DatasetID)

## check
names(y)
View(y)


## merge new numbers to create old key + new counts
z <- left_join(y,x)

##### check #####
View(z)

##### write out result (manual change to file name) #####
write.xlsx(z, "C:/rworking/deepseatools/indata/20240115-0_DatasetID_Key_DSCRTP.xlsx",
           overwrite = TRUE)

##### manual upload new key to Google Drive (point and click stuff) #####
##### ***OR*** full run set 'filt' to d #####
rm(filt_old)
d <- filt

##### ***OPTIONAL*** subsetting of indata to d (optional step for testing purposes) #####
# d <- d %>%
#   filter(
#     DatasetID == 'NIWA'
#   )

##### cleanup #####
rm(indata)
rm(filt_old)
rm(indata_old)
rm(x)
rm(z)
rm(y)

##### checking #####
# table(unique(factor(d$DataProvider)))
# table(unique(factor(d$RecordType)))
# table(unique(factor(d$DataContact)))
# table(factor(d$DatasetID), useNA = 'always')
#
# yo <- key %>%
#   # filter(class == 'Cruise') %>%
#   group_by(class) %>%
#   summarise(DatasetID = paste(unique(DatasetID), collapse = " | "),
#             n=n())
# View(yo)
# key %>% filter(grepl("Tu", DatasetID)) %>% pull(title) %>% unique()
yo <- filt %>%
  filter(as.numeric(Longitude) > 0) %>%
  pull(DatasetID) %>%
  unique()

yo2 <- filt %>%
  filter(as.numeric(Longitude) < 0) %>%
  pull(DatasetID) %>%
  unique()

oneeighty <- intersect(yo, yo2)

##### checking #####
# x <- d %>%
#   arrange(ObservationYear) %>%
#   filter(DatasetID %in% setdiff(d$DatasetID, key$DatasetID)) %>%
#   group_by(DatasetID) %>%
#   summarize(
#       ObservationYear_list = toString(unique(ObservationYear)),
#       ObservationDate_list = toString(unique(ObservationDate)),
#       Locality_list = toString(unique(Locality)),
#       RecordType_list = toString(unique(RecordType)),
#       N_Records=n(),
#       # DatasetID_list = toString(unique(DatasetID)),
#       DataProvider_list = toString(unique(DataProvider)),
#       Repository_list = toString(unique(Repository)),
#
#       Vessel_list = toString(unique(Vessel)),
#       VehicleName_list = toString(unique(VehicleName)),
#       WebSite_list = toString(unique(WebSite)),
#       #ImageURL = toString(unique(ImageURL)),
#       PI_list = toString(unique(PI)),
#       PIAffiliation_list = toString(unique(PIAffiliation)),
#       Citation_list = toString(unique(Citation)),
#       DataContact_list = toString(unique(DataContact)),
#       Reporter_list = toString(unique(Reporter)),
#       SurveyID_list = toString(unique(SurveyID))
#     )
#
# View(x)

##### ***OPTIONAL*** Fixing DatasetID Problems (optional) #####
# d <- d %>% mutate(DatasetID =
#                     ifelse(DatasetID == "NOAA_SH-18-12_ROV-Transect",
#                            'NOAA_SH-18-12',
#                            as.character(DatasetID)))

##### dealing with special character issues #####
## Install and load the stringi package if not already installed

# library(stringi)
#
# d$DataContact <- stri_trans_general(d$DataContact, "latin-ascii")
# unique(d$DataContact)
#
# contains_non_utf8 <- function(text) {
#   # Use stringi::stri_enc_detect to detect the encoding
#   encoding_info <- stringi::stri_enc_detect(text)
#
#   # Check if the detected encoding is not UTF-8
#   if (any(encoding_info$encoding != "UTF-8")) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
# }
#
#
# contains_non_utf8(unique(d$ScientificName))
# contains_non_utf8(unique(d$DataContact))
#
#
# d %>% filter(Encoding(ScientificName) == "UTF-8") %>% pull(ScientificName)
# d %>% filter(Encoding(ScientificName) == "UTF-8") %>% pull(ScientificName)
# d %>% filter(Encoding(Family) == "UTF-8") %>% pull(ScientificName)
#
#
#
# d <- d %>% mutate(ScientificName =
#                     ifelse(ScientificName == "Muriceides k\xfckenthali",
#                            'Muriceides kuekenthali',
#                            as.character(ScientificName)))
# d <- d %>% mutate(DataProvider =
#                     ifelse(DataProvider == "Due\xf1as et al. (2014)",
#                            'New Zealand National Institute of Water and Atmospheric Research (NIWA)',
#                            as.character(DataProvider)))
# d <- d %>% mutate(DataContact =
#                     ifelse(DataContact == "Due\xf1as, Luisa (lduenasm@unal.edu.co",
#                   'Dueñas, Luisa | duenasm@unal.edu',
#                   as.character(DataContact)))
#
#
#
#
#
# x <- unique(d$DataContact)
#
#
#
# table(x$DataContact, useNA = 'always')
# x$DataContact <- iconv(x$DataContact,to = "latin9")
# table(x$DataContact, useNA = 'always')
# missing <- x %>% filter(is.na(DataContact)==T) %>% pull(CatalogNumber)
#
# filt %>% filter(CatalogNumber %in% missing) %>% pull(DataContact)
#
#
# x <- d %>% filter(is.na(DataContact) == F) %>%
#   group_by(DatasetID, DataContact) %>%
#   summarize(n=n())
# View(x)
#
# missing <- x$DatasetID
# x <-filt %>% filter(DatasetID %in% missing) %>% group_by(DatasetID, DataContact, Citation) %>%
#   summarize(n=n())
# View(x)
#
#
#
#
# clean_utf8 <- function(text) {
#   iconv(text, to = "UTF-8", sub = "byte")
# }
#
# # Apply the clean_utf8 function to your data
# d$DataContact <-
# d$ScientificName <- sapply(d$ScientificName, clean_utf8)
# d$Order <- sapply(d$Order, clean_utf8)
# d$Family <- sapply(d$Family, clean_utf8)


##### checking #####
# setdiff(d$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, d$DatasetID)
##### cleanup #####
rm(filt)
##### important!! merge database with key  #####
d <- merge(d, key, all.x = TRUE)

##### checking #####
# x <- d %>% filter(DatasetID == "Carranza_etal_2012")
# write.csv(test, "c:/rworking/deepseatools/indata/carranza.csv")
# test2 <- read_utf8("c:/rworking/deepseatools/indata/carranza.csv")

# d %>% filter(CatalogNumber == '618051') %>% pull(Citation)


# table(d$class, useNA = 'always')
#
# x <- d %>%
#   group_by(class) %>%
#   summarize(DatasetsbyClass = length(unique(DatasetID)),
#     n = n())
# View(x)

# setdiff(d$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, d$DatasetID)
# #
# x <- d #%>%
#   # filter(
#   #   is.na(class) == T,
#   #   #DataProvider == "Temple University",
#   #   #class == "Literature"
#   #   )
#
# table(factor(x$class), useNA = 'always')
#
# library(stringr)
# test <- stringr::str_conv(d, "UTF-8")

##### *** run the reports *** #####
##### assign which datasets cross the 180 line #####
yo <- filt %>%
  filter(as.numeric(Longitude) > 0) %>%
  pull(DatasetID) %>%
  unique()

yo2 <- filt %>%
  filter(as.numeric(Longitude) < 0) %>%
  pull(DatasetID) %>%
  unique()

one_eighty <- intersect(yo, yo2)
not_one_eighty <- setdiff(filt$DatasetID, one_eighty)

##### checking #####
length(one_eighty)+length(not_one_eighty)

##### _create the folders for each type of report #####
dir.create('C:/rworking/deepseatools/reports/datasetid/cruise')
dir.create('C:/rworking/deepseatools/reports/datasetid/literature')
dir.create('C:/rworking/deepseatools/reports/datasetid/program')
dir.create('C:/rworking/deepseatools/reports/datasetid/repository')

##### _cruise #####
#cruise subset
yo <- d %>%
  filter(
    class == 'Cruise',
    DatasetID %in% not_one_eighty
  )

# length(unique(yo$DatasetID)
# run RMD
library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_cruise_no_leaflet.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/cruise')
}

yo <- d %>%
  filter(
    class == 'Cruise',
    DatasetID %in% one_eighty
  )

# length(unique(yo$DatasetID)
# run RMD

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_cruise_no_leaflet_one_eighty.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/cruise')
}

##### _literature ######
yo <- d %>%
  filter(
    class == 'Literature',
    DatasetID %in% not_one_eighty
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_literature_no_leaflet.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/literature')
}

yo <- d %>%
  filter(
    class == 'Literature',
    DatasetID %in% one_eighty
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_literature_no_leaflet_one_eighty.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/literature')
}

##### _program #####
yo <- d %>%
  filter(
    class == 'Program',
    DatasetID %in% not_one_eighty
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_program_no_leaflet.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/program')
}

yo <- d %>%
  filter(
    class == 'Program',
    DatasetID %in% one_eighty
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_program_no_leaflet_one_eighty.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/program')
}

##### _repository #####
yo <- d %>%
  filter(
    class == 'Repository',
    DatasetID %in% not_one_eighty
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_repository_no_leaflet.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/repository')
}

yo <- d %>%
  filter(
    class == 'Repository',
    DatasetID %in% one_eighty
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_repository_no_leaflet_one_eighty.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/repository')
}

##### _repository:MBARI #####
yo <- d %>%
  filter(
    DatasetID == 'MBARI'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_repository_MBARI_no_leaflet.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/repository')
}

##### check #####
d %>% filter(class == 'Cruise') %>% pull(DatasetID) %>% unique() %>% length()
d %>% filter(class == 'Literature') %>% pull(DatasetID) %>% unique() %>% length()
d %>% filter(class == 'Program') %>% pull(DatasetID) %>% unique() %>% length()
d %>% filter(class == 'Repository') %>% pull(DatasetID) %>% unique() %>% length()

cruise <- list.files('C:/rworking/deepseatools/reports/datasetid/cruise/')
literature <- list.files('C:/rworking/deepseatools/reports/datasetid/literature/')
program <- list.files('C:/rworking/deepseatools/reports/datasetid/program/')
repository <- list.files('C:/rworking/deepseatools/reports/datasetid/repository/')

length(cruise)
length(literature)
length(program)
length(repository)

biglist <- c(cruise, literature, program, repository)
biglist <- gsub(pattern = "\\.html$", "", biglist)

setdiff(biglist, unique(filt$DatasetID))
setdiff(unique(filt$DatasetID), biglist)
length(unique(filt$DatasetID))
length(biglist)

##### check #####
filt %>% filter(grepl('EX-19-05', DatasetID)) %>%
  group_by(DatasetID) %>%
  summarize(n=n())

extracted_strings <- sub("\\.html$", "", repository)
x <- d %>% filter(class == 'Repository') %>% pull(DatasetID) %>% unique()
setdiff(x, extracted_strings)


d %>% filter(class == "Cruise") %>%
  pull(DatasetID) %>%
  unique()
217

d %>% filter(class == "Literature") %>%
  pull(DatasetID) %>%
  unique()

26

d %>% filter(class == "Program") %>%
  pull(DatasetID) %>%
  unique()

12

d %>% filter(class == "Repository") %>%
  pull(DatasetID) %>%
  unique()

38

##### publish a list of datasetID's for review sheet #####
x <- d %>% group_by(DatasetID, class) %>%
  summarize(n = n()) %>%
  arrange(class, DatasetID)

View(x)

##### write CSV of x #####
setwd("C:/rworking/deepseatools/indata")
x %>%
  write.csv(paste("20190718-0_DatasetID_Dashboard_Review",".csv", sep = ''), row.names = FALSE)

##### updating DatasetID key with new 'n' #####

# build a frequency table by DatasetID
x <- filt %>% group_by(DatasetID) %>% summarize(n=n())
names(key)

# strip n from fieds
y <- key[,1:8]
names(y)

# merge new numbers
z <- merge(y,x)
names(z)

# write out result
write.xlsx(z, "C:/rworking/deepseatools/indata/20191217-0_DatasetID_Key_DSCRTP.xlsx",
overwrite = TRUE)


##### write a subset of the NDB for testing purposes
## filter
export <- sub %>% filter(Flag == "0",
                         FishCouncilRegion == "New England")
## write
setwd("C:/rworking/deepseatools/indata")
export %>%
  write.csv(paste("yo",".csv", sep = ''), row.names = FALSE)

## checking
table(sub$Flag)
filt <- sub %>% filter(Flag == "0")
filt %>% filter(CatalogNumber == "618055") %>% pull(Latitude)





##### check #####
filt %>%
  pull(DatasetID) %>% unique()

filt %>%
  filter(EndLatitude != "-999") %>%
  pull(DatasetID) %>% table()


sub %>%
  filter(CatalogNumber == "912379") %>%
  dplyr::select(Latitude, Longitude)
=
