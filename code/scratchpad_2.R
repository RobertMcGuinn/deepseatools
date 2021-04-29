##### Header #####
# Author: Robert P. McGuinn
# Date Started: 20201001
# Purpose: clean scratch work space for October of 2020. No sense can be made.
#   You should not try.
##### load packages #####
library(tidyverse)
library(knitr)
library(flextable)
library(sp)
library(rgdal)
library(maps)
library(extrafont)
library(googlesheets4)
library(leaflet)
library(RColorBrewer)
library(sf)

##### load database #####
setwd("C:/rworking/deepseatools/indata")
indata <- read_csv("DSCRTP_NatDB_20210414-0.csv")
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### load an individual dataset #####
setwd("C:/rworking/deepseatools/indata")
## x is defined in the runner script
# or you could define it here like this
# x <- "20200701-0_NOAA_NEFSC_Connecticut_ISIS2_Towcam_Packer_2015_2015"
sub <- read.csv(paste(x,'.csv', sep = ''), header = T)
flagged <- sub %>%  filter(Flag == "1")

##### load schema #####
## download Google Sheet version of schema for use in R  documents

## Register and download Google Sheet using googlesheets4::read_sheet
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

## checking
s %>% filter(FieldName == 'RecordType') %>% pull(ValidValues)
s %>% filter(FieldName == 'LocationComments') %>% pull(FieldDescription)
s %>% filter(FieldName == 'IdentificationComments') %>% pull(FieldDescription)

##### checking some key filters #####
filt %>% filter(grepl("Nautilus", Vessel)) %>% pull(SurveyID) %>% unique()
filt %>% filter(grepl("Manta", Vessel)) %>% pull(SurveyID) %>% unique()
filt %>% filter(grepl("Okeanos", Vessel)) %>% pull(SurveyID) %>% unique()
filt %>% filter(grepl("EX1811", SurveyID)) %>% pull(DatasetID) %>% unique()
filt %>% filter(grepl("EX1903", SurveyID)) %>% pull(Reporter) %>% unique()
filt %>% filter(grepl("EX", AccessionID)) %>% pull(AccessionID) %>% unique()
filt %>% filter(grepl("EX1903", SurveyID)) %>% pull(Reporter) %>%  length()

sub %>% filter(VernacularNameCategory == 'check with dataprovider') %>%
  pull(AphiaID)

sub %>% filter(VernacularNameCategory == 'check with dataprovider') %>%
  pull(ScientificName) %>% unique()

sub %>% filter(is.na(IndividualCount) == T) %>% pull(IndividualCount)

sub %>% filter(grepl('Insufficient', FlagReason))%>%
  group_by(ScientificName, FlagReason, Flag) %>%
  summarize(n=n())

x <- sub %>% filter(grepl('DIVE', SampleID)) %>%
  group_by(SampleID) %>%
  summarize(n=n()) %>%
  arrange(desc(SampleID))
View(x)

##### accessionID Kaitlin Graff #####
indata %>% filter(AccessionID == 'NOAA_CBNMS_Nautilus_Graiff_2017_2017') %>%
  pull(CatalogNumber) %>%
  length()

indata %>% filter(AccessionID == 'NOAA_CBNMS_Nautilus_Graiff_2017_2017') %>%
  pull(SurveyID) %>%
  unique()

## write it out
setwd("C:/rworking/deepseatools/indata")
indata %>% filter(AccessionID == 'NOAA_CBNMS_Nautilus_Graiff_2017_2017') %>%
write_csv("20200710-2_NOAA_CBNMS_Nautilus_Graiff_2017_2017.csv")

filt %>% filter(grepl("Graiff", Reporter)) %>%
  pull(AccessionID) %>%
  unique()

filt %>% filter(grepl("YPM", DatasetID)) %>%
  pull(WebSite) %>% length() %>%
  unique()

##### summary for Tom Hourigan on Southeast data #####
z <- c("NOAA_SWFSC_Submersible", "NOAA_M2-10-06-L1-AUV", "NOAA_M2-10-06-L2-ROV", "NOAA_M2-10-06-L3-AUV", "NOAA_M2-10-06-L3-ROV", "NOAA_PU-11-08", "NOAA_PU-14-13")

setdiff(z, unique(filt$DatasetID))

summary <- filt %>% filter(DatasetID %in% z) %>%
  group_by(DatasetID) %>%
  summarize(number_eventID = length(unique(EventID)))
##### arvind #####
filt %>% filter(AccessionID == "NMNH_1868_2011") %>%
  group_by(AccessionID, Station) %>% summarize(foo=n()) %>% View()

##### QA #####
unique(filt$DatabaseVersion)
filt %>% filter(grepl("NOAA", DatasetID)) %>% pull(DatasetID) %>% unique()
sub %>% pull(AccessionID) %>% unique()
sub %>% pull(DataProvider) %>% unique()
filt %>% filter(grepl("Cordell", DataProvider)) %>% pull(DatasetID) %>% unique()
sub %>% group_by(ImageFilePath, SampleID) %>% summarize(n=n()) %>% head() %>% View()




##### best images thing #####
## see E. Gugliotti work https://drive.google.com/drive/folders/1LoSALOTqH3OZM6Aml8Hc_2E11fCDqcBH?usp=sharing #####
filt %>% filter(CatalogNumber == '530910') %>% pull(ImageURL) %>% browseURL()


