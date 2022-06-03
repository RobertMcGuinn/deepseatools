##### Header #####
## Date started
## Author: Robert McGuinn
## Purpose: Alaska density relationships

##### packages #####
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)

##### bring current DB in #####
setwd("C:/rworking/deepseatools/indata")
indata <- read.csv("DSCRTP_NatDB_20220426-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

## cleanup
rm(indata)

##### checking #####
unique(filt$DatabaseVersion)

##### query #####
filt %>% filter(FishCouncilRegion == "North Pacific",
                ObservationYear == "2012" |
                  ObservationYear == "2014") %>%
  group_by(ObservationYear, Vessel, Locality, SurveyID, DatasetID) %>%
  summarize(n=n()) %>%
  View()

#####
x <- filt %>% filter(grepl("Alaska Endeavor", Vessel)) %>%
  pull(DatasetID) %>%
  unique()

paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/",
      x,
      ".html",
      sep = "")

x <- filt %>% filter(grepl("Vesteraalen", Vessel),
                     DatasetID == "NOAA_VA-14-08") %>%
  pull(DatasetID) %>%
  unique()

paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/",
      x,
      ".html",
      sep = "")

x <- filt %>% filter(grepl("Vesteraalen", Vessel),
                     DatasetID == "NOAA_VA-14-08") %>%
  pull(DatasetID) %>%
  unique()

paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/",
      x,
      ".html",
      sep = "")


##### looking at density values for specific datasets #####
filt %>% filter(DatasetID == "NOAA_SS-12-08",
                Density != -999) %>%
  pull(Density) %>%
  hist()

filt %>% filter(DatasetID == "NOAA_AE-14-04",
                Density != -999) %>%
  pull(Density) %>%
  hist()

#####

x <- filt %>% filter(CatalogNumber == "250128") %>%
  group_by(ScientificName, DatasetID, SampleID, Locality, gisMEOW, ImageURL) %>%
  summarize(n=n())

View(x)


