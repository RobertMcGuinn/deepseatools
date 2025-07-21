##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250210
## purpose: searching by catalognumber

##### linkage #####
filename <- 'mod_obis' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(robis)
library(leaflet)

##### source NDB #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### create table uniqueScientificName  #####
sci_names <- filt %>%
  filter(FishCouncilRegion == 'Caribbean') %>%
  pull(ScientificName) %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))

View(sci_names)

## rename columns for clarity
colnames(sci_names) <- c("ScientificName", "Frequency")
View(sci_names)

##### search OBIS API for particular ScientificName #####
name <- 'Amphimedon compressa'
obis_data <- occurrence(scientificname = name,
                        datasetid = 'f5a4799e-dc24-4807-89d9-01da47d52e3b')

##### check #####
filt %>%
  filter(ScientificName == name) %>%
  pull(CatalogNumber) %>% length()

##### get a species list from a region #####
# Get a full species list for the Gulf of Mexico
species_list <- checklist(geometry = "POLYGON((-97 20, -81 20, -81 30, -97 30, -97 20))",
                          datasetid = 'f5a4799e-dc24-4807-89d9-01da47d52e3b')

# View the first few species
head(species_list)

##### get a specific taxonID
target_id <- species_list$taxonID[3]

##### find and map occurrence records for a specific taxonid #####
occurrences_for_id <- occurrence(taxonid = target_id)
                                 # datasetid = 'f5a4799e-dc24-4807-89d9-01da47d52e3b')
                                 # geometry = "POLYGON((-97 20, -81 20, -81 30, -97 30, -97 20))")
library(leaflet)
map_leaflet(occurrences_for_id)







