##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250211
## purpose: Smithsonian API

##### linkage #####
filename <- 'mod_smithsonian' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

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
library(httr)
library(jsonlite)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### set API key and define base url#####
## c:/keys/2025211datagov.txt
key <- 'ACtfCdatKgnmfSKqht4njYzku1FcW6gj68f8jH6j'

# Define API Key
api_key <- key  # Replace with your actual key

# Define base URL
base_url <- "https://api.si.edu/openaccess/api/v1.0/search"

##### design query #####
query <- "1071877"
full_url <- paste0(base_url, "?q=", URLencode(query), "&api_key=", api_key)

##### make the API request #####
response <- GET(full_url)

## parse JSON response
data <- content(response, as = "text")
parsed_data <- fromJSON(data, flatten = TRUE)

##### check out the results #####
## look at the ark:
parsed_data$response$rows$content.descriptiveNonRepeating.record_link

## look at the ScientificName
parsed_data$response$rows$content.indexedStructured.scientific_name

## look at the data source
parsed_data$response$rows$content.freetext.dataSource


parsed_data <- fromJSON(data, flatten = TRUE)


library(purrr)



record <- parsed_data$response$rows[[1]]
names(record)



library(purrr)
library(dplyr)

# See which rows have indexedStructured data
has_structure <- map_lgl(parsed_data$response$rows$content.indexedStructured, ~ !is.null(.x))
which(has_structure)






