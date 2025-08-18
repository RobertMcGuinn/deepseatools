##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250814
## purpose: interact with GBIF data through the rgbif package

##### linkage #####
filename <- 'rgbif' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(rgbif)
library(tidyverse)

##### count records by basisOfRecord #####
occ_count(basisOfRecord='OBSERVATION')

##### count records for 'Puma concolor' using taxonKey with lat/long data (georeferened) only #####
## Note that hasCoordinate in occ_search() is the same as georeferenced in occ_count().
occ_count(taxonKey = 2435099,
          hasCoordinate = T,
          hasGeospatialIssue = F
          )

##### get records from a specific place name #####
denmark_code <- isocodes[grep("Denmark", isocodes$name), "code"]
occ_count(country=denmark_code)

##### count records in a particular dataset #####
# this key is Smithsonian: https://www.gbif.org/dataset/821cc27a-e3bb-4bc5-ac34-89ada245069d
# this key is for the DSCRTP: df8e3fb8-3da7-4104-a866-748f6da20a3c
occ_count(datasetKey='df8e3fb8-3da7-4104-a866-748f6da20a3c')

# All records from 2012 in the DSCRTP database
occ_count(datasetKey='df8e3fb8-3da7-4104-a866-748f6da20a3c', year=2012)

##### this is how you search gbif and extract data.frames#####
x <- occ_search(datasetKey='df8e3fb8-3da7-4104-a866-748f6da20a3c',
                scientificName = "Lophelia pertusa", limit = 10)
y <- data.frame(x$data)

##### isolate the 'CatalogNumber' vector #####
cats <- y$occurrenceID
cats <- sub(".*:", "", cats)

##### check for chosen 'CatalogNumber' in cats #####

filt %>% filter(CatalogNumber %in% cats) %>%
  pull(ScientificName) %>% unique()






