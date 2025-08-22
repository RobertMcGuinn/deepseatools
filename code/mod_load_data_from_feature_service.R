##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250818
## purpose: working with published feature services

##### linkage #####
filename <- 'feature_service_attributes' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- ''
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(arcgislayers)

##### parameters #####
fs_url <- "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/arcgis/rest/services/DSCRTP_NatDB/FeatureServer"

##### get the layer #####
fs <- arc_open(fs_url)
layer <- get_layer(fs, id = 0)

##### get service as an sf file (WARNING: this takes a while) #####
fs_table <- arc_select(layer, out_fields = "*", n_max = Inf)











