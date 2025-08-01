##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250731
## purpose:taking a look at the H3 gap analysis

##### linkage #####
filename <- 'h3_gap_analysis' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.Rmd', sep = '')
# browseURL(github_link)

##### packages #####
library(tidyverse)
library(sf)
library(terra)

##### load the geojson data to sf (level3) #####
my_geojson_file <- "c:/rworking/deepseatools/indata/h3_gap/h3_hexagons_03.geojson"
level3_sp <- st_read(my_geojson_file)

##### load the CSV data (level3) #####
my_csv_file <- "c:/rworking/deepseatools/indata/h3_gap/h3_scores_03.csv"
level3_tab <- read.csv(my_csv_file)

##### check #####
level3_tab %>% pull(combined) %>% table()
dim(level3_tab)
##### filter on combined score #####
x <- level3_tab %>% filter(combined > 0.5) %>% pull(h3_index) %>% unique()

##### get the corresponding filtered hexagons from the spatial layer #####
plot(level3_sp %>% filter(h3_index %in% x))





