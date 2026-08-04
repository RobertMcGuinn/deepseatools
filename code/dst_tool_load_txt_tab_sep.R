##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260803
## purpose: open txt with tab separated values.

##### parameters #####
##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(file_name)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)

##### packages #####
library(readr)

##### parameter #####
# Install readr if you haven't already: install.packages("readr")

file_path <- "C:/rworking/deepseatools/indata/dwca-noaa_dsc_rtp-v1.36/occurrence.txt"

##### Load the data #####
occurrence_data <- read_tsv(file_path)
