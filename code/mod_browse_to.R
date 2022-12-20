##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: open dashboard or other url

##### packages #####
library(tidyverse)

##### check #####
# filt %>%
#   filter(grepl("HURL", DatasetID)) %>%
#   group_by(Locality) %>%
#   summarize(n=n())

##### browse to a specified DatasetID dashboard #####
datasetid <- 'HURL'
url <- paste('https://deepseacoraldata.noaa.gov/Dataset%20Summaries/', datasetid, '.html', sep = '')
browseURL(url)
