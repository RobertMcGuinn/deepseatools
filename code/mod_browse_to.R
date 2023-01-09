##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: browse to datasetID

##### packages #####
library(tidyverse)

##### define variables #####
datasetid <- 'HURL'

url <- paste('https://deepseacoraldata.noaa.gov/Dataset%20Summaries/',
             datasetid,
             '.html',
             sep = '')

browseURL(url)
