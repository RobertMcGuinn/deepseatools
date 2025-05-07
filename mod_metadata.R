##### Header #####
## author: robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## date_started: 20250507
## purpose: download metadata from WAF

###### ***** download all metadata and put it in a folder ***** #####
###### create a list of datasetID's
z <- unique(filt$DatasetID)

##### manual: change directory to send images #####
dir.create('C:/rworking/deepseatools/indata/metadata', recursive = TRUE)
setwd("C:/rworking/deepseatools/indata/metadata")

##### loop to download images #####
for(i in z){
  download.file(paste('https://www.ncei.noaa.gov/waf/dsc-data/metadata/', i, '.xml', sep = ''),
                destfile = paste(i,'.xml', sep=''),
                mode = "wb")
}
