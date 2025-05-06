##### Header #####
## code author: robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## purpose: download images from WAF

###### download all images and put them in a folder #####
z <- ndb %>% filter(is.na(ImageURL) == F)

##### check #####
length(z$ImageFilePath) # if this is zero, then the code below will return nothing

##### manual: change directory to send images #####
dir.create('C:/rworking/deepseatools/indata/ne_canyons_images', recursive = TRUE)
setwd("C:/rworking/deepseatools/indata/ne_canyons_images")

##### loop to download images #####
for(i in 1:length(z$CatalogNumber)){
  download.file(as.character(z$ImageURL[i]),
                destfile = paste("DSCRTP",
                                 z$CatalogNumber[i],
                                 z$ScientificName[i],
                                 z$DepthInMeters[i],
                                 basename(as.character(z$ImageURL[i])),
                                 sep = '_'),
                mode = "wb")
}
