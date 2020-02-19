##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## started on: 20200212
## purpose: downloading images from NOAA National Database for Deep Sea Corals and Sponges

##### data load #####
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20191217-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

##### query and load images to folder #####
name <- "Bathypathes"
z <- filt %>% filter(is.na(ImageURL) == F,
                     Genus == name,
                     Ocean == 'North Atlantic' | Ocean == 'South Atlantic'
)

setwd("C:/rworking/deepseatools/indata/image_set_Wagner")
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


##### export Excel file of data records to folder #####

library(openxlsx)
setwd("C:/rworking/deepseatools/indata")
write.xlsx(z,'20200212-0_subset_Bathypathes_with_Images_NOAA_NDB_20191217-0_RPMGuinn.xlsx', row.names = FALSE)


