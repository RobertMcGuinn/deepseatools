##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## started on: 20200212
## purpose: downloading images from NOAA National Database for Deep Sea Corals and Sponges

##### packages #####
library(tidyverse)

##### load current version of national database #####
source('code/dst_tool_load_current_ndb.R')

##### query #####
z <- filt %>%
  filter(FishCouncilRegion == "Western Pacific",
         is.na(HighlightImageURL) == F)

##### check #####
length(z$CatalogNumber)
table(z$DatasetID)
unique(z$SampleID)
table(z$ScientificName)
summary(z$IndividualCount)
table(z$CategoricalAbundance)
hist(z$IndividualCount)
unique(z$ImageURL)

##### load images to folder #####
path <- 'C:/rworking/deepseatools/images/western_pacific'
#unlink(path, recursive = T)
dir.create(path)
setwd(path)

for(i in 1:length(z$CatalogNumber)){
  download.file(as.character(z$ImageURL[i]),
                destfile = paste("DSCRTP",
                                 z$CatalogNumber[i],
                                 z$DatasetID[i],
                                 z$ScientificName[i],
                                 z$DepthInMeters[i],
                                 basename(as.character(z$ImageURL[i])),
                                 sep = '_'),
                mode = "wb")
}


##### create a caption #####
z$caption <- paste0(z$ScientificName,
                    ', a ',
                    z$VernacularNameCategory,
                    ', collected on a cruise aboard the ',
                    z$Vessel,
                    ' using the ',
                    z$VehicleName,
                    ' ',
                    z$SamplingEquipment,
                    ' at ',
                    z$DepthInMeters,
                    ' in the ',
                    z$Locality,
                    ' on ',
                    z$ObservationDate,
                    '[DSCRTP CatalogNumber: ',
                    z$CatalogNumber,
                    '] ',
                    'Associated Taxa: ',
                    z$AssociatedTaxa,
                    ' ',
                    z$ImageURL)



z_select <- z %>% select(ImageURL, caption)

##### export Excel file of data records to folder #####
library(openxlsx)
setwd("C:/rworking/deepseatools/indata")
write.xlsx(z_select,
           '20261022-0_western_pacific_highlight_images.xlsx',
           rowNames = FALSE)




