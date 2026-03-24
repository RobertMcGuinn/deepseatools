##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## started on: 20200212
## purpose: downloading images from NOAA National Database for Deep Sea Corals and Sponges

##### packages #####
library(tidyverse)

##### load current version of national database #####
source('code/dst_tool_load_current_ndb.R')

##### query #####
z <- filt #%>% filter(
  # DatasetID == 'OET_NA165',
  # is.na(ImageURL) == F,
  # IndividualCount != -999,
  # IndividualCount > 2,
  # CategoricalAbundance == '2-10',
  # FishCouncilRegion == 'New England',
  # ScientificName == "Neoacis",
  # CatalogNumber == 1204882
  # grepl('NOAA_HB-19-03', DatasetID)
#)

##### check #####
length(z$CatalogNumber)
table(z$DatasetID)
unique(z$SampleID)
table(z$ScientificName)
summary(z$IndividualCount)
table(z$CategoricalAbundance)
hist(z$IndividualCount)
unique(z$ImageURL)

##### select specific images, like highlight #####
highlights <- c(1744954,1752562, 1769248)
z <- z %>% filter(CatalogNumber %in% highlights)

##### load images to folder #####
path <- 'C:/rworking/deepseatools/images/'
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

##### export Excel file of data records to folder #####
library(openxlsx)
setwd("C:/rworking/deepseatools/indata")
write.xlsx(z,
           'yo.xlsx',
           rowNames = FALSE)


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

##### print caption #####
z %>% pull(caption)


##### image notes #####
x<- c('https://www.ncei.noaa.gov/waf/dsc-data/images/001744/1744954.jpg',
'https://www.ncei.noaa.gov/waf/dsc-data/images/001752/1752562.png',
'https://www.ncei.noaa.gov/waf/dsc-data/images/001769/1769248.png')


filt %>% filter(ImageURL %in% x) %>% pull(CatalogNumber)



