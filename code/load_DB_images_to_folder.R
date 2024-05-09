##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## started on: 20200212
## purpose: downloading images from NOAA National Database for Deep Sea Corals and Sponges

##### packages #####
library(tidyverse)

##### data load #####
##### load national database (manual) #####
path <- "C:/rworking/deepseatools/indata"
csv <- "DSCRTP_NatDB_20240325-0.csv" # 'Aretha Franklin'
setwd(path)
indata <- read.csv(csv, header = T, encoding = 'latin1')
## encoding choice is either latin1 or UTF-8. Depends on incoming.
## this does not define encoding, it simply tells the importer
## which encoding is in the incoming file.
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### query #####
z <- filt %>% filter(
  is.na(ImageURL) == F,
  # IndividualCount != -999,
  # IndividualCount > 20,
  CategoricalAbundance == '2-10',
  # FishCouncilRegion == 'New England',
   ScientificName == "Primnoa resedaeformis",
  # CatalogNumber == 1188314
  grepl('NOAA_HB-19-03', DatasetID)
)

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
path <- 'C:/rworking/deepseatools/images/gardens'
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

