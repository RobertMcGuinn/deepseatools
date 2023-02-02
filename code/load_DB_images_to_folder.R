##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## started on: 20200212
## purpose: downloading images from NOAA National Database for Deep Sea Corals and Sponges

##### packages #####
library(tidyverse)

##### data load #####
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20221213-0.csv", header = T, encoding = 'latin9')
filt <- indata %>%
  filter(Flag == "0")
rm(indata)

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

##### query ERDDAP for list of CatalogNumbers #####
## get the list of CatalogNumbers into an variable
catlist <- z$CatalogNumber

## create the string that I need to pass into the URL
catlist_q <- paste0(catlist, collapse = "%7C")

## creating the URL (HTML output - select variables)
url <- paste('https://www.ncei.noaa.gov/erddap/tabledap/deep_sea_corals.htmlTable?ImageURL,ScientificName, CatalogNumber,DatasetID,Vessel,ObservationDate,gisMEOW,Locality,PI,IndividualCount,CategoricalAbundance&CatalogNumber=~%22(',catlist_q,')%22', sep = '')

## creating the URL (CSV output - all variables)
url_all_var <- paste('https://www.ncei.noaa.gov/erddap/tabledap/deep_sea_corals.csv?&CatalogNumber=~%22(',catlist_q,')%22', sep = '')

## browse to the URL
browseURL(url)
browseURL(url_all_var)

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
           '20220127-0_DENSE_Antipathes_furcata_images_NOAA_NDB_20220127-0_RPMGuinn.xlsx',
           rowNames = FALSE)


##### ERDDAP for a specific datasetID #####
DatasetID <- 'WHOI_AT-18'
url <- paste('https://www.ncei.noaa.gov/erddap/tabledap/deep_sea_corals.csv?CatalogNumber%2CVernacularNameCategory%2CVernacularName%2CScientificName%2CVerbatimScientificName%2CTaxonRank%2CAphiaID%2CPhylum%2CClass%2CSubclass%2COrder%2CSuborder%2CFamily%2CSubfamily%2CGenus%2CSubgenus%2CSpecies%2CSubspecies%2CScientificNameAuthorship%2CSynonyms%2CIdentificationComments%2CIdentifiedBy%2CIdentificationDate%2CIdentificationQualifier%2CAssociatedSequences%2CIndividualCount%2CCategoricalAbundance%2CDensity%2CCover%2CVerbatimSize%2CWeightInKg%2CCondition%2CAssociatedTaxa%2COccurrenceComments%2CSubstrate%2CHabitat%2CTemperature%2CSalinity%2CpH%2CpHscale%2CpCO2%2CTA%2CDIC%2COcean%2CLargeMarineEcosystem%2CCountry%2CFishCouncilRegion%2CLocality%2CStation%2CObservationDate%2CObservationYear%2CObservationTime%2Clatitude%2Clongitude%2CDepthInMeters%2CDepthMethod%2CgisEtopoDepth%2CgisGEBCODepth%2CgisCRMDepth%2CStartLatitude%2CStartLongitude%2CEndLatitude%2CEndLongitude%2CMinimumDepthInMeters%2CMaximumDepthInMeters%2CLocationAccuracy%2CNavType%2CLocationComments%2CSurveyID%2CVessel%2CPI%2CPIAffiliation%2CPurpose%2CSurveyComments%2CEventID%2CSamplingEquipment%2CVehicleName%2COtherData%2CDataProvider%2CDataContact%2CModified%2CRecordType%2CRepository%2CCitation%2CSampleID%2CTrackingID%2CDatasetID%2CWebSite%2CImageURL&DatasetID=%22', DatasetID,'%22', sep = '')
