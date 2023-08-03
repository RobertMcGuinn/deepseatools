##### Header #####
## author: Robert McGuinn
## started: 20200921

##### packages #####
library(bookdown)
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)
library(googlesheets4)

##### authorizations #####
## Set authentication token to be stored in a folder called \.secrets``
# options(gargle_oauth_cache = ".secrets")

## Authenticate manually
# gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:

# list.files(".secrets/")

## Check that the non-interactive authentication works by first deauthorizing:
# gs4_deauth()

## Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### load NDB from local file (manual)#####
digits = 12
setwd("C:/rworking/deepseatools/indata")
filename <- "DSCRTP_NatDB_20230620-0.csv"
indata <- read.csv(filename,
                   encoding = "latin9",
                   header = TRUE,
                   stringsAsFactors = FALSE)
filt <- indata %>%
  filter(Flag == 0)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))

##### render the QA dashboard #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'
knitr::opts_knit$set(resource.path = "c:/rworking/deepseatools/code")
setwd('c:/rworking/deepseatools/code')

filename <- "20230802-0_NOAA_HB1404_TowCam_fishes_MRhode_2014.csv"
# 20230719-2_NOAA_HB1404_TowCam_fishes_MRhode_2014

## render
## manual change version of dashboard version number is required
rmarkdown::render("C:/rworking/deepseatools/code/20230731-0_rmd_accession_qa_dashboard.Rmd",
                  output_file =  paste(filename,".docx", sep=''),
                  output_dir = 'C:/rworking/deepseatools/reports')


##### check #####
sub %>%
  # filter(ScientificName == "Keratoisis magnifica") %>%
  group_by(Flag,
           FlagReason,
           ScientificName,
           Phylum,
           Class,
           Order,
           Family,
           Genus,
           Species,
           VernacularNameCategory
  ) %>% summarize (n=n()) %>%
  View()

sub %>% pull(DepthInMeters) %>% median()
sub %>% filter(DepthInMeters == "68523") %>% pull(SampleID)
sub %>% pull(SurveyID) %>% table()
sub %>% pull(RecordType) %>% table()
s %>% filter(FieldName == "DataContact") %>% pull(FieldDescription)
sub %>% filter(grepl('>100', VerbatimSize)) %>% pull(MaximumSize)
sub %>% filter(is.na(LocationAccuracy) == T) %>% rownames()
sub %>% filter(grepl('>100', VerbatimSize)) %>% rownames()
rownames(sub %>% filter(grepl('>100', VerbatimSize)))
sub$row <- rownames(sub)
sub %>% filter(is.na(LocationAccuracy) == T) %>% pull(row)
sub %>% filter(row == "246") %>% pull(LocationAccuracy)
sub %>% filter(row == "247") %>% pull(LocationAccuracy)
sub %>% filter(row == "248") %>% pull(LocationAccuracy)
sub %>% slice(246:248) %>% pull(SampleID)
sub %>% filter(DepthInMeters > 30000) %>% pull(SampleID)

x <- paste(sub$SampleID, sub$ScientificName, sub$VerbatimScientificName)
table(duplicated(x))

filt %>% filter(grepl("NOAA", DataProvider)) %>% pull(DataProvider) %>% unique()

sub %>% filter(DepthInMeters>500) %>% select(CatalogNumber, DepthInMeters)

yo <- read.delim('c:/rworking/deepseatools/indata/20221031-0_NOAA_EX1304_Northeast_US_SBingo_2013.txt', sep = '\t')

yo %>% filter(DepthInMeters > 7000) %>% select(TrackingID, DepthInMeters)
sub %>% filter(DepthInMeters > 7000) %>% select(TrackingID, DepthInMeters)


# Acanthogorgia spissa (N=4)
# Anthomastus gyratus (N=1)
# Anthoptilum gowlettholmesae (N=4)
# Anthothela vickersi (N=5)
# Aphanostichopathes paucispina (N=1)
# Calibelemnon francei (N=1)
# Cladarisis nouvianae (N=2)
# Cornulariidae (N=1)
# Distichopathes hickersonae (N=2)

## manual change: make sure your target RMD in the render function step is correct.
##### MANUAL inspection of QA report in Word #####
## manual: then Develop Redmine Checklist
## manual: then SAVE to PDF
##### upload PDF report to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1Yz_-aOHIkZJ-WP2vdhUGVfMCUKQfxlgv"
setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)
##### checking #####
unique(sub$DatasetID)
filt %>% filter(grepl("HB", DatasetID)) %>% pull(DatasetID) %>% table()
unique(sub$SurveyID)
filt %>% filter(grepl("HB", SurveyID)) %>% pull(SurveyID) %>% table()
unique(sub$EventID)
unique(sub$Citation)
unique(sub$Repository)

x <- filt %>%
  filter(DatasetID == 'MBARI') %>%
  group_by(DataContact, DataProvider, ImageURL) %>%
  summarize(n=n())
View(x)

x <- filt %>%
  filter(FishCouncilRegion == "Caribbean" |
           FishCouncilRegion == "South Atlantic" |
           FishCouncilRegion == "Caribbean") %>%
  group_by(FishCouncilRegion, Vessel, EntryDate, ObservationYear) %>%
  summarize(n=n())
View(x)

x <- filt %>%
  filter(grepl("NOAA", DataProvider)) %>%
  group_by(DataProvider) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  group_by(Locality) %>%
  summarize(n=n())
View(x)

setwd("C:/rworking/deepseatools/indata")
sub <- read.csv("dsc_natdb.csv", header = TRUE)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

x <- filt %>%
  filter(FishCouncilRegion == "Caribbean" |
           FishCouncilRegion == "South Atlantic" |
           FishCouncilRegion == "Gulf of Mexico",
         as.Date(EntryDate) > "2019-10-01") %>%
  group_by(FishCouncilRegion, Vessel, VehicleName, EntryDate, ObservationYear) %>%
  summarize(n=n())
View(x)
length(x$CatalogNumber)

x <- filt %>%
  filter(grepl("NA", DatasetID)) %>%
  group_by(Vessel, DatasetID, DataProvider) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter(CategoricalAbundance == 'minimum count') %>%
  group_by(ScientificName, FlagReason, IndividualCount) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter() %>%
  group_by() %>%
  summarize(n=n())
View(x)

filt %>%
  filter(grepl("Deep Sea Coral", Repository)) %>%
  group_by(Repository, DatasetID) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(grepl("YOGI", VehicleName)) %>%
  group_by(DataProvider, VehicleName) %>%
  summarize(n=n()) %>% View()

x <- sub %>%
  filter(DepthInMeters < 50) %>%
  group_by(ScientificName, FlagReason, DepthInMeters, DepthMethod, ShallowFlag) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  group_by(CategoricalAbundance, IndividualCount) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter(VernacularNameCategory == "nipple foliose sponge (yellow)") %>%
  group_by(Flag, FlagReason, ScientificName) %>%
  summarize(n=n())
View(x)

s %>% filter(FieldName == "Modified") %>% pull(ValidValues)
s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(ValidValues)
s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(FieldDescription)
s %>% filter(FieldName == "TaxonRank") %>% pull(ValidValues)

##### checking #####
sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(RecordType, SamplingEquipment, Repository, ) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(IndividualCount, CategoricalAbundance) %>%
  summarize(n=n()) %>%
  View()

filt$DatasetURL <- paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/", filt$DatasetID, ".html", sep = "")
#head(filt$DatasetURL)

x <- "Yoklavich"
yo <- filt %>%
  filter(grepl(x, DataContact)|
           grepl(x, Reporter)|
           grepl(x, PI)|
           grepl(x, DatasetID) |
           grepl(x, Repository))

unique(yo$DatasetURL)


filt %>%
  filter(grepl("Southwest Fisheries", DataProvider)) %>%
  group_by(DatasetID, SurveyID, SamplingEquipment, DataContact, PI, Reporter) %>%
  summarize(n=n()) %>%
  View()

x <- filt %>%
  filter(grepl("Southwest Fisheries", DataProvider) |
           grepl("Northwest Fisheries", DataProvider))

sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(ObservationDate) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  filter(grepl("Desmophyllum", ScientificName)) %>%
  group_by(ScientificName) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(grepl("140", SurveyID)) %>%
  group_by(Vessel, SurveyID, AccessionID, DatasetID, DataProvider, SamplingEquipment ) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(grepl("ftp:", WebSite)) %>%
  group_by(WebSite, Citation, DatasetID, SurveyID, ObservationDate) %>%
  summarize(n=n()) %>%
  View()

x <- s %>%
  filter(FieldName == 'LocationAccuracy') %>%
  pull(ValidValues) %>%
View()

x <- s %>%
  filter(FieldName == 'OtherData') %>%
  pull(FieldDescription) %>%
  View()

##### mapit using leaflet #####
## optional create new 'sub' ##
sub2 <- sub
sub2$Longitude <- as.numeric(sub$Longitude)-6

   # filter(CatalogNumber == "1178074")
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap")
m <- addCircleMarkers(m, data=sub2,
                      radius=2,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=.5,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", sub2$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", sub2$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", sub2$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", sub2$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", sub2$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", sub2$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", sub2$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", sub2$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", sub2$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", sub2$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", sub2$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", sub2$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", sub2$EventID, "<br>",
                        "<b><em>","Latitude:","</b></em>", sub2$Latitude, "<br>",
                        "<b><em>","Longitude:","</b></em>", sub2$Longitude, "<br>",
                        "<b><em>","Image:","</b></em>",sub2$ImageURL))

m

##### export points and dive centers to GIS #####
# install.packages("arcgisbinding")
library(arcgisbinding)
arc.check_product()

## create x from sub
sub2 <- sub
sub2$Longitude <- as.numeric(sub$Longitude)-6
x <- sub2

## filter data
# get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 | Longitude != -999)
# make copy to turn into spatial points data frame.
x_geo <- x

## create spatial points data frame
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class
fgdb_path <- 'C:/rworking/deepseatools/gis/gis.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)

### Export dive points

## create summary by EventID
x <- sub %>% filter(Flag == 0) %>%
  group_by(EventID) %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude)
  )

## get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 |
                    Longitude != -999)

## make copy to turn into spatial points data frame.
x_geo <- x

## create spatial points data frame
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class

fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo_dives'), data=x_geo, overwrite = TRUE)

##### checking #####
yo <- filt %>% filter(grepl("Thomas", Vessel)) %>%
  group_by(DataProvider, SamplingEquipment, Vessel, VehicleName) %>%
  summarize(n=n())
View(yo)

x <- filt %>%
  filter(grepl("NOAA_SWFSC_AST", DatasetID)) %>%
       #  ObservationYear == 2019) %>%
  group_by(DatasetID,
           PI,
           PIAffiliation,
           NavType,
           Vessel,
           ObservationYear,
           SurveyID,
           VehicleName,
           SamplingEquipment,
           DataContact,
           Reporter,
           ReporterEmail) %>%
  summarize(n=n())
View(x)

write.csv(x, "c:/rworking/deepseatools/indata/20221027_pacific_cruises_post_2016_RPMcGuinn.csv")

sub %>%
  group_by(SampleID, EventID) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(grepl("West Coast", FishCouncilRegion)) %>%
  group_by(Vessel,IdentificationQualifier, IdentificationVerificationStatus, DataProvider, RecordType, VehicleName, SamplingEquipment, Repository, PI, Reporter, ReporterEmail) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(grepl("Alaska", DataProvider)) %>%
  group_by(DatasetID, Vessel, SurveyID, ObservationYear) %>%  summarize(n=n()) %>% View()

sub %>%
  #filter(grepl("Alaska", DataProvider)) %>%
  group_by(SurveyID, EventID, Station, Locality, ObservationYear) %>%
  summarize(n=n()) %>% View()

sub %>%
  #filter(FlagReason == "Invalid latitude | Invalid longitude") %>%
  group_by(Flag, Latitude, Longitude, EndLatitude, StartLatitude, EndLongitude, StartLongitude) %>%
  summarize(n=n()) %>% View()


sub %>%
  filter(FlagReason == "Possible intersection with land") %>%
  group_by(Flag, Latitude, Longitude) %>%
  summarize(n=n()) %>% View()

##### check side by side #####
x <- filt %>%

  filter(grepl("Pacific", FishCouncilRegion)) %>%
  group_by(DatasetID,
           PIAffiliation,
           NavType,
           LocationAccuracy,
           Vessel,
           ObservationYear,
           EventID,
           SurveyID,
           IdentificationQualifier,
           IdentificationVerificationStatus,
           DataProvider,
           RecordType,
           VehicleName,
           SamplingEquipment,
           Repository,
           PI,
           Reporter,
           ReporterEmail) %>%
  summarize(n=n())

View(x)

y <- sub %>%
  # filter(grepl("AFSC", DatasetID)) %>%
  group_by(DatasetID,
           PIAffiliation,
           NavType,
           LocationAccuracy,
           Vessel,
           EventID,
           SurveyID,
           IdentificationQualifier,
           IdentificationVerificationStatus,
           DataProvider,
           RecordType,
           VehicleName,
           SamplingEquipment,
           Repository,
           PI,
           Reporter,
           ReporterEmail) %>%
  summarize(n=n())
View(x)
View(y)







