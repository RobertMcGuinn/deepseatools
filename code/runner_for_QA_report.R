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
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### render the QA dashboard #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20220414-1_NOAA_SWFSC_SH1812_2018"

## render
rmarkdown::render("C:/rworking/deepseatools/code/20220629_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

## manual change: make sure your target RMD in the render function step is correct.
##### MANUAL inspection of QA report in Word, #####
## manual: then Develop Redmine Checklist
## manual: then SAVE to PDF.
##### upload PDF report to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1WcL_fMRU7SXpsc_60OWd5XzRQBvyCJOz"
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
  filter(grepl("Monterey", DataProvider)) %>%
  group_by(ImageURL) %>%
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
# sub <- filt %>%
#   filter(DatasetID == 'BOEM_Lophelia_I')


m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap")
m <- addCircleMarkers(m, data=sub,
                      radius=2,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=.5,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", sub$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", sub$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", sub$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", sub$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", sub$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", sub$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", sub$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", sub$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", sub$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", sub$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", sub$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", sub$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", sub$EventID, "<br>",
                        "<b><em>","Latitude:","</b></em>", sub$Latitude, "<br>",
                        "<b><em>","Longitude:","</b></em>", sub$Longitude, "<br>",
                        "<b><em>","Image:","</b></em>",sub$ImageURL))

m

##### export points and dive centers to GIS #####
## load packages
library(tidyverse)
library(rgdal)
install.packages("arcgisbinding")
library(arcgisbinding)
arc.check_product()

## create x from sub
x <- sub

## filter data
# get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 | Longitude != -999)
# make copy to turn into spatial points data frame.
x_geo <- x

## create spatial points data frame
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class
fgdb_path <- 'C:/rworking/sf/sf.gdb'
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
x <- filt %>%
  filter(grepl("Shimada", Vessel),
         ObservationYear == 2019) %>%
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







