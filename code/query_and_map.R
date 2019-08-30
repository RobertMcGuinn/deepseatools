##### Heading #####

# author: Robert P. McGuinn, rpm@alumni.duke.edu
# date started: 20190208
# purpose: query and map the national database for deep sea corals and sponges

##### _____ #####
##### bringing in database #####
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20190718-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

##### bringing in some other dataset #####
x <- "Laidig_DSCRTP_NatDB_Submission_Nautilus2016_FINAL_071119.csv"
setwd("C:/rworking/deepseatools/indata")
d<-read.csv(x, header = T)

# setwd("C:/rworking/digs/indata")
# indata1<-read.csv("DSCRTP_NatDB_20190418-0.csv", header = T) #DSCRTP_NatDB_20181005-0.csv # DSCRTP_NatDB_20181211-2.csv
# filt <- indata1 %>%
#   filter(Flag == "0")

##### download DatasetID key that is stored on google drive as an xlsx file #####

# create a list of files (or single file) that meets title query
x <- drive_find(q = "name contains '20190709-0_DatasetID_Key_DSCRTP'")

# browse to it
drive_find(q = "name contains '20190709-0_DatasetID_Key_DSCRTP'") %>% drive_browse()

# getting the id as a character string
y <- x$id

# this downloads the file to the specified path
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/20190709-0_DatasetID_Key_DSCRTP.xlsx", overwrite = TRUE)

# read the file into R as a data frame
key <- read.xlsx(dl$local_path)

# clean up
rm(y)
rm(x)

##### download Google Sheet version of schema for use in R #####
# Register and download Google Sheet
s <- gs_title('2019_DSCRTP_National_Database_Schema')
# s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
# gs_browse(s)
s <- gs_read(s)
names(s)

##### look at valid values for a given field #####
y <- s %>%
  filter(
    grepl("PI", FieldName)
  ) %>%
  group_by(
    FieldName
  ) %>%
  summarise(
    description = toString(unique(FieldDescription)),
    valid_values = toString(unique(ValidValues)),
    internal = toString(unique(InternalUseOnly)),
    n=n()
  )

y$valid_values
y$description


##### download the most current taxonomy from Google Sheets #####
# https://drive.google.com/open?id=0B9c2c_XdhpFBT29NQmxIeUQ4Tlk

n <- '20190708-0'

taxfl <- gs_title(paste(n, '_taxonomy_to_flag',sep = ''))
taxfl <- gs_read(taxfl)

taxch <- gs_title(paste(n, '_taxonomy_to_change', sep = ''))
taxch <- gs_read(taxch)

tax <- gs_title(paste(n, '_taxonomy', sep = ''))
tax <- gs_read(tax)

##### look at a specific taxon #####

tax %>% filter(ScientificName == 'Dendrophylliidae') %>% dplyr::select(ScientificName, AphiaID)

##### _____ #####
##### query #####

did <- "SWFSC_AST"
q <- "History"

x <- indata %>%
  arrange(ObservationYear) %>%
  filter(
    # CatalogNumber == '605195'
    # grepl(q, Repository),
    # grepl(q, Citation),
    # Reporter == 'McGuinn, Robert P.',
    # SurveyID == 'RB-09-07',
    # SampleID == 'USNM 52851',
    # Genus == "Lophelia"
    grepl(did, DatasetID),
    # grepl(q, DataProvider)
    #is.na(Ocean) == T
  )

z <- key %>% filter(grepl(did, DatasetID))

# create a summary of an alternate version of 'indata'
# y <- indata1 %>%
#   arrange(ObservationYear) %>%
#   filter(
#     # Reporter == 'McGuinn, Robert P.',
#     # SurveyID == 'RB-09-07',
#     # SampleID == 'USNM 52851',
#     # Genus == "Lophelia"
#     grepl(did, DatasetID)
#   )

##### map x using leaflet #####
#install.packages("leaflet")
#x <- sub
# x <- sub  %>%
#   arrange(ObservationYear)  %>%
#   filter(
#     #grepl('Wagner, Daniel', DataProvider),
#     #is.na(RecordType) ==  T
#     #RecordType 'literature'
#     #Flag == '0',
#     #DatasetID == 'RSMAS'
#   )

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", x$CatalogNumber, "<br>",
                        "ScientificName:", x$ScientificName, "<br>",
                        "RecordType:", x$RecordType, "<br>",
                        "Vessel:", x$Vessel, "<br>",
                        "DatasetID:", x$DatasetID, "<br>",
                        "ObservationYear:", x$ObservationYear, "<br>",
                        "DataContact:", x$DataContact, "<br>",
                        "SurveyID:", x$SurveyID, "<br>",
                        "SampleID:", x$SampleID, "<br>",
                        "TrackingID:", x$TrackingID, "<br>",
                        "Station:", x$Station, "<br>",
                        "Locality:", x$Locality, "<br>",
                        "ImageURL:", x$ImageURL, "<br>",
                        "Latitude:", x$Latitude, "<br>",
                        "Longitude:", x$Longitude, "<br>",
                        "Observation Year:", x$ObservationYear))
m

##### map d (from alternate indata) using leaflet #####

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=d,
                      # lat = x$LatitudeInDD,
                      # lng = x$LongitudeInDD,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "CatalogNumber:", d$CatalogNumber, "<br>",
                        "ScientificName:", d$ScientificName, "<br>",
                        "RecordType:", d$RecordType, "<br>",
                        "Vessel:", d$Vessel, "<br>",
                        "DatasetID:", d$DatasetID, "<br>",
                        "ObservationYear:", d$ObservationYear, "<br>",
                        "DataContact:", d$DataContact, "<br>",
                        "SurveyID:", d$SurveyID, "<br>",
                        "SampleID:", d$SampleID, "<br>",
                        "TrackingID:", d$TrackingID, "<br>",
                        "Station:", d$Station, "<br>",
                        "Locality:", d$Locality, "<br>",
                        "ImageURL:", d$ImageURL, "<br>",
                        "Latitude:", d$Latitude, "<br>",
                        "Longitude:", d$Longitude, "<br>",
                        "Observation Year:", d$ObservationYear))
m

##### grouping operation ####

x <- indata %>%
  arrange(ObservationYear) %>%
  filter(
    #SampleID == 'USNM 52851',
    #Genus == "Lophelia"
    grepl("M2-10-06", DatasetID)
  ) # %>%
  group_by(DatasetID, Flag, DataProvider, Repository, AccessionID, PI, PIAffiliation, DataContact, Reporter, Citation) %>%
  summarise(
    RecordType_list = toString(unique(RecordType)),
    n=n(),
    DatasetID_list = toString(unique(DatasetID)),
    #Habitat_list = toString(unique(Habitat)),
    ObservationYear_list = toString(unique(ObservationYear)),
    ObservationDate_list = toString(unique(ObservationDate)),
    Vessel_list = toString(unique(Vessel)),
    VehicleName_list = toString(unique(VehicleName)),
    SamplingEquipment_list = toString(unique(SamplingEquipment)),
    SurveyID_list = toString(unique(SurveyID)),
    EventID_list = toString(unique(EventID)),
    SampleID_list = toString(unique(SampleID)),
    TrackingID_list = toString(unique(TrackingID)),
    SurveyComments_list = toString(unique(SurveyComments)),
    ReporterComments_list = toString(unique(ReporterComments)),
    Locality_list = toString(unique(Locality)),
    WebSite_list = toString(unique(WebSite))
    # #ImageURL = toString(unique(ImageURL)),
    # #PI_list = toString(unique(PI)),
    # #Citation_list = toString(unique(Citation)),
    # Species_list = toString(unique(ScientificName)),
    # ImageFilePath_list = toString(unique(ImageFilePath)),
    # ImageURL_list = toString(unique(ImageURL))
    # #DataContact_list = toString(unique(DataContact)),
    #Reporter_list = toString(unique(Reporter))
  )

# View(x)
# # setwd("C:/rworking/digs/indata")
# #
# # x %>%
# #   write.csv("x.csv", row.names = FALSE)
# #
# # xsheet <- gs_upload("x.csv")
# # #xsheet
# # gs_browse(xsheet, ws = 1)
#
# # setwd("C:/rworking/digs/outdata")
# # write.xlsx(x,'20190208_0_NOAA_VO-05-08_from_DSCRTP_NatDB_20190117-0.xlsx',row.names = FALSE)

##### summary query #####
x <- filt  %>%
  arrange(ObservationDate)  %>%
  filter(
    #grepl('Olympic', DataProvider)
    #ObservationYear == '2010'
    # is.na(RecordType) ==  T
    #RecordType 'literature'
    #Flag == '1'
    #DatasetID == "NOAA_LS-05-01",
    #ScientificName == 'Alcyonacea'
    grepl('Stone, Robert', PI)
  ) %>%
  group_by(ObservationYear, DatasetID) %>% #Flag, FlagReason, CatalogNumber, SampleID
  summarise(
    n=n(),
    ScientificName = paste(unique(ScientificName), collapse=" | "),
    VernacularNameCategory = paste(unique(VernacularNameCategory), collapse=" | "),
    RecordType = paste(unique(RecordType), collapse=" | "),
    #DatasetID = toString(unique(DatasetID)),
    WebSite = paste(unique(WebSite), collapse=" | "),
    DataProvider = paste(unique(DataProvider), collapse=" | "),
    Repository = paste(unique(Repository), collapse=" | "),
    #ObservationYear = paste(unique(ObservationYear), collapse=" | "),
    ObservationDate = paste(unique(ObservationDate), collapse=" | "),
    Vessel = paste(unique(Vessel), collapse=" | "),
    VehicleName = paste(unique(VehicleName), collapse=" | "),
    NumberImages = paste(unique(ImageURL), collapse=" | "),
    PI = paste(unique(PI), collapse=" | "),
    Citation = paste(unique(Citation), collapse=" | "),
    DataContact = paste(unique(DataContact), collapse=" | "),
    Reporter = paste(unique(Reporter), collapse=" | ")
    #FlagReason = paste(unique(FlagReason), collapse=" | ")
  )

View(x)

# length(x[is.na(x$ImageURL) == T, c('ImageURL')])

##### Google Drive output #####
setwd("C:/rworking/digs/indata")

x %>%
  write.csv("query.csv", row.names = FALSE)

xsheet <- gs_upload("query.csv")
gs_browse(xsheet, ws = 1)

###### ##### writing to ArcGIS geodatabase and shapefile #####
sub <- x
sub_geo <- sub
coordinates(sub_geo) <- c("Longitude", "Latitude")
proj4string(sub_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

fgdb_path <- 'C:/data/aprx/explore/explore.gdb'
arc.write(file.path(fgdb_path, 'this4'), data=sub_geo)

# -OR- write it to a shapefile (this unfortunately results in truncated variable names)
arc.write('C:/data/aprx/explore/shapefiles/this.shp', data=sub_geo)######

##### write CSV of x #####

setwd("C:/rworking/deepseatools/indata")

x %>%
  write.csv(paste("20190718_0_DataseID_NOAA_SWFSC_AST_from_DSCRTP_NatDB_20190709-3",".csv", sep = ''), row.names = FALSE)
