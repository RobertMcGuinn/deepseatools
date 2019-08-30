##### New Data Patch / Initial Conform #####
# Original Code Author: Robert McGuinn (rpm@alumni.duke.edu, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu)
# Latest Status: Working with new Google Drive import of the schema.

##### Installation/Loading of Packages #####
library(stringr)
library(knitr)
library(maptools)
library(maps)

#install.packages("psych")

library(psych)

#install.packages("ggplot2")

library(ggplot2)

#install.packages("data.table")

library(data.table)

#install.packages("dplyr")
library(dplyr)

#install.packages("car")

#library(car)

#install.packages("gdata")

library(gdata)

library(stringr)

#install.packages("rgdal")

library(rgdal)

library(ggmap)

require(gdata)
library(googlesheets)
library(googledrive)

##### Data Input and Preparation #####
#set variables
DatasetID <- "20180712_1_R2006_database-TH_RPMcGuinn"
taxdataID <- "20180618_1_taxonomy.csv"

#bring in input data
indata <- read.csv(paste("C:/rworking/digs/indata/", paste(DatasetID, ".csv", sep = ""), sep = ""),header = T)
#indata <-read.table(paste("C:/rworking/digs/indata/", paste(DatasetID, ".txt", sep = ""), sep = ""), sep = "\t", allowEscapes=T, header = T)

#bring in input schema
# Register and download Google Sheet
s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
schema<- gs_read(s)
#gs_browse(s)

setwd("C:/rworking/digs/indata")
write.csv(schema, "2018_DSCRTP_Schema.csv")
schema <- read.csv(paste("C:/rworking/digs/indata/", paste("2018_DSCRTP_Schema", ".csv", sep = ""), sep = ""),header = T)

#bring in taxonomy table
tax <- read.csv(paste("C:/rworking/digs/indata/", taxdataID, sep = ""), header = T)

#deduplicate the taxonomy table
tax <- subset(tax,!duplicated(tax$ScientificName))

##### Define Key Functions #####
Trim <- function(x){
  gsub("^\\s+|\\s+$", "", x)
}
TrimAll <- function(x){
  dat<-Trim(indata[,c(x)])
  indata[,c(x)] <- dat
}
NumericNull<-function(x){
  dat <- indata[,c(x)]
  indata[is.na(dat) == T | dat == "" | dat == "NA", c(x)] <<- -999
}
FactorNull<-function(x){
  dat <- indata[,c(x)]
  indata[is.na(dat) == T | dat == "" | dat == "NA", c(x)] <<- NA
}
String <- function(x="") {
  x <- as.character(paste(x, collapse=""))
  class(x) <- c("String","character")
  return(x)
}

"[.String" <- function(x,i,j,...,drop=TRUE) {
  unlist(strsplit(x,""))[i]
}
"[<-.String" <- function(x,i,j,...,value) {
  tmp <- x[]
  tmp[i] <- String(value)
  x <- String(tmp)
  x
}
print.String <- function(x, ...) cat(x, "\n")

##### Define DSCRTP Schema Templates #####

# Full DSCRTP schema variables template
#TMPL_FullVariables <- read.csv("c:/rworking/digs/templates/20150604_TMPL_FullVariables.csv", header = F, stringsAsFactors = FALSE)
TMPL_FullVariables <- schema$FieldName
TMPL_FullVariables <- as.character(TMPL_FullVariables)
TMPL_FullVariables <- Trim(TMPL_FullVariables)

# DSCRTP schema numeric variables template
schemafilt <- schema %>% filter(
  DataType == "Float" |
  DataType == "Integer" |
  DataType == "Logical"
  )

TMPL_NumVariables <- schemafilt$FieldName
TMPL_NumVariables <- as.character(TMPL_NumVariables)
TMPL_NumVariables <- Trim(TMPL_NumVariables)

# DSCRTP schema text variables template

schemafilt <- schema %>%
  filter(
    DataType == "Character" |
      DataType == "Date" |
      DataType == "Factor"
  )

TMPL_CharVariables <- schemafilt$FieldName
TMPL_CharVariables <- as.character(TMPL_CharVariables)
TMPL_CharVariables <- Trim(TMPL_CharVariables)

##### Define Standards for the Data Category #####
# Load templates for REQUIRED and DESIRED fields:
# These will be in the "templates" directory
# It is critical that you load the templates for the correct data category

schemafilt <- schema %>%
  filter(
    PointProgram == "R"
  )

TMPL_RequiredFields <- schemafilt$FieldName
TMPL_RequiredFields <- as.character(TMPL_RequiredFields)
TMPL_RequiredFields <- Trim(TMPL_RequiredFields)

schemafilt <- schema %>%
  filter(
    PointProgram == "D"
  )

TMPL_DesiredFields <- schemafilt$FieldName
TMPL_DesiredFields <- as.character(TMPL_DesiredFields)
TMPL_DesiredFields <- Trim(TMPL_DesiredFields)

##### Data conformation #####
##### Remove Unnecessary "GIS" Variables #####
indata <- indata[,grepl("gis", names(indata)) == F]

##### Correct "Commonly Misspelled" Variables #####
x <-which(colnames(indata) == "OccurrenceComments.1")
colnames(indata)[x] <- "OccurrenceComments"

x <-which(colnames(indata) == "IdentifiedBy.1")
colnames(indata)[x] <- "IdentifiedBy"

x <-which(colnames(indata) == "Area..m2.")
colnames(indata)[x] <- "Area"

x <-which(colnames(indata) == "IdentificationQualifier.1")
colnames(indata)[x] <- "IdentificationQualifier"

x <-which(colnames(indata) == "VernacularName.y")
colnames(indata)[x] <- "VernacularName"

x <-which(colnames(indata) == "Oxygen_mgl")
colnames(indata)[x] <- "Oxygen"

x <-which(colnames(indata) == "Survey.Data")
colnames(indata)[x] <- "Delete"

x <-which(colnames(indata) == "Observation.Data")
colnames(indata)[x] <- "Delete2"

x <-which(colnames(indata) == "Record.Keeping")
colnames(indata)[x] <- "Delete3"

x <-which(colnames(indata) == "VernacularName.x")
colnames(indata)[x] <- "Delete4"




x <-which(colnames(indata) == "IdentificationComments.1")
colnames(indata)[x] <- "IdentificationComments"

x <-which(colnames(indata) == "DatasetID.")
colnames(indata)[x] <- "DatasetID"

x <-which(colnames(indata) == "LatitudeInDD")
colnames(indata)[x] <- "Latitude"

x <-which(colnames(indata) == "LongitudeInDD")
colnames(indata)[x] <- "Longitude"

x <-which(colnames(indata) == "Website")
colnames(indata)[x] <- "WebSite"

x <-which(colnames(indata) == "IdentificationComment")
colnames(indata)[x] <- "IdentificationComments"

x <-which(colnames(indata) == "VernacularNameCategory.y")
colnames(indata)[x] <- "VernacularNameCategory"

x <-which(colnames(indata) == "PercentCover")
colnames(indata)[x] <- "Cover"

x <-which(colnames(indata) == "AphiaID.y")
colnames(indata)[x] <- "AphiaID"

x <-which(colnames(indata) == "TaxonRank.y")
colnames(indata)[x] <- "TaxonRank"

x <-which(colnames(indata) == "ObservationYear.s.")
colnames(indata)[x] <- "ObservationYear"

x <-which(colnames(indata) == "FilePath")
colnames(indata)[x] <- "ImageFilePath"

x <-which(colnames(indata) == "DepthinMeter")
colnames(indata)[x] <- "DepthInMeters"

x <-which(colnames(indata) == "OservationTime")
colnames(indata)[x] <- "ObservationTime"

x <-which(colnames(indata) == "DepthComments")
colnames(indata)[x] <- "DepthMethod"

x <-which(colnames(indata) == "SurveyEventID")
colnames(indata)[x] <- "EventID"

x <-which(colnames(indata) == "CruiseID")
colnames(indata)[x] <- "SurveyID"

x <-which(colnames(indata) == "CruiseComments")
colnames(indata)[x] <- "SurveyComments"

x <-which(colnames(indata) == "WormsID")
colnames(indata)[x] <- "AphiaID"

x <-which(colnames(indata) == "IdentificationRemarks")
colnames(indata)[x] <- "IdentificationComments"

x <-which(colnames(indata) == "LocationComment")
colnames(indata)[x] <- "LocationComments"

x <-which(colnames(indata) == "GenBankID")
colnames(indata)[x] <- "AssociatedSequences"

x <-which(colnames(indata) == "GenbankID")
colnames(indata)[x] <- "AssociatedSequences"

x <-which(colnames(indata) == "OccurrenceRemarks")
colnames(indata)[x] <- "OccurrenceComments"

x <-which(colnames(indata) == "taxonphylum")
colnames(indata)[x] <- "Phylum"

x <-which(colnames(indata) == "taxonclass")
colnames(indata)[x] <- "Class"

x <-which(colnames(indata) == "taxonsubclass")
colnames(indata)[x] <- "Subclass"

x <-which(colnames(indata) == "taxonorder")
colnames(indata)[x] <- "Order"

x <-which(colnames(indata) == "taxonsuborder")
colnames(indata)[x] <- "Suborder"

x <-which(colnames(indata) == "taxonfamily")
colnames(indata)[x] <- "Family"

x <-which(colnames(indata) == "taxonsubfamily")
colnames(indata)[x] <- "Subfamily"

x <-which(colnames(indata) == "taxongenus")
colnames(indata)[x] <- "Genus"

x <-which(colnames(indata) == "taxonsubgenus")
colnames(indata)[x] <- "Subgenus"

x <-which(colnames(indata) == "taxonspecies")
colnames(indata)[x] <- "Species"

x <-which(colnames(indata) == "taxonsubspecies")
colnames(indata)[x] <- "Subspecies"

x <-which(colnames(indata) == "pHScAle")
colnames(indata)[x] <- "pHscale"

x <-which(colnames(indata) == "SizeCategory")
colnames(indata)[x] <- "Size"

x <-which(colnames(indata) == "observationyear")
colnames(indata)[x] <- "ObservationYear"

x <-which(colnames(indata) == "Vernacular.Name")
colnames(indata)[x] <- "VernacularName"

x <-which(colnames(indata) == "ScientificName.Authorship")
colnames(indata)[x] <- "ScientificNameAuthorship"

x <-which(colnames(indata) == "ImageFile")
colnames(indata)[x] <- "ImageFilePath"

x <-which(colnames(indata) == "Categoricalabundance")
colnames(indata)[x] <- "CategoricalAbundance"

x <-which(colnames(indata) == "ta")
colnames(indata)[x] <- "TA"

x <-which(colnames(indata) == "dic")
colnames(indata)[x] <- "DIC"

x <-which(colnames(indata) == "startlatitude")
colnames(indata)[x] <- "StartLatitude"

x <-which(colnames(indata) == "startlongitude")
colnames(indata)[x] <- "StartLongitude"

x <-which(colnames(indata) == "endlatitude")
colnames(indata)[x] <- "EndLatitude"

x <-which(colnames(indata) == "endlongitude")
colnames(indata)[x] <- "EndLongitude"

x <-which(colnames(indata) == "observationyear")
colnames(indata)[x] <- "ObservationYear"

x <-which(colnames(indata) == "pHScale")
colnames(indata)[x] <- "pHscale"

x <-which(colnames(indata) == "TEMPLATEERROR")
colnames(indata)[x] <- "TEMPLATECORRECTION"

x <-which(colnames(indata) == "SamplingProtocol")
colnames(indata)[x] <- "SamplingEquipment"

x <-which(colnames(indata) == "DecimalLatitude")
colnames(indata)[x] <- "Latitude"

x <-which(colnames(indata) == "XYUncertainty")
colnames(indata)[x] <- "LocationAccuracy"

x <-which(colnames(indata) == "individualCount")
colnames(indata)[x] <- "IndividualCount"

x <-which(colnames(indata) == "OriginatorResourceURL")
colnames(indata)[x] <- "WebSite"

x <-which(colnames(indata) == "DecimalLongitude")
colnames(indata)[x] <- "Longitude"

x <-which(colnames(indata) == "LocationComment")
colnames(indata)[x] <- "LocationComments"

##### Check schema matching #####
setdiff(names(indata), TMPL_FullVariables)
setdiff(TMPL_FullVariables, names(indata))

setdiff(TMPL_RequiredFields, names(indata))
setdiff(names(indata),TMPL_RequiredFields)

setdiff(TMPL_DesiredFields, names(indata))
setdiff(names(indata),TMPL_DesiredFields)

##### Add Remaining Template Variables #####
namevector<-setdiff(TMPL_FullVariables, names(indata))
for(i in namevector)
  indata[,i] <- NA

##### Any interactive changes to infomration #####
# indata$OccurrenceComments <- paste("VARS_RecordType", indata$VARS, "Mega.Habitat",
#                                    indata$Mega.Habitat, "Habitat2", indata$Habitat2, sep = ": ")

##### Match Template Column Order #####
indata<-indata[,c(TMPL_FullVariables)]

##### Add DatasetID and Modified Date #####
indata$DatasetID <- substring(DatasetID, 12)
indata$Modified <- substring(DatasetID, 1, 8)

# assign a particular Modified date
indata$Modified <- "2017-04-06"

##### Check schema matching #####
setdiff(names(indata), TMPL_FullVariables)
setdiff(TMPL_FullVariables, names(indata))

##### Null Assignment #####

## Run TrimAll on Raw Data
### This function is designed to remove blank spaces.
lapply(TMPL_NumVariables, FUN = TrimAll)
lapply(TMPL_CharVariables, FUN = TrimAll)

## Apply "-999" nulls to Numeric Variables
lapply(TMPL_NumVariables, FUN = NumericNull)

## Apply "NA" null to Character Variables
lapply(TMPL_CharVariables, FUN = FactorNull)

##### Additional Custom changes that you may want to turn on or off #####

# set flags to zero or 1 for new dataset
#table(indata$Flag)
#table(indata$FlagReason)
if(indata$Flag == "-999") {
  indata$Flag <- "0"
}
#
# indata$FlagReason <- "Embargo per Peter Etnoyer"

# # test
# unique(subset(indata, Flag == "1" , select = c(Flag, FlagReason, CatalogNumber, ScientificName, DataProvider)))


# # test
# unique(indata$Flag)

# set DepthInMeters value equal to average of Min and Max depth
# summary(indata$DepthInMeters, useNA = "always")
# indata$DepthInMeters <- (indata$MaximumDepthInMeters + indata$MinimumDepthInMeters)/2
# indata$DepthComments <- "averaged maximum and minimum depth values"

##### Save output with automated naming #####
setwd("c:/rworking/digs/outdata")
#create a version iteration number
z<-as.character(as.numeric(substring(DatasetID, 10, 10))+1)

# Use the String function defined above to manipulate name to iterate version number.
x <- String(DatasetID)
x[10:10] <- z

save(indata, file = paste(x, ".R", sep = ""))
write.csv(indata, paste(x, ".csv", sep = ""), row.names = F, eol = "\r")
#write.table(indata, paste(x, ".txt", sep = ""), sep="\t", eol = "\r")

##### Alternative: Write custom name for output  #####
# setwd("c:/rworking/digs/outdata")
# write.csv(indata, "20160818-0_taxmatch_NOAA_OER_EX1504_2015_2015.csv", row.names = F, eol = "\r")


##### Checking #####
TMPL_RequiredFields

x <- schema %>%
  filter(FieldName == "DataContact")

factor(x$FieldDescription)
factor(x$ValidValues)

