##### Header #####
## Author: Robert P. McGuinn
## Date Started: 20201001
## Purpose: clean scratch work space for October of 2020. No sense can be made.
##    You should not try.
##### load packages #####
library(tidyverse)
library(knitr)
library(flextable)
library(sp)
library(rgdal)
library(maps)
library(extrafont)
library(googlesheets4)
library(leaflet)
library(RColorBrewer)
library(sf)
library(openxlsx)

##### load NDB #####
## method using 'read_csv' setting all columns to charactr and using specific encoding
setwd("C:/rworking/deepseatools/indata")
indata<-read_csv("DSCRTP_NatDB_20210414-0.csv",
                 col_types = cols(.default = "c"),
                 locale = locale(encoding = 'ISO-8859-1'),
                 na = c("-999", "NA"))

filt <- indata %>%
  filter(Flag == "0")

## clean
rm(indata)

##### checking #####
str(filt)

##### load an individual dataset #####
setwd("C:/rworking/deepseatools/indata")
## x is defined in the runner script
# or you could define it here like this
# x <- "20200701-0_NOAA_NEFSC_Connecticut_ISIS2_Towcam_Packer_2015_2015"
sub <- read.csv(paste(x,'.csv', sep = ''), header = T)
flagged <- sub %>%  filter(Flag == "1")

##### load schema #####
## download Google Sheet version of schema for use in R  documents
## Register and download Google Sheet using googlesheets4::read_sheet
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
## checking
s %>% filter(FieldName == 'RecordType') %>% pull(ValidValues)
s %>% filter(FieldName == 'LocationComments') %>% pull(FieldDescription)
s %>% filter(FieldName == 'IdentificationComments') %>% pull(FieldDescription)
##### checking some key filters #####
filt %>% filter(grepl("Nautilus", Vessel)) %>% pull(SurveyID) %>% unique()
filt %>% filter(grepl("Manta", Vessel)) %>% pull(SurveyID) %>% unique()
filt %>% filter(grepl("Okeanos", Vessel)) %>% pull(SurveyID) %>% unique()
filt %>% filter(grepl("EX1811", SurveyID)) %>% pull(DatasetID) %>% unique()
filt %>% filter(grepl("EX1903", SurveyID)) %>% pull(Reporter) %>% unique()
filt %>% filter(grepl("EX", AccessionID)) %>% pull(AccessionID) %>% unique()
filt %>% filter(grepl("EX1903", SurveyID)) %>% pull(Reporter) %>%  length()

sub %>% filter(VernacularNameCategory == 'check with dataprovider') %>%
  pull(AphiaID)

sub %>% filter(VernacularNameCategory == 'check with dataprovider') %>%
  pull(ScientificName) %>% unique()

sub %>% filter(is.na(IndividualCount) == T) %>% pull(IndividualCount)

sub %>% filter(grepl('Insufficient', FlagReason))%>%
  group_by(ScientificName, FlagReason, Flag) %>%
  summarize(n=n())

x <- sub %>% filter(grepl('DIVE', SampleID)) %>%
  group_by(SampleID) %>%
  summarize(n=n()) %>%
  arrange(desc(SampleID))
View(x)

##### accessionID Kaitlin Graff #####
indata %>% filter(AccessionID == 'NOAA_CBNMS_Nautilus_Graiff_2017_2017') %>%
  pull(CatalogNumber) %>%
  length()

indata %>% filter(AccessionID == 'NOAA_CBNMS_Nautilus_Graiff_2017_2017') %>%
  pull(SurveyID) %>%
  unique()

## write it out
setwd("C:/rworking/deepseatools/indata")
indata %>% filter(AccessionID == 'NOAA_CBNMS_Nautilus_Graiff_2017_2017') %>%
write_csv("20200710-2_NOAA_CBNMS_Nautilus_Graiff_2017_2017.csv")

filt %>% filter(grepl("Graiff", Reporter)) %>%
  pull(AccessionID) %>%
  unique()

filt %>% filter(grepl("YPM", DatasetID)) %>%
  pull(WebSite) %>% length() %>%
  unique()

##### summary for Tom Hourigan on Southeast data #####
z <- c("NOAA_SWFSC_Submersible", "NOAA_M2-10-06-L1-AUV", "NOAA_M2-10-06-L2-ROV", "NOAA_M2-10-06-L3-AUV", "NOAA_M2-10-06-L3-ROV", "NOAA_PU-11-08", "NOAA_PU-14-13")

setdiff(z, unique(filt$DatasetID))

summary <- filt %>% filter(DatasetID %in% z) %>%
  group_by(DatasetID) %>%
  summarize(number_eventID = length(unique(EventID)))
##### arvind #####
filt %>% filter(AccessionID == "NMNH_1868_2011") %>%
  group_by(AccessionID, Station) %>% summarize(foo=n()) %>% View()

##### QA #####
unique(filt$DatabaseVersion)
filt %>% filter(grepl("NOAA", DatasetID)) %>% pull(DatasetID) %>% unique()
sub %>% pull(AccessionID) %>% unique()
sub %>% pull(DataProvider) %>% unique()
filt %>% filter(grepl("Cordell", DataProvider)) %>% pull(DatasetID) %>% unique()
sub %>% group_by(ImageFilePath, SampleID) %>% summarize(n=n()) %>% head() %>% View()




##### best images thing #####
## see E. Gugliotti work https://drive.google.com/drive/folders/1LoSALOTqH3OZM6Aml8Hc_2E11fCDqcBH?usp=sharing #####
filt %>% filter(CatalogNumber == '530910') %>% pull(ImageURL) %>% browseURL()
##### NMNH dup export for Tom Hourigan #####
sub <- filt %>%
  filter(DatasetID == "NMNH_IZ") %>%
  group_by(SampleID) %>%
  filter(n()>1) %>%
  arrange(SampleID)

View(sub)
write.xlsx(sub, "20210521-0_NMNH_duplicates_RPMcGuinn.xlsx")

##### Dave_Packer_2014 #####
## 2014_NECSI_P3_GulfofMaine_ROV_Connecticut_Packer
filt %>% filter(grepl('Connecticut', Vessel)) %>%
  group_by(Vessel, SurveyID, PI, ObservationYear, Reporter, AccessionID) %>%
  summarize(n=n()) %>% View()

##### Query for Rachel Bassett #####
x <- filt %>% filter(
                Phylum == "Porifera",
                as.numeric(DepthInMeters) >= 50,
                as.numeric(DepthInMeters) <= 6500,
                ObservationDate >= 2000,
                ObservationDate <= 2021,
                LargeMarineEcosystem == "Gulf of Mexico"
                )



##### export to GIS #####
## load packages
library(arcgisbinding)
arc.check_product()

## make sure lat and log are numeric
x$Latitude <- as.numeric(x$Latitude)
x$Longitude <- as.numeric(x$Longitude)

## get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 | Longitude != -999)
# make copy to turn into spatial points data frame.
x_geo <- x

##create spatial points data from x
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class
fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)

##### Chris Rooper Data #####
filt %>% filter(ObservationYear == "2013") %>%
  group_by(DatasetID, SurveyID, ObservationYear, PI, Reporter) %>%
  summarize(n=n()) %>% View()

filt %>% filter(grepl("1507", SurveyID)) %>%
  group_by(DatasetID, SurveyID, ObservationYear, PI, Reporter) %>%
  summarize(n=n()) %>% View()

filt %>% filter(grepl("HAPC_", EventID)) %>%
  group_by(DatasetID, SurveyID, ObservationYear, PI, Reporter) %>%
  summarize(n=n()) %>% View()

filt %>% filter(grepl("Goddard", IdentifiedBy) |
                  grepl("Wilborn", IdentifiedBy) |
                  grepl("Rooper", IdentifiedBy) |
                  grepl("Goddard", Reporter) |
                  grepl("Wilborn", Reporter) |
                  grepl("Rooper", Reporter) |
                  grepl("Stone", PI) |
                  grepl("Rooper", PI)) %>%
  group_by(DatasetID, SurveyID, ObservationYear, PI, Reporter, IdentifiedBy) %>%
  summarize(n=n()) %>% View()


##### arvind #####
s1 <- 1
s2a <- 1
s2b <- 1
s2c <- 1
s3 <- 1
s4 <- 1
s5 <- 1
s6 <- 1

predictlist <- c(s1, s2a, s2b, s2c, s3, s4, s5, s6)
predictlist_with_list <- list(s1, s2a, s2b, s2c, s3, s4, s5, s6)
class(predictlist)

for (i in 1:length(predictlist)){

  sd_list <- raster::calc(clusterstacklist[[i]], sd) #stand deviation compuatation

  SEM_list <- sd_list[[i]]/sqrt(nlayers(clusterstacklist[[i]])) #Standard error of the mean computation

  SEMplus <- overlay(predictlist[[i]], SEM_list[[i]], fun=sum) #plus SEM

  SEMminus <- overlay(predictlist[[i]], SEM_list[[i]], fun=function(r1,r2){return(r1-r2)}) #minus SEM

}


primes_list <- list(2, 3, 5, 7, 11, 13)
class(primes_list)

# loop version 2
for (i in 1:length(primes_list)) {
  print(primes_list[[i]])
}


# loop version 1
for (p in primes_list) {
  print(p)
}


##### 2 #####
###Stack SDMs from clusters###

st1 <- subset(rs1, c(39, 15, 17, 26,5, 3))
st2a <- subset(rs1, c(35, 8, 25, 9, 20, 7, 32, 28, 33, 11, 37, 19, 21))
st2b <- subset(rs1, c(23,12,1))
st2c <- subset(rs1, c(29,2,34,22))
st3 <- subset(rs1, c(13,10))
st4 <- subset(rs1, c(38,6,18))
st5 <- subset(rs1, c(16,4,31,30,27))
st6 <- subset(rs1, c(36,14,24))


#create list of cluster stacks
clusterstacklist <- c(st1, st2a, st2b, st2c, st3, st4, st5, st6)

##Average over probabilities in Stacked SDMs

s1 <- mean(subset(rs1, c(39, 15, 17, 26,5, 3)))
s2a <- mean(subset(rs1, c(35, 8, 25, 9, 20, 7, 32, 28, 33, 11, 37, 19, 21)))
s2b <- mean(subset(rs1, c(23,12,1)))
s2c <- mean(subset(rs1, c(29,2,34,22)))
s3 <- mean(subset(rs1, c(13,10)))
s4 <- mean(subset(rs1, c(38,6,18)))
s5 <- mean(subset(rs1, c(16,4,31,30,27)))
s6 <- mean(subset(rs1, c(36,14,24)))

#create list of

predictlist <- c(s1, s2a, s2b, s2c, s3, s4, s5, s6)

for (i in 1:length(predictlist)){

  sd_list <- raster::calc(clusterstacklist[[1]], sd) #stand deviation compuatation

  SEM_list <- sd_list[[i]]/sqrt(nlayers(clusterstacklist[[i]])) #Standard error of the mean computation

  SEMplus <- overlay(predictlist[[i]], SEM_list[[i]], fun=sum) #plus SEM

  SEMminus <- overlay(predictlist[[i]], SEM_list[[i]], fun=function(r1,r2){return(r1-r2)}) #minus SEM

}


##### David Packer Data #####
