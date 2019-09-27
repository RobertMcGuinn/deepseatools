##### Installation/Loading of Packages #####
#install.packages("pacman")
# library(pacman)
# #pacman::p_load(captioner, bundesligR)
# library(captioner, bundesligR)
# #install.packages("beanplot")
# library(beanplot)
# #install.packages("stringr")
# library(stringr)
# install.packages("knitr")
# library(knitr)
# #install.packages("tidyr")
# library(tidyr)
#install.packages("sp")
library(sp)
# #install.packages("maptools")
# library(maptools)
# #install.packages("maps")
# library(maps)
# #install.packages("reshape")
# library(reshape)
# #install.packages("reshape2")
# library(reshape2)
# #install.packages("psych")
# library(psych)
# install.packages("ggplot2")
# library(ggplot2)
# #install.packages("data.table")
# library(data.table)
# install.packages("tidyverse")
library(tidyverse)
# #install.packages("car")
# library(car)
# #install.packages("gdata")
# library(gdata)
# #install.packages("digest")
# library(digest)
# #install.packages("rgdal")
# library(rgdal)
# #install.packages("ggmap")
# library(ggmap)
# install.packages("rerddap")
library(rerddap)
# #install.packages("raster")
# library(raster)
# #install.packages("rworldxtra")
# library(rworldxtra)
# #install.packages("ggrepel")
# library(ggrepel)
# #install.packages("xtable")
# library(xtable)
# library(taxize)
# library(rgdal)
# library(dplyr)
# #install.packages("tidyverse")
# library(tidyverse)
# install.packages('leaflet')
library(leaflet)
# install.packages('extrafont')
library(extrafont)
# install.packages('RColorBrewer')
library(RColorBrewer)
# install.packages('googlesheets')
library(googlesheets)
# install.packages('googledrive')
library(googledrive)
library(rmarkdown)
library(knitr)
#install.packages("maps")
library(maps)
#install.packages("rgdal")
library(rgdal)
#install('raster')
library(raster)
#install.packages("spocc")
library(spocc)
#install.packages('arcgisbinding')
library(arcgisbinding)
arc.check_product()
#install.packages('refinr')
library(refinr)
# install.packages('marmap')
library(marmap)
#install.packages('prettydoc')
library(prettydoc)

##### Bringing in input datasets #####
#from csv
#clear
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20181114-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")
#rm(indata)

# setwd("C:/rworking/digs/indata")

# path <- "C:/rworking/digs/indata/20180810_0_Q42018_Corrections_by_THourigan.csv"
# path2 <- "C:/rworking/digs/indata/20180824_0_Additional_SpongeTaxonomyCorrection_THourigan.csv"
#
# d <- read.csv(path, header = T)
# d1 <- read.csv(path2, header = T)
# d <- read.dbf("select.dbf", header = T)

# setwd("C:/rworking/deep-sea-workbench/InData/IntakePipe")
# d1<-read.csv("DSCRTP_NatDB_20151008-1.csv", header = T)
# d_cor <- read.csv("Pacific_IslandsMap20150814_9-26.csv", header = T)
#
# #from table
# d <- read.table("DSCRTP_NatDB_20150626-1.txt", header = T, sep = ",")
#
#from xls
# d <- read.xlsx("20180810_0_Q42018_Corrections_by_THourigan.xlsx", sheetName = "Sheet1")

##### Resolve a test set using taxize functionality #####
# create a species list from our data
library(taxize)
list <- unique(d$ScientificName)
list <- c("Flabellum alabastrum")
list <- list[grepl("cf.", list)]
x <- gnr_resolve(names = list)
match <- classification(list, db = "eol")
# checking a specific name

list <- c("Flabellum alabastrum")
x <- gnr_resolve(names = list)
table(x$matched_name, x$data_source_title)

gnr_resolve(names = c("Pseudopterogorgia anceps"))

tax <- read.csv("20170622-0_taxonomy_RPMcGuinn.csv", header = T)
taxch <- read.csv("20170622-0_taxa_to_change_RPMcGuinn.csv", header = T)
taxfl <- read.csv("20170622-0_taxa_to_flag_RPMcGuinn.csv", header = T)

##### test list against tax #####
list <- read.csv("list.csv", header = T)
list <- d$ScientificName
list <- setdiff(list, tax$ScientificName)
list

setdiff(list, tax$ScientificName)
setdiff(list, taxch$VerbatimScientificName)
setdiff(list, taxfl$ScientificName)

classification(list, db = "eol")
classification(list, db = "ncbi")
classification(list, db = "col")
classification(list, db = "gbif")
classification(list, db = "tropicos")
classification(list, db = "gbif")
classification(list, db = "nbn")
classification(list, db = "worms")
classification(list, db = "natserv")

classification(c("Pseudopterogorgia anceps"), db = "worms")

help("classification")

# see a list of datbases currently being used
gnr_datasources()

# test

# checking
table(factor(d[d$ScientificName == "Siderastreas sp.", c("Flag")]))
d[d$ScientificName == "Swiftia torreyi", c("ScientificName", "Locality", "DataProvider")]
head(temp)
str(temp)
fix(temp)
names(temp)

x<-table(factor(temp[grepl("cf.", temp$submitted_name) , c("matched_name")]))
x<-temp[grepl("cf.", temp$submitted_name) & temp$data_source_title == "WoRMS", c("submitted_name", "matched_name", "data_source_title")]

fix(x)

class(x)
str(x)

setwd("C:/rworking/digs/outdata")
write.csv(temp,"20160419_Taxize_all_Matches_from_DSCRTP_NatDB_20160323-1.csv", row.names = F, quote = T)

# create a species list
list <- head(unique(d$ScientificName))

#create an object
class <- classification(list, db = 'gbif')

#convert class to dataframe
df <- data.frame(matrix(unlist(class), nrow=6, byrow=T),stringsAsFactors=FALSE)
str(class)
str(df)
df

# using the Taxosaurus names source DOES NOT WORK WELL!!
list <- list <- head(unique(d$ScientificName))
tnrs(query = list, source = "gbif")[ , -c(5:7)]

##### Subsetting according to geography #####

#Gulf of Mexico
indatafilter <- filter(indata, as.numeric(Latitude) > 18.1509,
                   as.numeric(Latitude) < 30.40613,
                   as.numeric(Longitude) < -80.45932,
                   as.numeric(Longitude) > -97.858208,
                   Flag == "0",
                   TaxonRank == "species" |
                   TaxonRank== "subspecies")
#&
#                  ScientificName == "Lophelia pertusa")

#bigger bounding box around South Atlanti and PR.
subset <- subset(d, as.numeric(Latitude) > 15.00000 &
                   as.numeric(Latitude) < 37.00000 &
                   as.numeric(Longitude) < -64.0000 &
                   as.numeric(Longitude) > -98.00000 &
                   Flag == "0")


setwd("C:/rworking/digs/outdata")
write.csv(indatafilter,"DSCRTP_NatDB_20160728-0_GOMEX_bounding_box.csv", row.names = F, quote = T)

##### Setting various to various filters for summary of QA datasets #####

subset <- d2
  #filter(as.numeric(IndividualCount) > 200)  %>%
  #filter(DataProvider == "" )
detach("package:plyr", unload=TRUE)

x <- d %>%
  filter(Order == "Pennatulacea")  %>%
  group_by(DataProvider, Locality, Family, ScientificName) %>%
  summarize(n_records = n(),
            minyear = min(as.numeric(ObservationYear)),
            maxyear = max(as.numeric(ObservationYear))
            )

fix(x)
setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"Summary_Table_AK_Sea_Pens_2015.csv", row.names = F, quote = T)


write.csv(d,"tanner_cortes_gislocality.csv", row.names = F, quote = T)






fix(x)
sum(x$n)

table(factor(x$Genus), useNA = "always")
table(factor(x$DataContact), useNA = "always")
table(factor(x$Repository), useNA = "always")
table(factor(x$Repository), useNA = "always")
#fix(x)

pi_tbl <-
  subset %>%
  group_by(PI) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
pi_tbl


cit_tbl <-
  subset %>%
  group_by(Citation) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
cit_tbl

genus_tbl <-
  subset %>%
  group_by(Genus) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
genus_tbl

repo_tbl <-
  subset %>%
  group_by(Repository) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
repo_tbl

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(genus_tbl, "genus_table_Gulf.csv")
write.csv(cit_tbl, "Citation_JHS.csv")
write.csv(order_tbl, "Order_table_JHS.csv")


subset$ObservationYear[subset$ObservationYear == -999] <- NA
summary(as.numeric(subset$ObservationYear), na.rm = T)

fix(subset)

length(subset$DataProvider)
sum(x$n)
sum(x$n_coral)
sum(x$n_sponges)

write.csv(x,"DSCRTP_20150626-1_DatasetID_nrows3785.csv", row.names = F, quote = T)

##### This is a way to split a dataset up into its constituent parts #####

lst1 <- split(x, with(x, interaction(Order,Locality)), drop = TRUE)
lapply(seq_along(lst1),function(i) write.csv(lst1[[i]],file=paste0(names(lst1[i]),".csv"),row.names=FALSE))

##### Exporting taxonomy table #####
x <- d %>%
  #filter(as.numeric(Flag) == 0)  %>%
  group_by(VernacularNameCategory, ScientificName, TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family,
           Subfamily, Genus, Subgenus, Species, Subspecies) %>%
  summarize(n = n())

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"20160818-0_unique_taxonomy_table_for_GOMEX_within_EEZ_from_DSCRTP_NatDB_20160728-0_.csv", row.names = F, quote = T)


##### Exporting dataset metadata #####
x <- filt %>%
  filter(grepl("Ron Brown", Vessel))  %>%
  #select(DataProvider, Repository, PI, DataContact, FlagReason)
  group_by(DataProvider, DatasetID, DataContact, Reporter, FlagReason, CatalogNumber, ImageURL) %>%
  summarize(n = n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            p_flagged = flagged/n,
            n_coral = length(Phylum[Phylum == "Cnidaria"]),
            n_sponges = length(Phylum[Phylum == "Porifera"]),
            start = min(as.numeric(ObservationYear[ObservationYear != -999])),
            end = max(as.numeric(ObservationYear[ObservationYear != -999])),
            n_orders = length(unique(Order)),
            imagefilepath = length(ImageFilePath[is.na(ImageFilePath) == F]),
            imageurl = length(ImageURL[is.na(ImageURL) == F]))
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"DSCRTP_NatDB_20160323_registry.csv", row.names = F, quote = T)

##### Taxonomic Table #####
x <- indata %>%
  filter(as.numeric(Flag) == 0, FishCouncilRegion)  %>%
  group_by(VernacularNameCategory, ScientificName, TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family,
           Subfamily, Genus, Subgenus, Species, Subspecies) %>%
  summarize(n = n())

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"Unique_taxonomy_table_for_GOMEX_from_DSCRTP_NatDB_20150714-0_.csv", row.names = F, quote = T)

##### Exporting dataset metadata basic taxonomy and depth distribution #####
x <- d %>%
filter(DataProvider == "NOAA, Alaska Fisheries Science Center" & Flag != "1")  %>%
    group_by(FlagReason,Order, ScientificName) %>%
    summarize(n = n(),
              Min_Depth = min(as.numeric(DepthInMeters[DepthInMeters != -999])),
              Max_Depth = max(as.numeric(DepthInMeters[DepthInMeters != -999])),
              Min_Lat = min(as.numeric(Latitude[Latitude != -999])),
              Max_Lat = max(as.numeric(Latitude[Latitude != -999])),
              Min_Long = min(as.numeric(Longitude[Longitude != -999])),
              Max_Long = max(as.numeric(Longitude[Longitude != -999])),
              noflag = length(Flag[Flag == "0"]),
              flagged = length(Flag[Flag == "1"]))

plot(a,b, cex = 0.5)
hpts <- chull(a,b)
hpts <- c(hpts, hpts[1])
lines(hpts, )


setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"datasetID_NatDB_20150807_tax_summary_AFSF.csv", row.names = F, quote = T)

##### Testing unique values #####
unique(factor(d_cor$DataProvider))
unique(factor(d_cor$DataProvider))

##### Looking at problems in maps #####
detach("package:plyr", unload=TRUE)
x <- d %>%
  filter(as.numeric(Latitude) < 50)  %>%
  group_by(CatalogNumber, SurveyID, EventID, DataProvider, SurveyID, ObservationYear, Latitude, Longitude, DepthInMeters, ScientificName) %>%
  summarise(n=n())

x <- subset(x, ScientificName == "Madracis brueggemanni")

setwd("C:/rworking/digs/OutData")
write.csv(d3,"longline.csv", row.names = F, quote = T)

##### Extract a GIS selection from dataset #####
setwd("C:/rworking/deep-sea-workbench/OutData")
bs_np5 <- read.csv("bs_np5.csv")
sel<-read.csv("data.csv")
data<-merge(bs_np5, sel)
write.csv(data, "data.csv")

x<- subset(d, CatalogNumber == "250111")

setwd("C:/rworking/digs/outdata/")
x<-read.csv("20151102-2_NOAA_OER_Alvin_AKSeamounts_2002_2002.csv")

x<-subset(x, Genus == "Bathypathes")
summary(x$DepthInMeters)

plot(x$Genus, x$DepthInMeters)+
  theme(axis.text.x=element_text(size=15,angle=90,hjust=1,vjust=0.05),
        axis.text.y=element_text(size=15)) +
  scale_y_continuous(limits = c(200, 800))


qplot(factor(Genus),as.numeric(DepthInMeters), data=x, geom=c("boxplot"),alpha=I(1),
           xlab="Genus", ylab="Depth (meters)") +
  theme(axis.text.x=element_text(size=15,angle=90,hjust=1,vjust=0.05),
        axis.text.y=element_text(size=15))

hist(as.Date(x$ObservationDate), freq = T, breaks = 30)


##### Working with the taxonomy table#####
subset(d, ScientificName == "Flabellum alabastrum", select = c(ScientificName, VernacularNameCategory, TaxonRank, Flag))

#make a list of items in the global dataset that need correction in the VernacularNameCategory issue
c <- unique(subset(d, ScientificName == "Aphrocallistidae", select = c(Phylum, ScientificName, TaxonRank, VernacularNameCategory, Flag)))
setwd("C:/rworking/digs/outdata")
write.csv(c,"DSCRTP_NatDB_20151008-2_Missing_VernacularNameCategories.csv", row.names = F, quote = T)

 o##### Mapping FishCouncilRegion #####
x <- unique(subset(d, Flag == 0, select = c(Latitude, Longitude, FishCouncilRegion)))
setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### Looking at records with no AphiaID for Scientific Name #####
x <- unique(subset(indata, is.na(AphiaID) == T, select = c(ScientificName)))
setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### Looking at flags in indata #####
sort(unique(indata$Flag))

##### Inspecting Variable Names #####
sort(unique(d$DatasetID))
table(factor(indata[grepl("Scotia", indata$Vessel), c("DataProvider", "EventID", "DataContact")]))
table(factor(filt[grepl("Okeanos", filt$Vessel), c("Vessel")]))
table(factor(indata[grepl("NOAA, Gulf of Farallones National Marine Sanctuary", indata$DataProvider), c("DataProvider")]))




sort(unique(names(d)))

indata[grepl("NOAA, Gulf of Farallones National Marine Sanctuary", indata$DataProvider), c("ImageFilePath", "ImageURL")]


##### Good Scraps #####
##### get rid of the additions "*.x" variables after a join #####
join <-join[, grep("\\.x$", names(join))]

##### find out which AphiaID's did not match.#####
table(join$AphiaID.y, useNA = "always")
unique(subset(join, is.na(AphiaID.y) == T, select = c(ScientificName)))

subset(tax, ScientificName == "Pennatula")

##### Duplicate testing#####
#gives you a data frame with a list of ids and the number of times they occurred.
n_occur <- data.frame(table(tax$ScientificName))
n_occur

#####tells you which ids occurred more than once.
n_occur[n_occur$Freq > 1,]

#####returns the records with more than one occurrence.
tax[tax$ScientificName %in% n_occur$Var1[n_occur$Freq > 1],]

##### fixing spefic issue in TCWC at specific Species names in TCWC set #####
indata[indata$ScientificName == "Antipathes abietina", ]$ScientificName <- "Elatopathes abietina"
indata[indata$ScientificName == "Elatopathes abietina", ]$Synonyms <- "Antipathes abietina"

indata[indata$ScientificName == "Antipathes pennacea" & is.na(indata$ScientificName) == F, ]$ScientificName <- "Plumapathes pennacea"
indata[indata$ScientificName == "Plumapathes pennacea"& is.na(indata$ScientificName) == F, ]$Synonyms <- "Antipathes pennacea"

indata[indata$ScientificName == "Asterosmilia prolifera" & is.na(indata$ScientificName) == F,  ]$ScientificName <- "Anomocora prolifera"
indata[indata$ScientificName == "Anomocora prolifera"& is.na(indata$ScientificName) == F,  ]$Synonyms <- "Asterosmilia prolifera"

##### Looking at vessels #####
unique(indata[grepl("1102", indata$SurveyID), c("Vessel", "SurveyID", "DataProvider")])
unique(d[grepl("Velero", d$Vessel), c("Vessel")])
unique(indata$Vessel)
head(indata$Vessel)
table(unique(indata$NavType))
unique(indata$DataProvider)



##### Getting rid of newline characters and read table tricks #####
setwd("c:/rworking/digs/indata")
x<-read.table("test.txt",sep="\t",allowEscapes=T, header = T)
write.table(x, "test.txt", sep="\t", eol = "\r", col.names = T, row.names = F)

##### Exploring fields #####
unique(d$Country)
table(d$IdentificationQualifier)
head(table(unique(d$IdentificationQualifier)))

unique(d[d$Flag == "0",]$Country)
unique(d1[d1$Flag == "0",]$Country)


length(d[d$Flag == "0",]$Country)
length(d1[d1$Flag == "0",]$Country)


length(d[d$Flag == "1",]$Country)

##### Looking at datasets that are repeated #####
x <- d %>%
  filter(grepl("Curt.Whitmire", d$DataContact) | grepl("Curt Whitmire", d$DataContact))  %>%
  group_by(Flag, FlagReason, DataProvider, Purpose, SurveyID, DataContact) %>%
  summarise(n=n())

setwd("C:/rworking/digs/OutData")
write.csv(x,"x.csv", row.names = F, quote = T)

##### Comparing growth and changes in quarterly updates #####
length(d$Flag)
length(d1$Flag)

# > length(d$Flag)
# [1] 356710
# > length(d1$Flag)
# [1] 332344

#to look at only unflagged records
d<-d[d$Flag == "0",]
d1<-d1[d1$Flag == "0",]

# looking at increase in unflagged records
length(d$Flag)- length(d1$Flag)

# number of IndividualCount
sum(as.numeric(d$IndividualCount[d$IndividualCount != -999])) - sum(as.numeric(d1$IndividualCount[d1$IndividualCount != -999]))

# number of new species
length(unique(d$ScientificName))-length(unique(d1$ScientificName))

# number of new images
(length(d$ImageURL) - length(d$ImageURL[d$ImageURL == "NA"])) - (length(d1$ImageURL) - length(d1$ImageURL[d1$ImageURL == "NA"]))

##### Summary By DataProvider and DatasetID

x <- indata %>%
  filter(DatasetID == 'BOEM_RB-07-04') %>%
  group_by(DatasetID, PI, DataContact, DataProvider, Locality, Vessel, VehicleName, ObservationYear, Citation, Repository) %>%
  summarize(n_records = n(),
            flagsum = length(Flag[Flag == "1"]),
            minyear = min(as.numeric(ObservationYear)),
            maxyear = max(as.numeric(ObservationYear)),
            latitude = mean(Latitude),
            longitude = mean(Longitude)
  )

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### GREPL Inspecting IdentificationComments Names #####
sort(unique(d$IdentifiedBy))

unique(d[grepl("143, 2010-01, 162", d$IdentificationComments), c("SampleID", "IdentifiedBy", "IdentificationQualifier")])
unique(d[grepl("100316302", d$SampleID), c("SampleID", "IdentifiedBy", "IdentificationQualifier")])

##### Wildcard searching on multiple fields to return specific data - base method with grepl #####

x <-
unique(
d[grepl("", d$SurveyID) &
           grepl("Phantom S", d$VehicleName) &
           grepl("", d$RecordType),
         c("DataProvider", "IdentifiedBy", "RecordType", "VehicleName")]
)

table(factor(x$DataProvider))
table(factor(x$VehicleName))


d[is.na(d$DataProvider) == T & d$Flag == 1, c("FlagReason","DataContact")]

##### Subsetting data via merge function using a list of SampleID or CatalogNumbers #####

#Alternative 1 - Using an outside list of ID's to match
setwd("C:/rworking/digs/indata")
x <-read.csv("sampleid.csv", header = T)
x


#Alternative 2 - Creating a subset list from within the dataset.
x<- head(unique(d$SampleID))
x<-data.frame(x)
x$SampleID <- x$x
x
x <- x[c(2)]
x

join <- merge(x, d, by.x = "SampleID", by.y = "SampleID", all.x = F)

join[,c("SampleID", "DataProvider", "ScientificName", "LargeMarineEcosystem")]


x

##### Looking at DataProvider #####
x <-
  unique(
    d[grepl("Monterey", d$DataProvider), #&
        #grepl("Center", d$DataProvider), #&
        #grepl("", d$RecordType),
      #c("DataProvider")
      ]
  )
x

# get output as a csv
setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### Looking at Vessel #####
x <-
  unique(
    d[grepl("Walton", d$Vessel),
        #grepl("Center", d$Vessel), #&
      #grepl("", d$RecordType),
      c("Vessel")]
  )
x

##### Exploring Oxygen #####
ox <- d %>%
  filter(DatasetID == "NOAA_CCEHBR_1952_2012",
         Flag == "0"#,
         #Oxygen != "-999"
         )  %>%
  group_by(CatalogNumber, SampleID, DatasetID, ScientificName, DataProvider, Repository, Citation, DataContact, Reporter, PI, Vessel, SurveyID, Latitude, Longitude, DepthInMeters) %>%
  summarize(n_records = n(),
            MinYear = min(as.numeric(ObservationYear)),
            MaxYear = max(as.numeric(ObservationYear)),
            MinOxygen = min(as.numeric(Oxygen)),
            MaxOxygen = max(as.numeric(Oxygen))
#             MinSalinity = min(as.numeric(Salinity)),
#             MaxSalinity = max(as.numeric(Salinity)),
#             Min_pH = min(as.numeric(pH)),
#             # Max_pH = max(as.numeric(pH))

  )


# check
fix(ox)

#create output
setwd("C:/rworking/digs/outdata")
write.csv(ox,"20160414-1_NOAA_CCEHBR_1952_2012_from_DSCRTP_NatDB_20160323-1.csv", row.names = F, quote = T)

##### Transformation of Oxygen Values (NOAA_SWFSC_Stierhoff_2003-2011) #####

# Getting the subset of interest
indata <- d %>%
  filter(DatasetID == "NOAA_SWFSC_Stierhoff_2003-2011", Oxygen != "-999", Flag == "0") %>%
  select(CatalogNumber, Oxygen)

# Plot the results
plot(indata$Oxygen, indata$DepthInMeters)

# Converting from micromols/L to mL/L
indata$Oxygen <- indata$Oxygen * 0.0224

setwd("C:/rworking/digs/outdata")
write.csv(indata,"20160414-1_Corrected_Oxygen_NOAA_SWFSC_Stierhoff_2003-2011.csv", row.names = F, quote = T)

##### Transformation of Oxygen Values (from HURL_LineIslands_2005_2005) #####

# Getting the subset of interest
indata <- d %>%
  filter(DatasetID == "HURL_LineIslands_2005_2005", Oxygen != "-999", Flag == "0") %>%
  select(CatalogNumber, Oxygen, DepthInMeters, SurveyID)

# Plot the results
library(ggplot2)
qplot(DepthInMeters, Oxygen, data = indata, colour = SurveyID)
#qplot(DepthInMeters, Oxygen, data = indata,  alpha = I(1/50))

# Converting from mg/L to mL/L
indata$Oxygen <- indata$Oxygen * 0.699301

# Plot the results
library(ggplot2)
qplot(DepthInMeters, Oxygen, data = indata, colour = SurveyID)
#qplot(DepthInMeters, Oxygen, data = indata,  alpha = I(1/50))


setwd("C:/rworking/digs/outdata")
write.csv(indata,"20160414-1_Corrected_Oxygen_HURL_LineIslands_2005_2005.csv", row.names = F, quote = T)

##### Transformation of Oxygen Values (from NOAA_OER_EX1402L3_2014_2014) #####

# Getting the subset of interest
indata <- d %>%
  filter(DatasetID == "NOAA_OER_EX1402L3_2014_2014", Oxygen != "-999", Flag == "0") %>%
  select(CatalogNumber, Oxygen, DepthInMeters, SurveyID)

# Plot the results
library(ggplot2)
qplot(SurveyID, Oxygen, data = indata, colour = SurveyID)
#qplot(DepthInMeters, Oxygen, data = indata,  alpha = I(1/50))

# Converting from mg/L to mL/L
indata$Oxygen <- indata$Oxygen * 0.699301

# Plot the results
library(ggplot2)
qplot(DepthInMeters, Oxygen, data = indata, colour = SurveyID)
#qplot(DepthInMeters, Oxygen, data = indata,  alpha = I(1/50))

setwd("C:/rworking/digs/outdata")
write.csv(indata,"20160414-1_Corrected_Oxygen_NOAA_OER_EX1402L3_2014_2014.csv", row.names = F, quote = T)

##### Exploring pH #####
ph <- d %>%
  filter(Flag == "0", pH != "-999")  %>%
  group_by(DataProvider, DatasetID, DataContact) %>%
  summarize(n_records = n(),
            MinYear = min(as.numeric(ObservationYear)),
            MaxYear = max(as.numeric(ObservationYear)),
#             MinOxygen = min(as.numeric(Oxygen)),
#             MaxOxygen = max(as.numeric(Oxygen)),
#             MinSalinity = min(as.numeric(Salinity)),
#             MaxSalinity = max(as.numeric(Salinity)),
            Min_pH = min(as.numeric(pH)),
            Max_pH = max(as.numeric(pH))

  )

setwd("C:/rworking/digs/outdata")
write.csv(ph,"ph.csv", row.names = F, quote = T)

##### Exploring Salinity #####
sa <- d %>%
  filter(Flag == "0",
         FishCouncilRegion == "Pacific",
         DatasetID != "MBARI_1989_2012",
         Salinity != "-999"#,
         #(as.numeric(Salinity) >= 0  & as.numeric(Salinity) < 50)
         )  %>%
  group_by(DataProvider, DatasetID, Vessel, SurveyID, DataContact, FishCouncilRegion, DepthInMeters, Salinity) %>%
  summarize(n_records = n(),
            MinYear = min(as.numeric(ObservationYear)),
            MaxYear = max(as.numeric(ObservationYear)),
#             MinOxygen = min(as.numeric(Oxygen)),
#             MaxOxygen = max(as.numeric(Oxygen)),
            MinSalinity = min(as.numeric(Salinity)),
            MaxSalinity = max(as.numeric(Salinity))
#             Min_pH = min(as.numeric(pH)),
#             Max_pH = max(as.numeric(pH))

  )

# plots

qplot(DepthInMeters, Salinity, data = sa, color = DatasetID, size=I(3))
ggplot(sa, aes(x=DataProvider, y=Salinity, fill=DatasetID)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

setwd("C:/rworking/digs/outdata")
write.csv(sa,"sa.csv", row.names = F, quote = T)

##### Merging from DataProvider correction  #####

setwd("C:/rworking/digs/indata")
d<-read.csv("20151102-9_NOAA_AFSC_CentralAleutian_JasonII_2004.csv", header = T)
s1<-read.csv("20151102-8_NOAA_AFSC_CentralAleutian_JasonII_2004_Corrected_Bob_Stone_20160314_sheet1.csv", header = T)
s2<-read.csv("20151102-8_NOAA_AFSC_CentralAleutian_JasonII_2004_Corrected_Bob_Stone_20160314_outliers.csv", header = T)
d2<-read.csv("20151102-8_NOAA_AFSC_CentralAleutian_JasonII_2004.csv", header = T)

# Change some incorrect
x <-which(colnames(s1) == "Catalog..")
colnames(s1)[x] <- "CatalogNumber"

x <-which(colnames(s2) == "Catalog..")
colnames(s2)[x] <- "CatalogNumber"

## Inspecting
# make a table of CatalogNumbers
n_occur <- data.frame(table(s2$CatalogNumber))

# return those CatalogNumbers with frequencies greater than one
n_occur[n_occur$Freq > 1,]

# merge
join <- merge(d, s2, by = "CatalogNumber", all.x = F, all.y = F)
# names(join)
# str(join)

## replace old values with new values
join$IdentifiedBy.x <- join$IdentifiedBy.y
join$ScientificName.x <- join$ScientificName.y
join$IdentifiedBy.x <- join$IdentifiedBy.y
join$IndividualCount <- join$Count
join$ObservationDate.x <- join$ObservationDate.y
join$Longitude.x <-  join$Longitude.y
join$ImageFilePath.x <-  join$ImageFilePath.y
join$Suborder.x <- join$Suborder.y
join$IdentificationDate.x <- join$IdentificationDate.y
join$IdentificationQualifier.x <-join$IdentificationQualifier.y
join$Condition.x <- join$Condition.y
join$Substrate.x <- join$Substrate.y
join$ObservationTime.x <- join$ObservationTime.y
join$DepthInMeters <- join$Depth
join$Latitude.x <- join$Latitude.y
join$EventID.x <- join$EventID.y

# strip all columns that contain ".y"
join <- join[,-grep("\\.y", colnames(join))]

# strip specific columns
drops <- c("Depth","Count","X", "X.1" )
join <- join[ , !(names(join) %in% drops)]
# # Checking
# names(join)
# length(join$CatalogNumber)
# length(s2$CatalogNumber)
# length(join$CatalogNumber)
# table(join$CatalogNumber[join$CatalogNumber == "-999"])

## Change names of updated variables with the (.x) ending back to the correct name

# Return a list of names wher column names end in .x

# # Checking
# names(join[,grepl("\\.x", colnames(join))])

#Change the corrected .x names back to the schema matching variable name
names(join)[names(join)=="ScientificName.x"] <- "ScientificName"
names(join)[names(join)=="IdentifiedBy.x"] <- "IdentifiedBy"
names(join)[names(join)=="IdentificationDate.x"] <- "IdentificationDate"
names(join)[names(join)=="IdentificationQualifier.x"] <- "IdentificationQualifier"
names(join)[names(join)=="Substrate.x"] <- "Substrate"
names(join)[names(join)=="ObservationTime.x"] <- "ObservationTime"
names(join)[names(join)=="Longitude.x"] <- "Longitude"
names(join)[names(join)=="ImageFilePath.x"] <- "ImageFilePath"
names(join)[names(join)=="Suborder.x"] <- "Suborder"
names(join)[names(join)=="Condition.x"] <- "Condition"
names(join)[names(join)=="ObservationDate.x"] <- "ObservationDate"
names(join)[names(join)=="Latitude.x"] <- "Latitude"
names(join)[names(join)=="EventID.x"] <- "EventID"

# checking with output
# fix(join)
# setdiff(names(d), names(join))
# setwd("C:/rworking/digs/outdata")
# write.csv(join,"x.csv", row.names = F, quote = T)

# Use the "join" table just created.  It is an updated version of
# corrected values from S2 joined with all of the other information in d.
# Create dsub, a subset of d containing only CatalogNumbers not in join
list <- setdiff(d$CatalogNumber, join$CatalogNumber)
dsub <- d[d$CatalogNumber %in% list, ]

# dsub corrected with changed values from S1
join2 <- merge(dsub, s1, by = "CatalogNumber", all.x = F, all.y = F)

# Checking
length(join2$CatalogNumber)
length(s1$CatalogNumber)-length(join2$CatalogNumber)

#replace old values with new values
join2$IdentifiedBy.x <- join2$IdentifiedBy.y
join2$ScientificName.x <- join2$ScientificName.y
join2$IdentifiedBy.x <- join2$IdentifiedBy.y
join2$IndividualCount <- join2$Count
join2$ObservationDate.x <- join2$ObservationDate.y
join2$Longitude.x <-  join2$Longitude.y
join2$ImageFilePath.x <-  join2$ImageFilePath.y
join2$Suborder.x <- join2$Suborder.y
join2$IdentificationDate.x <- join2$IdentificationDate.y
join2$IdentificationQualifier.x <-join2$IdentificationQualifier.y
join2$Condition.x <- join2$Condition.y
join2$Substrate.x <- join2$Substrate.y
join2$ObservationTime.x <- join2$ObservationTime.y
join2$DepthInMeters <- join2$Depth
join2$Latitude.x <- join2$Latitude.y
join2$EventID.x <- join2$EventID.y

# strip all columns that contain ".y"
join2 <- join2[,-grep("\\.y", colnames(join2))]

# strip specific columns
drops <- c("Depth","Count","X", "X.1" )
join2 <- join2[ , !(names(join2) %in% drops)]

# # Checking
# names(join2)
# length(join2$CatalogNumber)
# length(s2$CatalogNumber)
# length(join2$CatalogNumber)
# table(join2$CatalogNumber[join2$CatalogNumber == "-999"])

## Change names of updated variables with the (.x) ending back to the correct name

# Return a list of names wher column names end in .x

# # Checking
# names(join2[,grepl("\\.x", colnames(join2))])

##### #Change the corrected .x names back to the schema matching variable name
names(join2)[names(join2)=="ScientificName.x"] <- "ScientificName"
names(join2)[names(join2)=="IdentifiedBy.x"] <- "IdentifiedBy"
names(join2)[names(join2)=="IdentificationDate.x"] <- "IdentificationDate"
names(join2)[names(join2)=="IdentificationQualifier.x"] <- "IdentificationQualifier"
names(join2)[names(join2)=="Substrate.x"] <- "Substrate"
names(join2)[names(join2)=="ObservationTime.x"] <- "ObservationTime"
names(join2)[names(join2)=="Longitude.x"] <- "Longitude"
names(join2)[names(join2)=="ImageFilePath.x"] <- "ImageFilePath"
names(join2)[names(join2)=="Suborder.x"] <- "Suborder"
names(join2)[names(join2)=="Condition.x"] <- "Condition"
names(join2)[names(join2)=="ObservationDate.x"] <- "ObservationDate"
names(join2)[names(join2)=="Latitude.x"] <- "Latitude"
names(join2)[names(join2)=="EventID.x"] <- "EventID"

# # checking with output
# fix(join)
# setdiff(names(d), names(join2))
# setwd("C:/rworking/digs/outdata")
# write.csv(join,"x.csv", row.names = F, quote = T)

# # Checking
# names(join)
# names(join2)

## join and join2 should be all the corrected values, now they just need rbind
fixed <- rbind(join, join2)

# #Checking
# names(fixed)
# R
# length (d$CatalogNumber)

# fixed[fixed$CatalogNumber == "552029", c("DepthInMeters")]
# d[d$CatalogNumber == "552029", c("DepthInMeters")]
# d2[d2$CatalogNumber == "552029", c("DepthInMeters")]
#
# setwd("C:/rworking/digs/outdata")
# write.csv(fixed,"x.csv", row.names = F, quote = T)

## Isolate the new records from Bob from S2
nocat <- s2[s2$CatalogNumber == "-999", ]

# # Checking
# names(nocat)
# length(nocat$CatalogNumber)

x <-which(colnames(nocat) == "Depth")
colnames(nocat)[x] <- "DepthInMeters"

x <-which(colnames(nocat) == "Count")
colnames(nocat)[x] <- "IndividualCount"

indata<-nocat
# do a bunch of stuff here using new data patch scripts

final <- rbind(fixed, indata)

length(final$CatalogNumber)
length(d$CatalogNumber)

# write output
setwd("C:/rworking/digs/outdata")
write.csv(final,"20160316-0_NOAA_AFSC_CentralAleutian_JasonII_2004.csv", row.names = F, quote = T)

##### Exporting data from DataProvider #####

##### Exporting dataset metadata basic taxonomy and depth distribution #####

# Bringing in input data
setwd("C:/rworking/digs/indata")
d<-read.csv("DSCRTP_NatDB_20160226-0.csv", header = T)

# Checking
unique(d$DataProvider)

# Filtering dataset

x <- d %>%
  filter(Flag != "1", DataProvider == "Monterey Bay Aquarium Research Institute")

setwd("C:/rworking/deep-sea-workbench/OutData")
write.csv(x,"DSCRTP_NDB_v20160226_MBARI_subset.csv", row.names = F, quote = T)

##### joining new haulid information #####
# Bringing in input data
setwd("C:/rworking/digs/indata")
d<-read.csv("20160223-9_NOAA_AFSC_EBS_Canyons_2014_2014.csv", header = T)
d1<-read.csv("HaulDeploymentKey.csv", header = T)
join <- merge(d, d1, by.x = "EventID", by.y = "Deployment_ID", all.x = F)
join$TrackingID <- join$EventID
join$EventID <- join$Haul


setwd("C:/rworking/digs/outdata")
write.csv(join,"20160223-9_NOAA_AFSC_EBS_Canyons_2014_2014.csv", row.names = F, quote = T)

##### Selecting DatasetID and looking at Citation #####

table(factor(d[grepl("SWFSC", d$DatasetID), c("DatasetID")]))

x <- indata %>%
  filter(grepl("Flower", indata$Citation))  %>%
  group_by(DataProvider, ScientificName, DatasetID, Citation, FishCouncilRegion) %>%
  summarize(n_records = n(),
            minyear = min(as.numeric(ObservationYear)),
            maxyear = max(as.numeric(ObservationYear))
)
View(x)



setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### Table PIAffiliation #####
d$PIAffiliation
table(d$PIAffiliation)

##### GREPL SurveyID #####
x <- d %>%
  filter(DatasetID == "BOEM_2007_2010")  %>%
  group_by(Flag, FlagReason, PI, DataContact, SurveyID, Reporter, Vessel, Locality, Family, ScientificName) %>%
  summarize(n_records = n(),
            minyear = min(as.numeric(ObservationYear)),
            maxyear = max(as.numeric(ObservationYear))
  )

##### Read in txt delimited data from MBARI JAVA query#####
setwd("C:/rworking/digs/indata")

# upload all data files (skipping first 9 lines)
d1 <- read.delim2("MBARI_download_Porifera_1982-2016_external.txt", skip = 9, header = T, sep="\t", quote = "\"")
d2 <- read.delim2("MBARI_download_Anthozoa_2012-2016_external.txt", skip = 9, header = T, sep="\t", quote = "\"")
d3 <- read.delim2("MBARI_download_Anthoathecatae_2012-2016_external.txt", skip = 9, header = T, sep="\t", quote = "\"")

d1 <- d1[order(d1$ObservationDate), ]
d2 <- d2[order(d2$ObservationDate), ]
d3 <- d3[order(d3$ObservationDate), ]

d1a <- d1[1:39435,]
d1b <- d1[39436:78870,]

write.csv(d1a,"MBARI_Porifera_Download_External_A_1982_2016.csv", row.names = F, quote = T)
write.csv(d1b,"MBARI_Porifera_Download_External_B_1982_2016.csv", row.names = F, quote = T)
write.csv(d2,"MBARI_Anthozoa_Download_External_2012_2016.csv", row.names = F, quote = T)
write.csv(d3,"MBARI_Anthoathecatae_Download_External_2012_2016.csv", row.names = F, quote = T)

#check
setdiff(names(d1), names(d2))
setdiff(names(d1), names(d3))
tail(d1a$ObservationDate, n=30)

##### Mapping process for MBARI data #####
# requires ggmap library
library(ggmap)

# transform latitudes and longitudes that are not behaving
x <- as.character(d1$Longitude[d1$Longitude != "0" & is.na(d1$Longitude) == F])
y<-as.numeric(x)
plot(y)
z<- y[y < 0]
plot(z)

x2 <- as.character(d1$Latitude[d1$Latitude != "0" & is.na(d1$Latitude) == F])
y2<-as.numeric(x2)
plot(y2)
z2<- y2[y2 > 0]
plot(z2)

z2 <- z2[is.na(z2) == F]
z <- z[is.na(z) == F]

xy <- cbind(z,z2)
class(xy)
xy <- as.data.frame(xy)
names(xy) <- c("Longitude", "Latitude")

lat = mean(z2)
long = mean(z)
loc <- get_map(location = c(long,lat), maptype = "terrain",zoom = 3)
ggmap(loc) + geom_point(data=xy, color = "red", size = 1, aes(x=Longitude, y = Latitude))

##### Importing schema #####

#set working directory
setwd("C:/rworking/digs/indata")

#read from csv to dataframe
s<-read.csv("20160418-1_Schema_for_DSC_RTP_National_Database.csv", header = T)

#get rid of extra fields
s<-s[,-(20:33)]

#check
fix(s)
names(s)
str(s)
unique(s$DataType)
unique(s$OldGroup)

x <- s %>%
  filter(grepl("GIS", OldGroup)) %>%
  group_by(OBIS_USA_Section,OldGroup) %>%
  summarise(n=n())
fix(x)

x <- s %>%
  filter(InternalUseOnly == 1) %>%
  group_by(FieldName) %>%
  summarise(n=n())
fix(x)



# ____ Comparing field names between database and schema
# trim whitespace from FieldName
s$FieldName <- gsub(" ", "", s$FieldName)

# look at set differences
setdiff(names(d), s$FieldName)
setdiff(s$FieldName,names(d))

##### looking at standard download  #####

# set working directory
setwd("C:/rworking/digs/indata")

# read from csv to dataframe
st<-read.csv("deep_sea_corals_standard.csv", header = T)

#check
sort(names(st))

##### grepl inpection of ScienficName  #####

table(factor(d[grepl("Zoantharia", d$ScientificName) , c("ScientificName")]))
d[grepl("Zoantharia", d$ScientificName) , c("ScientificName", "Flag", "FlagReason")]

##### Compare two columns using fuzzy mapping #####

# Here's where the algorithm starts...
# Generate a signature from the set you want to match to reduce some of the minor differences between strings
# The signature is created by  1. convert all characters to lower case, 2. sort the words alphabetically, and 3. concatenate them with no spaces.
# So for example, United Kingdom would become kingdomunited
# We might also remove stopwords such as 'the' and 'of'.

# Define the signature and the partial functions
signature=function(x){
  sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
  return(sig)
}
partialMatch=function(x,y,levDist=0.05){
  xx=data.frame(sig=sapply(x, signature),row.names=NULL)
  yy=data.frame(sig=sapply(y, signature),row.names=NULL)
  xx$raw=x
  yy$raw=y
  xx=subset(xx,subset=(sig!=''))
  xy=merge(xx,yy,by='sig',all=T)
  matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass="Duplicate"
  todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
  colnames(todo)=c('sig','raw')
  todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
  todo=merge(todo,yy,by.x='partials',by.y='sig')
  partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
  partial.matched$pass="Partial"
  matched=rbind(matched,partial.matched)
  un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched=rbind(matched,un.matched)
  }
  matched=subset(matched,select=c("raw.x","raw.y","pass"))

  return(matched)
}

# load master taxonomy table
setwd("C:/rworking/digs/indata")
master <- read.csv("20160421-1_taxonomy.csv", header = T)
x <- unique(d$ScientificName)
y <- master$ScientificName

# First just try setdiff
diff<-setdiff(x,y)
setwd("C:/rworking/digs/outdata")
write.csv(diff,"setdiff.csv", row.names = F, quote = T)

# create the fuzzy matches (use the functions above)
matches=partialMatch(x,y)

# # do the match the other way around if needed
# matches=partialMatch(y,x)

# take a look
fix(matches)
length(matches$raw.x)
length(x)
length(y)

# create a list of those rows that did not match up at all
list<-setdiff(x, matches$raw.x)

# subset the data to non-matching records olnly
nonmatch<-subset(d, d$ScientificName %in% list, select = c(ScientificName,VernacularNameCategory,
                                                 TaxonRank,ScientificNameAuthorship,
                                                 AphiaID,Phylum,Class,
                                                 Subclass,Order,Suborder,Family,
                                                 Subfamily,Genus,Subgenus,Species,Subspecies))

nonmatch <- nonmatch %>%
   group_by(ScientificName,VernacularNameCategory,
            TaxonRank,ScientificNameAuthorship,
            AphiaID,Phylum,Class,
            Subclass,Order,Suborder,Family,
            Subfamily,Genus,Subgenus,Species,Subspecies) %>%
  summarize(n_records = n())

# look at the relult
fix(nonmatch)

# do a sub for only the partial matches
pmatches <- matches[matches$pass == "Partial",]

# save a copy of partials
setwd("C:/rworking/digs/outdata")
write.csv(pmatches,"20160420-1_partial_matches_btw_NatDB_20160323-1_and_20160419-1_taxonomy.csv", row.names = F, quote = T)

# save a copy of nonmatch
setwd("C:/rworking/digs/outdata")
write.csv(nonmatch,"20160420-1_non_matches_btw_NatDB_20160323-1_and_20160419-1_taxonomy.csv", row.names = F, quote = T)

##### Length of Names #####

length(names(d))
du <- d[d$Flag == "0",]
t <-table(factor(du$SamplingEquipment), useNA = "always")
t2 <-table(factor(du$RecordType), useNA = "always")

#output
setwd("C:/rworking/digs/outdata")
write.csv(t2,"t2.csv", row.names = F, quote = T)

##### NGIA Point Locality Assignment #####
# library(rgdal)
# setwd("C:/rworking/digs/indata")
# ngia <- readOGR(".", "undersea") #note: to make this work
# #I needed to modify the undersea file in QGIS (vector/multipoint to single point conversion)
# proj4string(ngia)
load("C:/rworking/digs/indata/ngia.RData")

# Creating gisNGIA field
#defining subset of d to use, keeping the original data frame intact (this is a subset of d)
sub <- indata %>%
  filter(Flag == "0") %>%
  select(CatalogNumber, Locality, Latitude, Longitude)

# defining coordinates "sub"
coords <- subset(sub, select = c("Longitude", "Latitude"))

# making coords vectors numeric
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)

# creating SpatialPointsDataFrame from the subset.
spdf<-SpatialPointsDataFrame(coords, sub,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                             match.ID = TRUE)

# NGIA (gisLocality) Point to point intersection with NGIA (long run time for big sets)
# Define these vectors, used in the loop.
# the closestSiteVector is the vector of ID's from the ngia layer that was ID'd as the closest
# named locality.  the minDistVec gives the actual distance between the occurrence point and the name (ngia) point
closestSiteVec <- vector(mode = "numeric",length = nrow(spdf))
minDistVec     <- vector(mode = "numeric",length = nrow(spdf))

# Warning: this can take long time with the whole dataset. Much faster for subsets.

for (i in 1 : nrow(spdf))
{
  distVec <- spDistsN1(ngia,spdf[i,],longlat = TRUE)
  #units of distance are in kilometers Great Circle Distance
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}

# #checking the names that were generated
# names(ngia)
# names(spdf)

# defining the NGIA names vector
# this pulls out the FULL_NAME from the ngia spatial points
# data frame based on the closestSiteVec calculated in the loop above
gisNGIA <- as(ngia[closestSiteVec,]$FULL_NAME,"character")

# assign the Full Name vector back to the data in the appropriate place
sub$gisNGIA <- gisNGIA

# assign the minDistVec to a new variable called gisLocalityDist (units = km).  Adding this
# variable allows us to threshhold the distance
# units of distance are in kilometers Great Circle Distance
sub$gisNGIADist <- minDistVec

# checking the new variables
sub2 <- sub %>%
  group_by(Locality, gisNGIA) %>%
  summarize(n_records = n())
fix(sub2)
sub2 <- sub[gisNGIA == "Abraham Canyon", ]


# checking the results of the NGIA gisLocality assignments
table(factor(data$gisNGIA), useNA = "always")
table(factor(data$Locality), useNA = "always")

sub2 <- subset(sub, gisNGIADist < 1000)
head(sub2[,c("Locality", "gisNGIADist", "gisNGIA")], n = 1000)
tail(sub2[,c("Locality", "gisNGIADist", "gisNGIA")], n = 1000)
fix(sub2)

#output
setwd("C:/rworking/digs/outdata")
write.csv(data,"gisNGIA_Localities.csv", row.names = F, quote = T)
names(data)

##### GEBCO Point Locality Assignment #####
library(rgdal)
setwd("C:/data/BaseLayers/GEBCO_feature_names/features")
gebco <- readOGR(".", "features-point") #note: to make this work
#I needed to modify the undersea file in QGIS (vector/multipoint to single point conversion)
proj4string(gebco)
gebco<-spTransform(gebco, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#create a unified variable
gebco$nametype <- paste(gebco$name, gebco$type, sep = " ")

# checking
names(gebco)

# NGIA (gisLocality) Point to point intersection with NGIA (long run time for big sets)
# Define these vectors, used in the loop.
# the closestSiteVector is the vector of ID's from the ngia layer that was ID'd as the closest
# named locality.  the minDistVec gives the actual distance between the occurrence point and the name (ngia) point
closestSiteVec <- vector(mode = "numeric",length = nrow(spdf))
minDistVec     <- vector(mode = "numeric",length = nrow(spdf))

# Warning: this can take long time with the whole dataset. Much faster for subsets.

for (i in 1 : nrow(spdf))
{
  distVec <- spDistsN1(gebco,spdf[i,],longlat = TRUE)
  #units of distance are in kilometers Great Circle Distance
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}


# #checking the names that were generated
# names(ngia)
# names(spdf)

# defining the NGIA names vector
# this pulls out the FULL_NAME from the ngia spatial points
# data frame based on the closestSiteVec calculated in the loop above
gisGEBCO <- as(gebco[closestSiteVec,]$nametype,"character")

# assign the Full Name vector back to the data in the appropriate place
sub$gisGEBCO<- gisGEBCO

# assign the minDistVec to a new variable called gisLocalityDist (units = km).  Adding this
# variable allows us to threshhold the distance
# units of distance are in kilometers Great Circle Distance
sub$gisGEBCODist <- minDistVec


#plot the data
plot(ngia)
#plot(gebco)
points(spdf, col = "red", cex = .6)

#look at output
sub2 <- sub %>%
  group_by(Locality, gisNGIA, gisGEBCO) %>%
  summarise(n=n())
fix(sub2)

#output
setwd("C:/rworking/digs/outdata")
write.csv(sub,"gis_Localities.csv", row.names = F, quote = T)
names(sub)

##### Graphing using facets #####
qplot(DepthInMeters, data = d, colour = Class, facets = .~Order, bins = 30)

##### Loading geoJSON #####
library(rgdal) # the library we'll be using
setwd("C:/rworking/digs/indata")
cg <- readOGR("Carcharhinus galapagensis.JSON", "OGRGeoJSON")
proj(cg)

spplot(cg,c("ACRES"),scales=list(draw=TRUE,cex=0.75))

writeOGR(obj=cg, dsn= "tempdir", layer="cg", driver="ESRI Shapefile")


summary(cg)

##### Table of how many images are in the database #####
t <- table((is.na(indata$ImageURL)==F), useNA = "always")
t

##### Gulf MesoPhotic Species List for Peter E. #####
x <- d %>%
  filter(as.numeric(Flag) == 0,
         FishCouncilRegion == "Gulf of Mexico",
         VernacularNameCategory == "stony coral (branching)",
         as.numeric(DepthInMeters) > 49,
         as.numeric(DepthInMeters) < 201)  %>%
  group_by(VernacularNameCategory, ScientificName, TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family,
           Subfamily, Genus, Subgenus, Species, Subspecies) %>%
  summarize(n = n())

#output
setwd("C:/rworking/digs/outdata")
write.csv(x,"20160623-0_DSCRTP_NatDB_20160620-7_GOMEX_StonyCoralBranching_Meso_Subset_Summary.csv", row.names = F, quote = T)

y <- d %>%
  filter(as.numeric(Flag) == 0,
         FishCouncilRegion == "Gulf of Mexico",
         VernacularNameCategory == "black coral",
         as.numeric(DepthInMeters) > 49,
         as.numeric(DepthInMeters) < 201)  %>%
  group_by(VernacularNameCategory, ScientificName, TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family,
           Subfamily, Genus, Subgenus, Species, Subspecies) %>%
  summarize(n = n())


#output
setwd("C:/rworking/digs/outdata")
write.csv(y,"20160623-0_DSCRTP_NatDB_20160620-7_GOMEX_Black_Coral_Meso_Subset_Summary.csv", row.names = F, quote = T)

z <- d %>%
  filter(as.numeric(Flag) == 0,
         FishCouncilRegion == "Gulf of Mexico",
         VernacularNameCategory == "gorgonian coral",
         as.numeric(DepthInMeters) > 49,
         as.numeric(DepthInMeters) < 201)  %>%
  group_by(VernacularNameCategory, ScientificName, TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family,
           Subfamily, Genus, Subgenus, Species, Subspecies) %>%
  summarize(n = n())


#output
setwd("C:/rworking/digs/outdata")
write.csv(z,"20160623-0_DSCRTP_NatDB_20160620-7_GOMEX_Gorgonian_Coral_Meso_Subset_Summary.csv", row.names = F, quote = T)

##### Standardizing DataContact #####
x <- indata %>%
  filter(as.numeric(Flag) == 0,
         DataProvider == "Smithsonian Institution, National Museum of Natural History") %>%
  group_by(DataProvider, DataContact) %>%
  summarize(n = n())

##### Faceted ggplot ####

x$DepthCat <-
  factor(x$DepthCat,
         levels=c("50-200 meters", "201-1000 meters", ">1000 meters"), order=TRUE)

indatafilter <- indata  %>%
  filter(Flag == "0", DepthInMeters != "-999",
         as.numeric(DepthInMeters) > 0,
         as.numeric(DepthInMeters) <= 6000,
         VernacularNameCategory == "gorgonian coral",
         TaxonRank == "species",
         #Genus == "Nicella",
         #Ocean == "North Pacific" | Ocean == "North Atlantic",
         FishCouncilRegion == "Gulf of Mexico",
         #FishCouncilRegion == "South Atlantic",
         is.na(VernacularNameCategory) == F,
         #is.na(FishCouncilRegion) == F,
         DataProvider != "Monterey Bay Aquarium Research Institute",
         is.na(DepthCat) == F
  )

g <- ggplot(data=filt, aes(x=Phylum, fill=Ocean)) +
  geom_bar()
g

g + theme(axis.text.x = element_text(angle = 90, vjust = .2, hjust=1)) + coord_flip() + theme_dark()

##### substrate and habitat code translation #####

# from csv
setwd("C:/rworking/digs/indata")
d <- read.csv("20160818-0_NOAA_OER_EX1504_2015_2015.csv", header = T)
codes <- read.csv("codes.csv", header = T)

# add the existing substrate and habitat codes to OccurrenceComments.
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

d$OccurrenceComments <- paste5(d$OccurrenceComments,
                               "Original Substrate values",d$Substrate,
                               "Original Habitat values",d$Habitat,
                               sep = "; ", na.rm = T)

# # checking
# names(d)
# table(d$OccurrenceComments)
# head(d$OccurrenceComments)

# extracting the substrate variable vector from the dataset
sub <- d$Substrate

# creating a substrate dataframe from the vector
sub <- as.data.frame(sub)

# creating an ordering variable
sub$order <- 1:nrow(sub)

# # check
# length(sub$sub)
# names(codes)

# joining the codes
join <- merge(sub, codes, by.x = "sub", by.y = "codes", all.x = T)
join <- join[order(join$order),]

# # check
# fix(join)
# names(join)
# length(join$english)
# length(d$Substrate)

# substitute newly created vector of english translation of substrate codes back
# into the original dataset
d$Substrate <- join$english

# extracting the habitat variable vector from the dataset
hab <- d$Habitat

# creating a habitat dataframe from the vector
hab <- as.data.frame(hab)
#fix(hab)

# separate the habitat variable into a bunch of colum n - create new dataframe called habsep
habsep <- hab %>% separate(hab, c("a", "b", "c", "d", "e", "f", "g","h" ), extra = "merge", fill = "right")
#fix(habsep)

# add an ordering variable to the
habsep$order <- 1:nrow(habsep)

# do all the matching
newhab <- merge(habsep, codes, by.x = "a", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'a_new'

newhab <- merge(newhab, codes, by.x = "b", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'b_new'

newhab <- merge(newhab, codes, by.x = "c", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'c_new'

newhab <- merge(newhab, codes, by.x = "d", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'd_new'

newhab <- merge(newhab, codes, by.x = "e", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'e_new'

newhab <- merge(newhab, codes, by.x = "f", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'f_new'

newhab <- merge(newhab, codes, by.x = "g", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'g_new'

newhab <- merge(newhab, codes, by.x = "h", by.y = "codes", all.x = T)
newhab <- newhab[order(newhab$order),]
names(newhab)[names(newhab) == "english"] <- 'h_new'

# reorder the newhab table
newhab <- newhab[order(newhab$order),]

# paste all the hab fields together while leaving out the NA
# values in the paste string.

paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

newhab$paste <- paste5(newhab$a_new, newhab$b_new,
                       newhab$c_new, newhab$d_new,
                       newhab$e_new, newhab$f_new,
                       newhab$g_new, newhab$h_new,
                       sep = "; ", na.rm = T)
# # check
# length(newhab$paste)
# table(newhab$paste)
# fix(newhab)

# add the tranlated habitat variable back to the orginal set
d$Habitat <- newhab$paste

setwd("C:/rworking/digs/outdata")
write.csv(d,"20160818-0_NOAA_OER_EX1504_2015_2015.csv", row.names = F, quote = T)

##### Looking into bugs in Genus using grep #####
x <- table(indata[grepl(" ", indata$Genus), c("Genus", "Subgenus", "Flag", "ScientificName")], useNA = "always")
x <- as.data.frame(x)
x[x$Freq != 0,]

##### Using ERDDAP #####
getwd()

download.file(url="http://ecowatch.ncddc.noaa.gov/erddap/tabledap/deep_sea_corals.html?CatalogNumber,DataProvider<100,ScientificName,VernacularNameCategory,TaxonRank,ObservationDate,latitude,longitude,DepthInMeters,DepthMethod,Locality,LocationAccuracy,Repository,IdentificationQualifier,SamplingEquipment,RecordType,SampleID", destfile="test.csv")
test<-read.csv(file="/home/bsimons/test.csv")

##### Mapping Density #####
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
library(scales)
library(ggmap)
library(MASS)
library(sp)
#install.packages("viridis")
library(viridis)

# subset
sub <- indata %>%
  filter(Flag == "0", Genus == "Nicella")


# get density polygons
dens <- contourLines(
  kde2d(sub$Longitude, sub$Latitude,
        lims=c(expand_range(range(sub$Longitude), add=0.5),
               expand_range(range(sub$Latitude), add=0.5))))

# this will be the color aesthetic mapping
sub$Density2 <- 0

# density levels go from lowest to highest, so iterate over the
# list of polygons (which are in level order), figure out which
# points are in that polygon and assign the level to them

for (i in 1:length(dens)) {
  tmp <- point.in.polygon(sub$Longitude, sub$Latitude, dens[[i]]$x, dens[[i]]$y)
  pop$Density2[which(tmp==1)] <- dens[[i]]$level
}

map <- get_map(location="United States", zoom=3, maptype="satellite")

gg <- ggmap(Canada, extent="normal")
gg <- gg + geom_point(data=pop, aes(x=LONG, y=LAT, color=Density))
gg <- gg + scale_color_viridis()
gg <- gg + theme_map()
gg <- gg + theme(legend.position="none")
gg

##### Preparation of siteXspecies matrix #####
site.sp <- cast(sub, gisMEOW ~ ScientificName,
                value='IndividualCount',
                sum)
site.sp <- as.data.frame(site.sp)

# creating a site variable
site.sp$site <- paste(site.sp$gisMEOW, sep = "::")

# getting rid of non-needed variables
site.sp <- site.sp %>%
  select(-gisMEOW)

# moving the site variable to the beginning
col_idx <- grep("site", names(site.sp))
site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  select(-site)

# look at result
site.sp[1:10, 1:10]

# Ger a summary by site (row)
yo <- apply(site.sp, 1, sum)

# bottom, left, top and right
par(mar=c(20,10,5,2))
barplot(yo, las=2, cex.names = .8,
        main = "Number of Coral Observations by Marine Eco-Region")
title(xlab = "Marine Eco-Region", cex = 1.5,
      line = 15)
title(ylab = "Number of Observations", cex = 1.5,
      line = 4)

##### Looking at images #####
x <- indata %>%
  filter(is.na(ImageFilePath) == F,
         is.na(ImageURL) == T) %>%
  select(CatalogNumber, DataProvider, PI, DataContact, Vessel, ObservationYear, ImageFilePath, ImageURL, ObservationDate, Latitude, Longitude, DatasetID)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20161013-0_Possible_Missing_Images_from_DSCRTP_NatDB_20151008-0.csv", row.names = F, quote = T)

fix(x)

##### Inspecting DataProvider ######
table(factor(indata$DataProvider), useNA = "always")

##### Inspecting Cover ######
table(factor(indata$Cover), useNA = "always")

##### Southeastern broad region subset based on selection box for Daniel Wagner #####

# #From Daniel Wagner on 20161020
# Here are the boundaries for the box that I want to get the deep-sea coral records from:
# N: 35.25° N
# S: 14.5° N
# W: 63.8° W
# E: 97.75° W

indatafilter <- subset(indata, as.numeric(Latitude) > 14.50000 &
                   as.numeric(Latitude) < 35.25000 &
                   as.numeric(Longitude) < -63.8000 &
                   as.numeric(Longitude) > -97.75000 &
                   Flag == "0")

setwd("C:/rworking/digs/outdata")
write.csv(indatafilter,"DSCRTP_NatDB_20160901-0_Subset_Souteastern_Bounding_Box.csv")

##### Mapping filtered datasets #####
# Transforming Longitudes to 0-360
# stripping off longitudes for use as a vector
xpos <- indatafilter$Longitude
# Use this additional transformation if you cross the dateline (Alaska stuff), don't use the transformationif not in Alaska or HI
# xpos <- ifelse(xpos < 0, (180-xpos*(-1))+180, xpos)
#summary(x)

# setting lat
lat <- c(min(indatafilter$Latitude), mean(indatafilter$Latitude))

# # setting Longitude range
# # use next line if not in Alaska
# lon <- c(min(indata$Longitude), max(indata$Longitude))

# OR

# use this if in Alaska or HI, and not the line "lon" line above
lon <- c(-150,-100)

#creating bounding box, change the f value if you want the bigger box
bbox<-make_bbox(lon,lat, f = 20) # f value sizes box small number zoom in
#bbox
map <- get_map(bbox, maptype = "satellite")
p <- ggmap(map)
p <- p + geom_point(data=indatafilter, aes(x=xpos, y=Latitude), size=1, color = "red")
p

##### removing blank rows from dataset #####
indata <- indata %>%
  filter(is.na(DataProvider) == F)


##### histogram with density estimator and rug #####

indatafilter <- indata %>%
  filter(FishCouncilRegion == "NewEngland",
         Flag == "0")

# rm(list = ls())
# Data: Proportion of choices from the good decks as reported in 39 studies
par(cex.main = 1.5,
    mar = c(6, 6, 4, 5) + 0.1,
    mgp = c(3.5, 1, 0),
    cex.lab = 1.5 ,
    font.lab = 3,
    cex.axis = 1.3,
    bty = "n",
    las = 1)

h <- hist(indatafilter$DepthInMeters,
          freq = F,
          main = "",
          xlab = "",
          ylab = "",
          xlim = c(0,2000),
          axes = FALSE,
          col = "grey",
          breaks = 100,
          las = 2)

axis(1, seq(0, 2000, by = 100), las=2, cex.axis=.6)
axis(2, labels = F, lwd.ticks = 1)

rug(jitter(indatafilter$DepthInMeters))
mtext("Depth (meters)", side = 1, line = 4, cex = 1.5, font = 2)
mtext("Proportion of Records", side = 2, line = 1, cex = 1.5, font = 2, las = 0)
lines(density(indatafilter$DepthInMeters), lwd = 2)

##### Mapping filtered datasets #####
# Transforming Longitudes to 0-360
# stripping off longitudes for use as a vector
xpos <- indatafilter$Longitude
# Use this additional transformation if you cross the dateline (Alaska stuff), don't use the transformationif not in Alaska or HI
# xpos <- ifelse(xpos < 0, (180-xpos*(-1))+180, xpos)
#summary(x)

# setting lat
#lat <- c(min(indatafilter$Latitude), max(indatafilter$Latitude))
lat <- c(52,56)
# # setting Longitude range
# # use next line if not in Alaska
# lon <- c(min(indata$Longitude), max(indata$Longitude))

# OR

# use this if in Alaska or HI, and not the line "lon" line above
lon <- c(-140,-130)

#creating bounding box, change the f value if you want the bigger box
bbox<-make_bbox(lon,lat, f = 1) # f value sizes box small number zoom in
#bbox
map <- get_map(bbox, maptype = "satellite")
p <- ggmap(map)
p <- p + geom_point(data=indatafilter, aes(x=xpos, y=Latitude), size=1, color = "red")
p


##### Contours from ETopo1 #####

library(marleaflet)
papoue <- getNOAA.bathy(lon1 = 140, lon2 = 155,
                        lat1 = -13, lat2 = 0, resolution = 10)


# Creating color palettes
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))



plot(papoue, image = TRUE, land = TRUE, lwd = 0.03,
     bpal = list(c(0, max(papoue), greys),
                 c(min(papoue), 0, blues)))


points(indata$Longitude, indata$Latitude, pch = 21, col = "black",
       bg = "yellow", cex = 1.3)
text(152, -7.2, "New Britain\nTrench", col = "white", font = 3)

##### Looking at Falkor data #####
tab

x <- indata %>%
  filter(grepl("Falkor", Vessel)) %>%
  group_by(ScientificName, Flag, FlagReason, DataProvider, DataContact, Vessel, Purpose, SurveyID, RecordType) %>%
  summarise(n=n())

x <- indata %>%
  filter(grepl("Falkor", Vessel), is.na(ImageURL) == F)  %>%
  group_by(ScientificName, Flag, FlagReason, DataProvider, DataContact, Vessel, Purpose, SurveyID, Vessel, ImageURL, Latitude, Longitude) %>%
  summarise(n=n())

fix(x)

setwd("C:/rworking/digs/OutData")
write.csv(x,"DSCRTP_NatDB_20160901-0_Falkor_Subset.csv", row.names = F, quote = T)

##### Working on Taxonomy Table #####

setwd("C:/rworking/digs/indata")
tax <- read.csv("20161201-0_taxonomy.csv",header = T)
tax$ScientificName <- trim(tax$ScientificName)
no_auth <- read.csv("no_auth.csv",header = T)
join <- merge(tax, no_auth, by = "ScientificName", all.x = T)
names(join)


setwd("c:/rworking/digs/outdata")
write.csv(join, "newtax.csv", row.names = F, eol = "\r")

# length(missing$ScientificName)
# length(join2$ScientificName)
# join3 <- subset(join2,!duplicated(join2$ScientificName))
# length(join3$ScientificName)
# join4 <- join3[,names(tax)]
# length(join4$ScientificName)
#
# newtax <- rbind(tax, join4)
# newtax$TaxonRank <- tolower(newtax$TaxonRank)
# #
# # check
# length(missing$ScientificName) + length(tax$ScientificName)
# length(newtax$ScientificName)
# newtax %>%
#   filter(grepl("Australisis", ScientificName)) %>%
#   select(ScientificName, TaxonRank, AphiaID, Phylum)
# #
# setwd("c:/rworking/digs/outdata")
# write.csv(newtax, "newtax.csv", row.names = F, eol = "\r")
##### Counting Records #####
x <- indata %>%
  filter(Flag == "0") %>%
  group_by(FishCouncilRegion) %>%
  summarise(n=n())

fix(x)
setwd("C:/rworking/digs/OutData")
write.csv(x,"yo.csv", row.names = F, quote = T)

##### DatasetID Exploration #####
datasetID <- indata %>%
  filter(Flag == "0") %>%
  group_by(DataProvider, Repository, Vessel, VehicleName, ObservationYear, RecordType, SamplingEquipment, SurveyID, DatasetID) %>%
  summarize(n_records = n(),
            EventID_count = length(unique(EventID)),
            MinYear = min(as.numeric(ObservationYear)),
            MaxYear = max(as.numeric(ObservationYear)))

setwd("C:/rworking/digs/OutData")
write.csv(datasetID,"DSCRTP_NatDB_20170119-0_Dataset_Summary.csv", row.names = F, quote = T)


##### DatasetID Exploration #####
datasetID <- indata %>%
  filter(Flag == "0") %>%
  group_by(Vessel, VehicleName, SurveyID, ObservationYear, DatasetID) %>%
  summarize(n_records = n(),
            EventID_count = length(unique(EventID)))


setwd("C:/rworking/digs/OutData")
write.csv(datasetID,"DSCRTP_NatDB_20170119-0_Dataset_Summary_Minimalist.csv", row.names = F, quote = T)

##### Exporting Temple Dataset for Rachel Basset #####
datasetID <- indata %>%
  filter(Flag == "0", grepl("Temple", DataProvider))

setwd("C:/rworking/digs/OutData")
write.csv(datasetID,"DSCRTP_NatDB_20170119-0_Temple_Data_for_Rachel_Basset.csv", row.names = F, quote = T)


##### GREPL Filtering #####
x <- indata %>%
  filter(grepl("Katz", DataContact))  %>%
  group_by(Flag, FlagReason, PI,DataProvider, Citation, Repository, SurveyID, DataContact) %>%
  summarise(n=n())
fix(x)

x <- indata %>%
  filter(grepl("118669", CatalogNumber))  %>%
  group_by(Flag, FlagReason, PI,DataProvider, Purpose, SurveyID, DataContact) %>%
  summarise(n=n())

fix(x)

setwd("C:/rworking/digs/OutData")
write.csv(x,"DSCRTP_NatDB_20170125-1_EX1102_Katz_Subset.csv", row.names = F, quote = T)

##### Extract GOMEX data for mapping purposes #####
#unique(indata$FishCouncilRegion)

# with images
x <- indata %>%
  filter(grepl("Gulf of Mexico", FishCouncilRegion), Flag == "0", is.na(ImageURL) == F) %>%
  dplyr::select(DatasetID, CatalogNumber, SampleID, ScientificName, IndividualCount, Vessel, VehicleName, Latitude, Longitude, ObservationYear, DepthInMeters, gisEtopoDepth, ImageURL)
x$DepthInFeet <- x$DepthInMeters * 3.28084
x$DepthInFeet[x$DepthInFeet < 0] <- -999
x$DepthInFeet <- as.numeric(x$DepthInFeet)
View(x)

# with images and without images
x <- indata %>%
  filter(grepl("Gulf of Mexico", FishCouncilRegion), Flag == "0") %>%
  select(CatalogNumber, SampleID, ScientificName, IndividualCount, Vessel, VehicleName, Latitude, Longitude, ObservationYear, DepthInMeters, gisEtopoDepth, ImageURL)
x$DepthInFeet <- x$DepthInMeters * 3.28084
x$DepthInFeet[x$DepthInFeet < 0] <- -999
x$DepthInFeet <- as.numeric(x$DepthInFeet)

#table(x$DepthInMeters)

#fix(x)

setwd("C:/rworking/digs/OutData")
write.csv(x,"DSCRTP_NatDB_20170125-1_GOMEX_Subset_for_map5.csv", row.names = F, quote = T)

##### R based mapping work #####
library(ggmap)
x <- indata %>%
  filter(grepl("Gulf of Mexico", FishCouncilRegion), Flag == "0") %>%
  select(CatalogNumber, SampleID, ScientificName,
         IndividualCount, Vessel, VehicleName, Latitude, Longitude, ObservationYear, DepthInMeters, gisEtopoDepth, ImageURL)

loc <- get_map(location = c(as.numeric(mean(x$Longitude)), as.numeric(mean(x$Latitude))), maptype = "terrain",zoom = 3)
ggmap(loc) + geom_point(data=x, color = "red", size = 1, aes(x=Longitude, y = Latitude))

g <- ggplot(data=x, aes(x=ScientificName, fill=ScientificName)) +
  geom_bar() + facet_grid(Vessel ~ VehicleName)

g + theme(axis.text.x = element_text(angle = 90, vjust = .2, hjust=1))


##### _____ exploring melt #####
#load required packages
library(reshape)

#set working directory
setwd("C:/rworking/digs/indata")

#from csv
indata<-read.csv("PSBF_forRMelt_rpm.csv", header = T)

#check
names(indata)

#melt the data
melt <- melt(indata, id=c("Site","Cruise","Dive","Location","Transect","PhotoName","Area"))

#take a look at the new data result
fix(melt)

#export the data
setwd("C:/rworking/digs/outdata")
write.csv(melt,"melted.csv", row.names = F, quote = T)

##### _____ robis and leaflet #####
install.packages("devtools")
install.packages("xml2")
install.packages("rgbif")
install.packages("sp")
install.packages("robis")
install.packages("htmlwidgets")
install.packages("mregions")
library(devtools)
devtools::install_github("ropensci/rgbif", force = T)
devtools::install_github("iobis/robis")
require(rgdal)
require(sp)
require(dplyr)
require(reshape2)
require(xml2)
require(ggplot2)
require(robis)
require(rgbif)
require(mregions)

data <- occurrence(resourceid = 586)
data <- occurrence("Lophelia")

# for this example, convert back from data frame tbl (dplyr) to standard data frame
data <- as.data.frame(data)

head(data)
head(data, n = 100)
dim(data)
nrow(data)
ncol(data)
names(data)
str(data)
summary(data)
View(data)

# now convert to data frame tbl (dplyr)
data <- tbl_df(data)

data
head(data)
print(data, n = 100)

#filtering
require(robis)
require(dplyr)

data <- occurrence("Lophelia")
data %>% filter(scientificName == "Lophelia pertusa" & yearcollected > 2005)
View(data)

#Reordering
names(data)
data %>% arrange(datasetID, desc(eventDate))
table(data$datasetID)
unique(data$occurrenceRemarks)
View(data)

#Selecting and renaming columns
data %>% select(scientificName, eventDate, lon=decimalLongitude, lat=decimalLatitude)

#select() can be used with distinct() to find unique combinations of values:
data %>% select(scientificName, locality) %>% distinct()
View(data)

#Adding columns
data %>% mutate(zone = .bincode(minimumDepthInMeters, breaks=c(0, 10, 100))) %>% select(minimumDepthInMeters, zone) %>% filter(!is.na(zone)) %>% print(n = 100)

#Aggregation
data %>% summarise(lat_mean = mean(decimalLatitude), lat_sd = sd(decimalLatitude))
data %>% group_by(scientificName) %>% summarise(records=n(), datasets=n_distinct(datasetName))

#Restructuring
#This example converts a dataset from OBIS to a matrix format, which is more suitable for community analysis:

require(robis)
require(reshape2)

data <- occurrence(resourceid = 586)
wdata <- dcast(data, locality ~ scientificName, value.var = "individualCount")

#And the other way around, from wide format to long format:
ldata <- melt(wdata, variable.name = "scientificName", value.name = "individualCount")

#Plotting
#In this example, data for one species is extracted from an OBIS dataset.
#Density and depth are visualized using the ggplot2 package:

require(robis)
require(dplyr)
require(reshape2)
require(ggplot2)

data <- occurrence(resourceid = 586)
data <- occurrence("Lophelia")

afil <- data %>%
  filter(scientificName == "Lophelia pertusa") %>%
  group_by(locality) %>%
  summarise(n = mean(individualCount), lon = mean(decimalLongitude), lat = mean(decimalLatitude), depth = mean(minimumDepthInMeters))

ggplot() + geom_point(data = afil, aes(lon, lat, size = n, colour = depth)) +
  scale_colour_distiller(palette = "Spectral") +
  theme(panel.background = element_blank()) + coord_fixed(ratio = 1) + scale_size(range = c(2, 12))

# mapping
require(leaflet)

data <- occurrence("Paramuricea")

# QC Flagging
qcflag <- function(qc, number) {
  mask <- 2^(number-1)
  return(sapply(qc, function(x) {
    return(sum(bitwAnd(x, mask) > 0))
  }))
}

data$qcnum <- qcflag(data$qc, c(24, 28))

colors <- c("red", "orange", "green")[data$qcnum + 1]

# Mapping
require(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addCircleMarkers(m, data=data.frame(lat=data$decimalLatitude, lng=data$decimalLongitude), radius=3, weight=0, fillColor=colors, fillOpacity=0.5)
m

##### Using Marine Regions #####
# Install
install.packages("mregions")

# Or get the dev version
devtools::install_github("ropenscilabs/mregions")
install.packages("mregions")
library("mregions")

#Get list of place types (getting error on this one)
res <- mr_place_types()
head(res$type)

#Get Marineregions records by place type
res <- mr_records_by_type(type = "Continent")
head(res)

#Get and search polygons of interest names
res <- mr_names(layer = "MarineRegions:iho")
names <- mr_names_search(res, q = "Gulf of Mexico")
View(names)

# get polygon of interest
shp <- mr_shp(
  key = "MarineRegions:iho")
names(shp)
shp <- shp[shp@data$name == "Gulf of Mexico"]
View(shp)
sp::plot(shp)

##### Get occurrences from OBIS and the NDB and Map them #####
spec <- "Adelogorgia phyllosclera"
#get from OBIS
library(rerddap)
library(robis)
library(leaflet)
library(rgbif)
obis <- occurrence(spec)
obis <- obis %>%
  filter(grepl(spec, scientificName))#,collectionCode != "NOAA_DSC_RTP") #collectionCode != "NOAA_DSC_RTP
obis <- obis[, c('scientificName', 'decimalLongitude', 'decimalLatitude', 'datasetName')]
names(obis)[2:3] <- c('longitude', 'latitude')
View(obis)

# get the occurrences from the national database using erddap
indata <- tabledap('deep_sea_corals',
              fields=c('latitude','longitude','ScientificName', "ImageURL"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

# OR get them from a local working coopy
# setwd("C:/rworking/digs/indata")
# indata<-read.csv("DSCRTP_NatDB_20170125-1.csv", header = T)

# #filter ndb records from local copy
# ndb <- indata %>%
#   filter(grepl(spec, ScientificName), Flag == "0")
# ndb <- ndb %>%
#   filter(grepl(spec, ScientificName))
# ndb <- ndb[, c('ScientificName', 'Longitude', 'Latitude', 'ImageURL')]
# names(ndb)[2:3] <- c('longitude','latitude')
# View(ndb)

#filter ndb records from erddap (slight nameing differences, no Flag field)
ndb <- indata %>%
  filter(grepl(spec, ScientificName))
ndb <- ndb[, c('ScientificName', 'longitude', 'latitude', 'ImageURL')]
View(ndb)
ndb$longitude <- as.numeric(ndb$longitude)
ndb$latitude <- as.numeric(ndb$latitude)

# get from GBIF
key <- name_backbone(name=spec)$speciesKey
dat <- occ_search(taxonKey=key, return='data')
dat <- data.frame(dat)
dat <- dat %>%
  filter(grepl(spec, name))#, ownerInstitutionCode != "DSC_RTP")
gbif <- dat[, c('name', 'decimalLongitude', 'decimalLatitude', "ownerInstitutionCode")]
names(gbif)[2:3] <- c('longitude', 'latitude')
View(gbif)

# Interactive map using leaflet
m <- leaflet()
m <- addProviderTiles(m, "CartoDB.DarkMatter") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=obis,
                      radius=8,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = obis$datasetName
                      )
m <- addCircleMarkers(m, data=ndb,
                      radius=6,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1
                      #popup = zz$ScientificName
                      )
m <- addCircleMarkers(m, data=gbif,
                      radius=5,
                      weight=0,
                      fillColor= "blue",
                      fillOpacity=1,
                      popup = gbif$ownerInstitutionCode
                      )
m <- addPolygons(m, data = shp, fillColor = "orange", fillOpacity = .3)
m
##### _____ Looking at Depth #####
# creation of flag filter
x <- indata %>%
  filter(Flag == 0)

# creating a depth flag
x$DepthDiff <- abs(x$DepthInMeters - x$gisEtopoDepth)
x$FlagDepth <- ifelse(x$DepthDiff > 1500,1,0)

# plotting
p <- ggplot(x, aes(DepthInMeters,gisEtopoDepth))
p + geom_point(aes(colour = factor(x$FlagDepth)), size = .7) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50))

#checking numbers of flagged records
table(x$FlagDetph)

##### Working with the Coastal Relief Model and ETOPO####
##### Looking for Alaska Provider records #####

filt <- indata %>%
  filter(grepl("Alaska", Vessel), Flag == "0") %>%
  group_by(DataProvider, Vessel, VehicleName, ObservationYear) %>%
  summarise(n = n())

filt <- indata %>%
  filter(grepl("Provider", Vessel), Flag == "0") %>%
  group_by(DataProvider, DataContact, PI, Vessel, VehicleName, ObservationYear) %>%
  summarise(n = n())

View(filt)

##### _____ getting live topo data #####
##### setting bounding box coordinates #####
minLon <- -85
maxLon <- -82
minLat <- 23
maxLat <- 26

##### get ETOPO1 data from NCEI #####

url <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/",
                "wcs.groovy?filename=etopo1_bedrock.tif&",
                "request=getcoverage&version=1.0.0&service=wcs&",
                "coverage=etopo1_bedrock&CRS=EPSG:4326&format=geotiff&",
                "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",
                minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")
fname <- "etopo_test.tif"
download.file(url, fname, mode="wb", cacheOK="false")
etopo <- raster(fname)

##### get CRM data from NCEI #####
install.packages("raster")
library(raster)
library(sp)
url.hi <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/",
                "wcs.groovy?filename=crm.tif&",
                "request=getcoverage&version=1.0.0&service=wcs&",
                "coverage=crm&CRS=EPSG:4326&format=geotiff&",
                "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",
                minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")
fname.hi <- "crm_test.tif"

download.file(url.hi, fname.hi, mode="wb", cacheOK="false")

crm <- raster::raster(fname.hi)

setwd("C:/rworking/digs/outdata")
writeRaster(crm,'crm.tif')

##### Filtering the coral data to match elevation data extraction #####
filt <- filter(indata, as.numeric(Latitude) > minLat,
                       as.numeric(Latitude) < maxLat,
                       as.numeric(Longitude) < maxLon,
                       as.numeric(Longitude) > minLon,
                       Flag == "0")
#View(filt)
coordinates(filt) <- c("Longitude","Latitude")
proj4string(filt) <- proj4string(d)

# country boundaries
library(rworldxtra)
data(countriesHigh)

##### extract raster CRM and ETOPO data to points #####
filt$gisCRM <- extract(crm,filt)
filt$gisETOPO <- extract(etopo,filt)
filt$gisCRM.hawaii <- extract(d, filt)

# changing the sign of the depth to match schema directions
filt$gisCRM <- filt$gisCRM * -1
filt$gisETOPO <- filt$gisETOPO * -1

names(filt)
filtdata <- as.data.frame(filt)
names(filtdata)

p <- ggplot(filtdata, aes(DepthInMeters,gisETOPO))
p + geom_point(size = .7) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(h = 0, v = 0, col = "gray60")


##### make some maps of this stuff  #####
bplt <- spplot(bathy.hi, col.regions=topo.colors(64),
               colorkey=FALSE, scales=list(draw=TRUE),
               xlim = c(minLon, maxLon), ylim=c(minLat, maxLat),
               sp.layout=list(
                 # add contours:
                 list("sp.lines", rasterToContour(bathy.hi,
                                                  levels=c(-1000, -100, -50, 0, 1000))),
                 list("sp.polygons", countriesHigh, lwd=2, fill="grey"),
                 # add trawl locations coded by cruise & gear:
                 list("sp.points", filt,
                      lwd=2, cex=2, col="black"
                 )
               )
)
plot(bplt)

# black and white version
bplt <- spplot(rast, col.regions=NA, colorkey=FALSE,
               scales=list(draw=TRUE),
               xlim = c(minLon, maxLon), ylim=c(minLat, maxLat),
               sp.layout=list(
                 # add contours:
                 list("sp.lines",
                      rasterToContour(rast, levels=c(-2000, -1000, -100, -50, 0, 1000))),
                 list("sp.polygons", countriesHigh, lwd=2, fill="grey"),
                 # add trawl locations coded by cruise & gear:
                 list("sp.points", filt,
                      lwd=2, cex=2, col="black")))
plot(bplt)

##### _____ subsetting FLoSEE 2010 for Ren S. #####
x <- indata %>%
  filter(grepl("FLoSEE 2010", indata$SurveyID))  %>%
  group_by(Flag, FlagReason, DataProvider, EventID, Purpose, SurveyID, DataContact) %>%
  summarise(n=n())

View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"DSCRTP_NatDB_20170215-1_FLoSEE_2010_subset.csv", row.names = F, quote = T)







##### Accessing data via ERDDAP #####
install.packages("digest")
install.packages("data.table")
install.packages("DBI")
install.packages("assertthat")
install.packages("Rcpp")
install.packages("rerddap")
install.packages("magrittr")
install.packages("curl")

library(magrittr)
library(Rcpp)
library(assertthat)
library(DBI)
library(devtools)
library(data.table)
library(digest)
library(rerddap)
library(curl)

# list all datasets on server
# look at table datasets
x <- head(ed_datasets('table', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
fix(x)

x <- head(ed_datasets('table', url = "http://coastwatch.pfeg.noaa.gov/erddap/"), n=10)
View(x)

# looking at grid datasets
x <- head(ed_datasets('grid', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
fix(x)

# getting the whole dataset
x <- tabledap("allDatasets", url = "http://coastwatch.pfeg.noaa.gov/erddap/")
View(x)


# Get info on a datasetid, then get data given information learned
info('deep_sea_corals', url = "https://ecowatch.ncddc.noaa.gov/erddap/")$variables

x <- tabledap('deep_sea_corals',
              fields=c('latitude','longitude','ScientificName', "ImageURL"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

x <- x[is.na(x$ImageURL) == F,]

View(x)

##### Exporting to KML #####

## Load required packages
#install.packages("maptools")
library(maptools)
library(rgdal)

##Set your working directory

setwd("C:/rworking/digs/outdata")

# load the live deep sea coral and sponge database occurrence locations via ERDDAP.
x <- tabledap('deep_sea_corals',
              fields=c('latitude','longitude','ScientificName', "CatalogNumber", "FishCouncilRegion", "Locality",
                       "DataProvider", "Vessel", "VehicleName", "ObservationYear", "DepthInMeters", "ImageURL"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

x <- x[is.na(x$ImageURL) == F,]
x <- data.frame(x)
x$latitude <- as.numeric(x$latitude)
x$longitude <- as.numeric(x$longitude)



# do some geographic subsetting

x <- filter(x, latitude > 25 , latitude < 30, longitude > -100)
#fix(x)

x <- filter(x, x$FishCouncilRegion == "North Pacific")

# plot the xY coordinates

plot(x$longitude, x$latitude)

## create a SpatialPointsDataframe object and add the appropriate CRS

coordinates(x)<- c("Longitude", "Latitude")
proj4string(x)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points


setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x.kml", layer= "DSC_RTP", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

## if you have Google Earth installed double click on the kml file you just created to open it.
#The points should be loaded as labelled pins on the map.If you click on the pin you will be able to see its full name and capacity.



##### Importing Arc ASCII raster #####
# load required packages
library(raster)
library(rgdal)

# remove "tidyr" because it has a conflict with "extract"
rs.unloadPackage("tidyr")

# bring in all CRM files (except southern pacific)
setwd("C:/rworking/digs/indata/crm")

hi <- raster("hawaii_crm_v1.asc")
centgom <- raster("central_gom_crm_v1.asc")
centpac <- raster("central_pacific_crm_v1.asc")
fleast <- raster("fl_east_gom_crm_v1.asc")
neatl <- raster("ne_atl_crm_v1.asc")
nwpac <- raster("nw_pacific_crm_v1.asc")
pr <- raster("puerto_rico_crm_v1.asc")
seatl <- raster("se_atl_crm_v1.asc")
westgom <- raster("western_gom_crm_v1.asc")

# make a spatialpointsdataframe out of database points
sp_indata <- indata
coordinates(sp_indata) <- c("Longitude","Latitude")
proj4string(sp_indata) <- proj4string(hi)

# extract all depth values to points
sp_indata$hi <- extract(hi,sp_indata)
sp_indata$centgom <- extract(centgom,sp_indata)
sp_indata$centpac <- extract(centpac,sp_indata)
sp_indata$fleast <- extract(fleast,sp_indata)
sp_indata$neatl <- extract(neatl,sp_indata)
sp_indata$nwpac <- extract(nwpac,sp_indata)
sp_indata$pr <- extract(pr,sp_indata)
sp_indata$seatl <- extract(seatl,sp_indata)
sp_indata$westgom <- extract(westgom,sp_indata)

# change sign of all depth values
sp_indata$hi <- sp_indata$hi * -1
sp_indata$centgom <- sp_indata$centgom * -1
sp_indata$centpac <- sp_indata$centpac * -1
sp_indata$fleast <- sp_indata$fleast * -1
sp_indata$neatl <- sp_indata$neatl * -1
sp_indata$nwpac <- sp_indata$nwpac * -1
sp_indata$pr <- sp_indata$pr * -1
sp_indata$seatl <- sp_indata$seatl * -1
sp_indata$westgom <- sp_indata$westgom * -1

sp_indata$gisCRM <- sp_indata %>%
  mean(as.numeric(hi),
       as.numeric(centgom),
       as.numeric(centpac),
       as.numeric(fleast),
       as.numeric (neatl),
       as.numeric(nwpac),
       as.numeric(pr),
       as.numeric(seatl),
       as.numeric(westgom))

data<-as.data.frame(sp_indata)
data<-data[,111:119]
names(data)
data$CRM <- rowMeans(data, na.rm = T)
length(data$CRM)
indata$CRM <- data$CRM
names(indata)

indata$DepthDiff <- abs(indata$gisEtopoDepth - indata$CRM)
indata$FlagDepth <- ifelse(indata$DepthDiff > 100,1,0)

p <- ggplot(indata, aes(gisEtopoDepth,CRM))
p + geom_point(aes(colour = factor(indata$FlagDepth)), size = .7) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(h = 0, v = 0, col = "gray60")

##### _____ Depth Check #####
##### filtering data #####
d <- indata %>%
  filter(Flag == "0", grepl("Primnoa pacifica", indata$ScientificName)) %>%
  group_by(DataProvider, Vessel, VehicleName, SamplingEquipment, SurveyID, ScientificName) %>%
  summarize(n = n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            minDepth = min(as.numeric(DepthInMeters)),
            maxDepth = max(as.numeric(DepthInMeters)))

##### setting thresholds for difference and creating flags#####
t <- 100
z <- 20

##### building selection conditions for each depth value #####
#finding depth difference for CRM
d$CRMDepthDiff <- abs(d$DepthInMeters - d$gisCRMDepth)
#crm depth flags
crm <- d %>%
  filter(CRMDepthDiff > z & gisCRMDepth != "-999") %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)
#finding depth difference for Etopo
d$EtopoDepthDiff <- abs(d$DepthInMeters - d$gisEtopoDepth)
#etopo depth flags
etopo <- d %>%
  filter(EtopoDepthDiff > t & gisEtopoDepth != "-999") %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)
#finding depth difference for GEBCO
d$GEBCODepthDiff <- abs(d$DepthInMeters - d$gisGEBCODepth)

#GEBCO depth flags
gebco <- d %>%
  filter(GEBCODepthDiff > t & gisGEBCODepth != "-999") %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)
#depth flags from reported values
depth <- d %>%
  filter(as.numeric(DepthInMeters) == -999 | as.numeric(DepthInMeters) < 30) %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)


##### building joint file of all depth probs and de-dup
dx <- rbind(crm,etopo,gebco,depth)

##### testing for duplicated records #####
dx <- dx[!duplicated(dx), ]

#checking
length(dx$CatalogNumber)

##### make a shapefile with the OGR driver #####
d <- indata


coordinates(d) <- c("Longitude", "Latitude")
proj4string(d) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(d, dsn="d",
         layer= "d",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### making a KML file of the results #####

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points
setwd("C:/rworking/digs/outdata")
writeOGR(dx, dsn="dx.kml", layer= "DSC_RTP", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

## if you have Google Earth installed double click on the kml file you just created to open it.
#The points should be loaded as labelled pins on the map.If you click on the pin you will be able to see its full name and capacity.

##### export layer #####
setwd("C:/rworking/digs/outdata")
write.csv(dx,"dx.csv", row.names = F, quote = T)

##### making a leaflet map of the results #####
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=dx,
                      radius=8,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1
                      #popup = dx$DatasetID
)
m <- addCircleMarkers(m, data=etopo,
                      radius=6,
                      weight=0,
                      fillColor= "black",
                      fillOpacity=1,
                      popup = etopo$DepthInMeters
)
m <- addCircleMarkers(m, data=gebco,
                      radius=5,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = gebco$DepthInMeters
)
m

##### Plotting results of selections for each GIS assigned value #####
p <- ggplot(crm, aes(DepthInMeters, gisCRMDepth))
p +
  geom_point(aes(shape = factor(Flag), size = as.numeric(Flag)), size = 5) +
  scale_shape_manual(values=c(1,4)) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(col = "gray60")

##### Exporting a final set for consideration #####

p <- ggplot(etopo, aes(DepthInMeters, gisCRMDepth))
p +
  geom_point(aes(shape = factor(Flag), size = as.numeric(Flag)), size = 5) +
  scale_shape_manual(values=c(1,4)) +
  geom_point(aes(colour = factor(CRMFlagDepth)), size = 2) +
  scale_color_manual(values=c("#3b2c68", "#f2070b")) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(col = "gray60")# +
  #geom_label_repel(aes(label=CatalogNumber))

coordinates(depth)<- c("Longitude", "Latitude")
proj4string(depth)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(depth, dsn="yo.shp",
         layer= "DSC_RTP",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)



##### _____ Getting the next taxonomy table ready with WoRMS matching #####
setwd("C:/rworking/digs/indata")
tax <- read.csv("20170608-2_taxonomy_RPMcGuinn.csv", header=T, na.strings=c("","NA"))
names(tax)



list <- setdiff(list, taxfl$ScientificName)
list <- setdiff(list, taxch$VerbatimScientificName)
list <- setdiff(list, tax$ScientificName)
list

setwd("C:/rworking/digs/outdata")
write.csv(list,"list.csv", row.names = F, quote = T)

# bringing in non-matching species list
setwd("C:/rworking/digs/indata")
list <- read.csv("list.csv", header=T)
names(list)
View(list)

# checking difference
setdiff(list$ScientificName, tax$ScientificName)

# bringing in matched list from WoRMS
setwd("C:/rworking/digs/indata")
listmatch <- read.csv("list_matched.csv", header=T)
names(listmatch)
View(listmatch)

# getting rid of unwanted columns and rows (BE CAREFUL! )
listmatch <- listmatch[,1:27]
dim(listmatch)

# creating an empty tax table to populate
newtax <- tax[0,]

#adding enough empty rows
newtax[1:length(listmatch$ScientificName),]<-NA
View(newtax)

# creating added
newtax$ScientificName <- listmatch$ScientificName_accepted
newtax$AphiaID <- listmatch$AphiaID_accepted
newtax$ScientificNameAuthorship <- listmatch$Authority_accepted
newtax$Phylum <- listmatch$Phylum
newtax$Class <- listmatch$Class
newtax$Order <- listmatch$Order
newtax$Family <- listmatch$Family
newtax$Genus <- listmatch$Genus
newtax$Subgenus <- listmatch$Subgenus
newtax$Species <- listmatch$Species
newtax$Subspecies <- listmatch$Subspecies
View(newtax)

# combine new taxa with original file
tax$ScientificNameAccepted <- NA
tax2<-rbind(tax,newtax)

# exporting the new joint taxonomy table
setwd("C:/rworking/digs/outdata")
write.csv(tax2,"20170504-0_taxonomy.csv", row.names = F, quote = T)

##### Checking taxonomy version differences#####
oldtax <-read.csv("20161208-0_taxonomy.csv", header = T)
scrub <- setdiff(oldtax$ScientificName, tax$ScientificName)
add <- setdiff(tax$ScientificName, oldtax$ScientificName)

setwd("C:/rworking/digs/outdata")
write.csv(scrub,"scrub.csv", row.names = F, quote = T)
write.csv(add,"add.csv", row.names = F, quote = T)

##### _____ Looking at MBARI records #####
x <- indata %>%
  filter(grepl("Monterey", indata$DataProvider) | grepl("MBARI", indata$DataProvider))  %>%
  group_by(SurveyID,SamplingEquipment, Vessel, VehicleName) %>%
  summarise(n=n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]))

View(x)
table(factor(x$ObservationYear), useNA = "always")

##### _____ Looking at specific taxonomic master table inclusions ######
x <- tax %>%
  filter(grepl("nr.", ScientificName))

View(x)


##### Data set inspections #####

setwd("C:/rworking/digs/indata")
x <- read.csv("20170517-0_MBARI_Porifera_1997_2015.csv", header=T)
table(x$LocationAccuracy, x$NavType, useNA = "always")
table(x$NavType, useNA = "always")
table(x$EventID, useNA = "always")
table(x$SamplingEquipment, useNA = "always")
table(x$SurveyID, useNA = "always")
table(x$ObservationYear, useNA = "always")

# create a nice cross table between Vessel and VehicleName
y <- table(x$Vessel, x$VehicleName, useNA = "always")
y <- data.frame(y)
y <- y[y$Freq > 0,]
View(y)

# create a nice cross table between Vessel and VehicleName
y <- table(x$PI, x$PIAffiliation, useNA = "always")
y <- data.frame(y)
y <- y[y$Freq > 0,]
View(y)

##### Looking at Smithsonian data contact and citation #####

x <- indata %>%
  filter(grepl("Smithsonian", indata$DataProvider))  %>%
  group_by(DataContact, Citation) %>%
  summarise(n=n())

View(x)






##### Bringing in unmatched taxa in the Smithsonian database #####

#from csv
setwd("C:/rworking/digs/indata")
nomatch<-read.csv("20170315-0_taxonomy_without_WoRMS_match_RPMcGuinn.csv", header = T)
x<-read.csv("20170213-4_NMNH_NWPacific_Hourigan_1883_2017.csv", header = T)

join <- merge(nomatch, x, by.x = "ScientificName", by.y = "ScientificName", all.x = T)
join <- join[,c("VernacularNameCategory","VernacularName", "ScientificName", "TaxonRank", "AphiaID",
"Phylum","Class","Subclass","Order","Suborder",	"Family",	"Subfamily","Genus", "Subgenus",
"Species","Subspecies","ScientificNameAuthorship")]

join2 <- subset(join,!duplicated(join$ScientificName))

join2$HigherTaxonNameAuthorship <- "NA"
join2$TrueSynonyms <- "NA"
join2$SynonymAphiaID <- "-999"

setwd("C:/rworking/digs/indata")
tax <- read.csv("20170315-1_taxonomy.csv", header=T, na.strings=c("","NA"))
names(tax)
View(tax)

# checking difference
setdiff(join2$ScientificName, tax$ScientificName)

newtax <- rbind(tax, join2)

setwd("C:/rworking/digs/outdata")
write.csv(newtax,"20170315-2_taxonomy.csv", row.names = F, quote = T)

table(x$DepthMethod)
table(x$Size)
table(x$Citation)




##### Looking at Okeanos Citations #####
x <- indata %>%
  filter(grepl("Okeanos", Vessel)) %>%
  group_by(DataProvider, IdentificationQualifier, DataContact, WebSite) %>%
  summarise(n=n())

View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"DSCRTP_NatDB_20170301-0_MBARI_subset.csv", row.names = F, quote = T)









##### _____ Looking at MBARI sponge set #####
##### Setting working directories to intake #####
#setwd("C:/rworking/deep-sea-workbench/InData/IntakePipe")
setwd("C:/rworking/digs/indata")

##### Bringing in MBARI input datasets #####

#from csv
d <- read.csv("20170317-1_MBARI_Porifera_1997_2015.csv", header = T)

x <- d %>%
  group_by(PI,VehicleName, EventID, ObservationDate) %>%
  summarise(n=n())

View(x)



##### Getting subset of MBARI data for Tom #####
x <- indata %>%
  filter(grepl("Monterey", DataProvider)) %>%
  group_by(DataProvider) %>%
  summarise(n=n())

View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"DSCRTP_NatDB_20170301-0_MBARI_subset.csv", row.names = F, quote = T)

##### _____ Looking at Oceana data #####
x <- indata %>%
  filter(grepl("Geoff", indata$DataContact) | grepl("Oceana", indata$Citation))  %>%
  group_by(Flag, FlagReason, DataProvider, Purpose, SurveyID, DataContact, Phylum, DatasetID, EntryDate) %>%
  summarise(n=n(),count=sum(IndividualCount))
View(x)

setwd("C:/rworking/digs/OutData")
write.csv(x,"x.csv", row.names = F, quote = T)

##### _____ Working with image recognition #####
#install.packages("devtools")
# install.packages("https://github.com/jeremiedb/mxnet_winbin/raw/master/mxnet.zip", repos = NULL)
# install.packages("DiagrammeR")
# install.packages("imager")
require(DiagrammeR)
require(mxnet)
require(imager)
setwd("C:/rworking/digs/indata")
model <- mx.model.load("Inception/Inception_BN", iteration = 39)
mean.img <- as.array(mx.nd.load("Inception/mean_224.nd")[["mean_img"]])

im <- load.image("c:/rworking/digs/indata/coral.jpg")
plot(im)

preproc.image <- function(im, mean.image) {
  # crop the image
  shape <- dim(im)
  short.edge <- min(shape[1:2])
  xx <- floor((shape[1] - short.edge) / 2)
  yy <- floor((shape[2] - short.edge) / 2)
  croped <- crop.borders(im, xx, yy)
  # resize to 224 x 224, needed by input of the model.
  resized <- resize(croped, 224, 224)
  # convert to array (x, y, channel)
  arr <- as.array(resized) * 255
  dim(arr) <- c(224, 224, 3)
  # subtract the mean
  normed <- arr - mean.img
  # Reshape to format needed by mxnet (width, height, channel, num)
  dim(normed) <- c(224, 224, 3, 1)
  return(normed)
}

normed <- preproc.image(im, mean.img)

# Now we are ready to classify the image! We can use the predict
# function to get the probability over classes.

prob <- predict(model, X = normed)
dim(prob)
# As you can see prob is a 1 times 1000 array,
# which gives the probability over the 1000 image classes of the input.
# We can use the max.col on the transpose of prob. get the class index.

max.idx <- max.col(t(prob))
max.idx <- order(prob[,1], decreasing = TRUE)[1:20]
max.idx
# The index do not make too much sense.
# So let us see what it really corresponds to. We can read the names of the classes from the following file.

synsets <- readLines("Inception/synset.txt")
result <- synsets[max.idx]
result


##### using image classification #####
library(rgdal)
library(raster)
#install.packages("caret")
library(caret)
library("raster")
library("cluster")
#install.packages("randomForest")
library("randomForest")

# loading the layerstack
im1 <- stack("C:/rworking/digs/indata/imagetest/1.jpg")
im2 <- stack("C:/rworking/digs/indata/imagetest/2.jpg")
im3 <- stack("C:/rworking/digs/indata/imagetest/3.jpg")
im6 <- stack("C:/rworking/digs/indata/imagetest/6.jpg")

plotRGB(im2, r=3,g=2,b=1,stretch="hist")

## returns the values of the raster dataset and write them in a matrix.
image <- im6
v <- getValues(image)
i <- which(!is.na(v))
v <- na.omit(v)

## kmeans classification
E <- kmeans(v, 12, iter.max = 100, nstart = 10)
kmeans_raster <- raster(image)
kmeans_raster[i] <- E$cluster
plot(kmeans_raster)

## clara classification
clus <- clara(v,12,samples=500,metric="manhattan",pamLike=T)
clara_raster <- raster(image)
clara_raster[i] <- clus$clustering
plot(clara_raster)

## unsupervised randomForest classification using kmeans
vx<-v[sample(nrow(v), 500),]
rf = randomForest(vx)
rf_prox <- randomForest(vx,ntree = 1000, proximity = TRUE)$proximity

E_rf <- kmeans(rf_prox, 12, iter.max = 100, nstart = 10)
rf <- randomForest(vx,as.factor(E_rf$cluster),ntree = 500)
rf_raster<- predict(image,rf)
plot(rf_raster)

# comparing the methods
class_stack <- stack(kmeans_raster,clara_raster,rf_raster)
names(class_stack) <- c("kmeans","clara","randomForest")

plot(class_stack)

# histogram of classified raster
hist(rf_raster)

#computing patch statistic metrics
#install.packages("SDMTools")
require(SDMTools)

met<-PatchStat(rf_raster,cellsize = 1,latlon = FALSE)
View(met)

# Subsetting through metrics
x <- met %>%
  filter(core.area.index < .5)  %>%
  dplyr::select(patchID)
plot <- x[1,1]

#subsetting raster
r <- rf_raster
r[r != 11] <- NA
plot(r)






##### _____ Logging onto MBARI with SQL Server #####

# SQL Server login info:
#   ————————————————————————
# Server: SQL Server 2008
# Host= "dione.mbari.org”
# Port= “51001”
# Authentication = “Basic"
# UserName= "everyone”
# Password= “NeWW1stLst”
# ——————————————————————————

install.packages("RODBC")
library(RODBC)


driver.name <- "SQL Server"
db.name <- "VARS"
host.name <- "dione.mbari.org"
port <- "51001"
server.name <- "dione.mbari.org"
user.name <- "everyone"
pwd <- "NeWW1stLst"

# Use a full connection string to connect
con.text <- paste("DRIVER=", driver.name,
                  ";Database=", db.name,
                  ";Server=", server.name,
                  ";Port=", port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", user.name,
                  ";PWD=", pwd, sep = "")

con <- odbcDriverConnect("Driver=SQL Server;Server=dione.mbari.org:51001;Database=VARS;Uid=everyone;Pwd=NeWW1stLst")

res <- sqlQuery(con1, 'select * from information_schema.tables')

odbcCloseAll()nnection <- odbcDriverConnect("Server=dione.mbari.org;Database=VARS;Uid=everyone; Pwd=NeWW1stLst")

conn = pymssql.connect(server='dione.mbari.org:51001', user='everyone', password='NeWW1stLst', database='VARS')




##### _____ Bringing in MBARI sponge data #####
d <- read.csv("20170314-1_MBARI_Porifera_1997_2015.csv", header = T, sep = ",")
x <- d %>%
  group_by(ObservationID_FK,videoArchiveName) %>%
  summarise(n=n())

View(x)
d1 <- read.csv("20170501-0_MBARI_Porifera_1997_2015.csv", header = T, sep = ",")

# looking at places where have differences
d1[d1$SampleID == 1882988, c("Locality")]
d[d$ObservationID_FK == 1882988, c("Locality")]

# looking at places where don't have differences
d1[d1$SampleID == 914943, c("SampleID", "ScientificName", "ObservationDate", "ObservationTime","VehicleName")]
d[d$ObservationID_FK == 914943, c("ObservationID_FK", "ConceptName", "ObservationDate", "RecordedDate", "RovName")]

names(d)


# looking at matching values
Reduce(intersect, list(d1$SampleID, d$ObservationID_FK))

# how many values in common
length(Reduce(intersect, list(d1$SampleID, d$ObservationID_FK)))

min(d$ObservationID_FK)
max(d$ObservationID_FK)

##### exploring SampleID and ObservationID_FK #####
head(d1$SampleID)
table(d1$SampleID, useNA = "always")

x <- head(d1$SampleID, n = 100)
y <- head(d$ObservationID_FK, n = 100)

x <- as.numeric(d1$SampleID)
y <- as.numeric(d$ObservationID_FK)

sort(as.numeric(x))
sort(as.numeric(y))

setdiff(x,y)
setdiff(y,x)

length(x)
length(y)

head(d$ObservationID_FK)
setdiff(as.numeric(d1$SampleID), as.numeric(d$Observation_FK))
setdiff(d$Observation_FK, d1$SampleID)
length(setdiff(d1$SampleID, d$Observation_FK))

##### preparing to merge  #####
x <- d %>%
  dplyr::select(ObservationID_FK, videoArchiveName, RecordedDate, ObservationDate)

names(x) <- c("SampleID","videoArchiveName", "RecordedDate", "ObservationDate_MBARI")
names(x)

##### rounding time to nearest second #####
x$RecordedDate<-as.POSIXct(x$RecordedDate, "%Y-%m-%d %H:%M:%S")

# stripping off time zone
x$RecordedDate <- substr(x$RecordedDate, 0, 19)
x$RecordedDate <- as.character(x$RecordedDate)

# split Recorded Date and Recorded Time
y <- data.frame(do.call('rbind', strsplit(as.character(x$RecordedDate),' ',fixed=TRUE)))
names(y)

# adding split variables to the original
x$ObservationDate <- y$X1
x$ObservationTime <- y$X2
names(x)

##### merge step #####
# merge
join <- merge(d1, x, by = "SampleID", all.x = T)
names(join)

#Looking at results of join
View(table(factor(join$videoArchiveName), useNA = "always"))
View(table(factor(join$RecordedDate), useNA = "always"))
View(table(factor(join$ObservationDate.y), useNA = "always"))
View(table(factor(join$ObservationTime.y), useNA = "always"))

#checking
length(d1$SampleID) - length(join$SampleID)

##### assigning variables back to the right place
names(join)
join$SampleID <- paste(join$videoArchiveName, join$ObservationTime.y)
join$Modified <- join$ObservationDate.x
join$ObservationDate <- join$ObservationDate.y
join$ObservationTime <- join$ObservationTime.y

##### drop specific uneeded columns
drops <- c("ObservationDate.x",
           "ObservationDate.y",
           "ObservationTime.x",
           "ObservationTime.y",
           "videoArchiveName",
           "RecordedDate",
           "ObservationDate_MBARI")
newdata <- join[ , !(names(join) %in% drops)]

# check
setdiff(names(indata), names(newdata))
setdiff(names(newdata), names(indata))
setdiff(names(newdata), TMPL_FullVariables)
setdiff(TMPL_FullVariables, names(newdata))

# setting ObservationYear
newdata$ObservationYear <- substr(newdata$ObservationDate, 0, 4)
head(newdata$ObservationYear)

# reorder variables
newdata<-newdata[,c(names(indata))]

# add website
newdata$WebSite <- "http://www.mbari.org/products/research-software/video-annotation-and-reference-system-vars/query-interface/"

##### inspect new data #####
x <- newdata %>%
  group_by(DepthInMeters, MinimumDepthInMeters, MaximumDepthInMeters) %>%
  summarise(n=n())
View(x)


##### export new data #####
setwd("C:/rworking/digs/outdata")
write.csv(newdata,"20170427-0_MBARI_Porifera_1997_2015.csv", row.names = F, quote = T)

##### _____read data in from Google Drive #####
id <- "0B9c2c_XdhpFBaXFDbVJkdWotelE" # google file ID
d <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id), header = T)
View(d)


##### _____ MBARI data Country assignment module #####
d <- read.csv("20170427-0_MBARI_Porifera_1997_2015.csv", header = T, sep = ",")

# creating SpatialPointsDataFrame from the data set.
coords<-d[,c("Longitude", "Latitude")]
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)
spdf<-SpatialPointsDataFrame(coords, d,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                             match.ID = TRUE)

# bringing in subset of world eez layer from join operation in ArcGIS
join <- readOGR("C:/data/BaseLayers/scratch", "join")
names(join)
join2 <- join[,c("CatalogNum", "Sovereign1")]
join3 <-  as.data.frame(join2)
names(join3)

join4 <- merge(d, join3, by.x = "CatalogNumber", by.y = "CatalogNum", all.x = T)

names(join4)

join4$Country <- join4$Sovereign1

table(join4$Country, useNA = "always")
table(country$Country, useNA = "always")
country <- join4[,c("CatalogNumber","Country")]

setwd("C:/rworking/digs/outdata")
write.csv(country,"20170501-0_country_assignment_from_World_EEZ_v9_20161021.csv", row.names = F, quote = T)

##### working on RACE Trawl Survey data 2012-2016 #####
x <- indata %>%
  filter(grepl("Trawl survey to provide fisheries", indata$Purpose)) %>%
  group_by(SurveyID, EventID, SampleID, TrackingID, ObservationYear, ObservationDate, DataProvider, Purpose, Latitude, LocationComments) %>%
  summarize(n = n())

View(x)

x <- indata %>%
  filter(grepl("EX14", indata$SurveyID)) %>%
  group_by(SurveyID, Habitat, Substrate) %>%
  summarize(n = n())

View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170501-0_Habitat_Substrate_from_EX.csv", row.names = F, quote = T)

##### MBARI sponge data finalization #####
table(indata$SurveyID, useNA = "always")
table(indata$ImageURL, useNA = "always")
table(indata$PI, useNA = "always")

##### Rachel EX subset #####
x <- indata %>%
  filter(grepl("ex", indata$SurveyID)) %>%
  group_by(SurveyID, Habitat, Substrate) %>%
  summarize(n = n())

View(x)

unique(x$SurveyID)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170503-0_Habitat_Substrate_from_EX_for_Rachel.csv", row.names = F, quote = T)

##### Bringing in John Reed data for inspection #####

"20170503_NF-11-09-CIOERT_Reed_Farrington"

##### Ren inspection of DataProvider names #####
x <- indata %>%
  filter(grepl("Center", indata$DataProvider)) %>%
  group_by(DataProvider) %>%
  summarize(n = n())

View(x)

unique(x$SurveyID)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170503-0_Habitat_Substrate_from_EX_for_Rachel.csv", row.names = F, quote = T)

##### Looking at species issues #####
x <- indata %>%
  filter(grepl("Asterosmilia", indata$ScientificName)) %>%
  group_by(ScientificName, AphiaID) %>%
  summarize(n = n())

View(x)

unique(x$SurveyID)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170503-0_Habitat_Substrate_from_EX_for_Rachel.csv", row.names = F, quote = T)

##### Olympic Coast National Marine Sanctuary subsetting for Nancy Wright #####

x <- indata %>%
  filter(grepl("Asterosmilia", indata$ScientificName)) %>%
  group_by(ScientificName, AphiaID) %>%
  summarize(n = n())

View(x)

unique(x$SurveyID)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170503-0_Habitat_Substrate_from_EX_for_Rachel.csv", row.names = F, quote = T)



x <- indata %>%
  filter(grepl("Olympic", indata$DataProvider) | grepl("Olympic", indata$Locality)) %>%
  group_by() %>%
  summarize(n = n())

View(x)

unique(x$SurveyID)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170503-0_Habitat_Substrate_from_EX_for_Rachel.csv", row.names = F, quote = T)

d <- filter(indata, as.numeric(Latitude) > 47,
                       as.numeric(Latitude) < 49,
                       as.numeric(Longitude) < -124,
                       as.numeric(Longitude) > -126,
            Flag == "0"
                       )

x <- d %>%
  filter(Flag == "0") %>%
  group_by(DataProvider, Repository, SurveyID, PI, Vessel, VehicleName, Locality, ObservationYear) %>%
  summarize(n = n(),
            latitude = mean(Latitude),
            longitude = mean(Longitude))

setwd("C:/rworking/digs/outdata")
write.csv(d,"20170505-1_OCNMS_All_Corals_and_Sponges_from_DSCRTP_NatDB_20170324-0_RPMcGuinn.csv", row.names = F, quote = T)

##### Bringing in direct intersect of OCNMS boundary with current unflaaged records#####
names(d)
d <- read.csv("x.csv", header = T)

x <- d %>%
  filter(grepl("Lophelia", d$Scientific) | grepl("Primnoa", d$Scientific)) %>%
  group_by(CatalogNum, Scientific, Observat_1, Vessel, VehicleNam, Flag, PI, DatasetID) %>%
  summarize(n = n())
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170505-1_OCNMS_Lophelia_and_Primnoa_DSCRTP_NatDB_20170324-0_RPMcGuinn.csv", row.names = F, quote = T)


##### _____ Taxize for filling out taxonomy table #####
##### Resolve a test set using taxize functionality #####
##### create a species list from our data #####
library(taxize)
list <- unique(d$ScientificName)
match <- classification(list, db = "eol")

spc <- c("Anthomastus robustum")
classification(spc, db = "eol")

list <- list[grepl("cf.", list)]
temp <- gnr_resolve(names = list)
match <- classification(list, db = "eol" )

# checking a specific name
x <- gnr_resolve(names = c("Agaricia fragilis contracta"))
table(x$matched_name, x$data_source_title)

##### _____ Getting CRM or ETOPO URL for downloading outside of R #####
##### setting bounding box coordinates #####
minLon <- -72
maxLon <- -65
minLat <- 37
maxLat <- 46
##### get ETOPO1 data from NCEI #####
url <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/",
             "wcs.groovy?filename=etopo1_bedrock.tif&",
             "request=getcoverage&version=1.0.0&service=wcs&",
             "coverage=etopo1_bedrock&CRS=EPSG:4326&format=geotiff&",
             "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",
             minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")

##### get CRM data from NCEI #####
url.hi <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/",
                "wcs.groovy?filename=crm.tif&",
                "request=getcoverage&version=1.0.0&service=wcs&",
                "coverage=crm&CRS=EPSG:4326&format=geotiff&",
                "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",
                minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")

##### _____ MBARI sponge data locality correction #####
d <- read.csv("20170517-0_MBARI_Porifera_1997_2015.csv", header = T, sep = ",")
s <- read.csv("samples.csv", header = T, sep = ",")
names(s) <- c("VehicleName", "EventID", "Locality_s")
s$MergeField <- paste(s$VehicleName,"ROV",s$EventID)
d$MergeField <- paste(d$VehicleName,d$EventID)
s <- s[!duplicated(s), ]
x <- merge(d, s, by="MergeField", all.x = T)
x$Locality <- x$Locality_s
d <- x

setdiff(names(d), names(indata))
setdiff(names(indata), names(d))
d$EventID <- d$EventID.x
d$VehicleName <- d$VehicleName.x
d <- d[,names(indata)]

# checking
names(x)
table(factor(s$Locality_s), useNA = "always")
tail(table(factor(s$MergeField), useNA = "always"))
tail(table(factor(d$MergeField), useNA = "always"))
setdiff(d$CatalogNumber, x$CatalogNumber)
yo <- table(x$CatalogNumber)
yo <- table(d$CatalogNumber)
View(yo)
yo <- x[x$CatalogNumber == "676743",112:116]

setwd("C:/rworking/digs/outdata")
write.csv(d,"20170517-0_MBARI_Porifera_1997_2015.csv", row.names = F, quote = T)










##### _____ Inspection of MCZ DataContact #####
x <- indata %>%
  filter(grepl("Academy", indata$DataProvider)) %>%
  group_by(DataProvider,DataContact) %>%
  summarize(n = n())

View(x)

unique(x$SurveyID)


##### _____ Subsetting by Geography for Lisa Wickliffe Aquaculture Analysis #####
indatafilter <- filter(indata, Flag == "0",
                       as.numeric(DepthInMeters) > 50,
                       as.numeric(DepthInMeters) < 90,
                       # is.na(ImageURL) == F
                       as.numeric(Latitude) > 23,
                       as.numeric(Latitude) < 38,
                       as.numeric(Longitude) < -112,
                       as.numeric(Longitude) > -127
                       )

##### Exporting the data subset as a CSV #####
setwd("C:/rworking/digs/outdata")
write.csv(indatafilter,"20170526_0_Geo_Subset_NatDB_20170324_0_RPMcGuinn.csv", row.names = F, quote = T)

##### exporting a shapefile #####
coordinates(indatafilter) <- c("Longitude", "Latitude")
proj4string(indatafilter) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/rworking/digs/outdata")
writeOGR(indatafilter, dsn="20170526_0_Geo_Subset_NatDB_20170324_0_RPMcGuinn",
         layer= "20170526_0_Geo_Subset_NatDB_20170324_0_RPMcGuinn",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

coordinates(indata) <- c("Longitude", "Latitude")
proj4string(indata) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/rworking/digs/outdata")
writeOGR(indata, dsn="database",
         layer= "database",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)


##### importing a shapefile #####
setwd("C:/rworking/digs/indata")
select <- readOGR(".", "yo")

# getting urls from shapefile selection
urls <- select$ImagURL
urls <- urls[!is.na(urls)]
urls

##### working on exporting all images #####
# getting urls from file
indatafilter <- indata[1:5,]
urls <- indatafilter$ImageURL

setwd("C:/rworking/digs/outdata/imageset")
setwd("C:/rworking/digs/outdata/imageset5k")
setwd("C:/rworking/digs/outdata/imageset_SCB_50-90m")
setwd("C:/rworking/digs/outdata/imageset20k")

for (url in urls) {
  download.file(url, destfile = basename(url), mode = "wb")
}

##### creating a set of the original data that matches the GIS selection #####@
sel <-indatafilter[indatafilter$CatalogNumber %in% select$CtlgNmb,]

##### Exporting the data subset as a CSV #####
setwd("C:/rworking/digs/outdata")
write.csv(sel,"20170526_0_AOI_20k__NatDB_20170324_0_RPMcGuinn.csv", row.names = F, quote = T)

##### _____ How many images in database? #####
table(is.na(indata[indata$Flag == "0",]$ImageURL) == T)
table(is.na(indata[indata$Flag == "0",]$ImageFilePath) == T)

##### which datasets have ImageFilePath but no ImageURL #####
x <- indata %>%
  filter(is.na(ImageFilePath) == F, is.na(indata$ImageURL) == T, Flag == "0") %>%
  group_by(DataProvider, SurveyID, DatasetID, DataContact, Reporter, IdentifiedBy) %>%
  summarise(n=n())
View(x)

##### inspect one such dataset
x <- indata %>%
  filter(is.na(ImageFilePath) == F,
         is.na(indata$ImageURL) == T,
         Flag == "0",
         DatasetID == "MBARI_1989_2012") %>%
  group_by(DataProvider, SurveyID, DatasetID, ImageFilePath, ImageURL) %>%
  summarise(n=n())
View(x)


##### _____ Use Taxize and WoRMS SOAP interface #####
#interface to get new records in the taxonomy table
#install.packages("taxize")
library(taxize)

##### load the taxonomic list of interest #####
setwd("C:/rworking/digs/indata")
list <- read.csv("list.csv", header = T)
tax <- read.csv("20170622-0_taxonomy_RPMcGuinn.csv", header = T)
taxch <- read.csv("20170622-0_taxa_to_change_RPMcGuinn.csv", header = T)
taxfl <- read.csv("20170622-0_taxa_to_flag_RPMcGuinn.csv", header = T)

##### test list against tax #####
setdiff(list, tax$ScientificName)
setdiff(list, taxch$ScientificName)
setdiff(list, taxfl$ScientificName)

##### looking for things in the taxonomy tables #####
x<-5
diff <- tax[grepl(list$ScientificName[x], tax$ScientificName) |
              grepl(list$ScientificName[x+1], tax$ScientificName) |
              grepl(list$ScientificName[x+2], tax$ScientificName) |
              grepl(list$ScientificName[x+3], tax$ScientificName) |
              grepl(list$ScientificName[x+4], tax$ScientificName), c("ScientificName")]
diff

x<-"Hamacantha (Vomerula)"
tax[grepl(x, tax$ScientificName), c("ScientificName")]
taxfl[grepl(x, taxfl$ScientificName), c("ScientificName", "FlagReason")]
taxch[grepl(x, taxch$VerbatimScientificName), c("VerbatimScientificName","ScientificName")]


##### get the rest of the taxonomic string from worms #####
list <- unique(list$ScientificName)
wid <- get_wormsid(query = list, searchterm = list,
                   searchtype = "scientific",
                   accepted = T, verbose = F)

##### get full classification #####
class_list <- classification(wid, db = "eol")

##### create a clean data frame #####
list2env(class_list,envir=.GlobalEnv)

# getting rid of na entries in list
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
# class_list <- na.omit.list(class_list)
df <- data.frame(plyr::ldply(class_list, function(x) t(data.frame(x))))
df <- df[is.na(df$X1) == F,]
df <- df[df$X1 != "Kingdom",]
df <- df[grep("[[:digit:]]", invert = TRUE, df$X1), ]
View(df)
names(df) <- c("AphiaID",
               "Kingdom",
               "Phylum",
               "Class",
               "Subclass",
               "Order",
               "Suborder",
               "Family",
               "Genus",
               "ScientificName")
##### splitting ScientificName to get specific epithet #####
yo <- str_split_fixed(df$ScientificName, " ", 2)
yo <- data.frame(yo)
df$Species <- yo$X2

write.csv()

##### _____ Working with Worms SOAP Interface to return ScientificNameAuthorship  #####

##### Set up and installation of Worms SOAP #####
install.packages("XML", repos = "http://cran.r-project.org", dependencies = TRUE)
download.file("http://www.omegahat.net/Prerelease/XMLSchema_0.8-0.tar.gz", "XMLSchema")
install.packages("XMLSchema", type="source", repos = NULL)
download.file("http://www.omegahat.net/Prerelease/SSOAP_0.91-0.tar.gz", "SSOAP")
install.packages("SSOAP", type="source", repos = NULL)
library(SSOAP)
w = processWSDL("http://www.marinespecies.org/aphia.php?p=soap&wsdl=1")
iface = genSOAPClientInterface(, w)
# AphiaID = iface@functions$getAphiaID("Solea solea",1,('http://www.marinespecies.org/aphia.php?p=soap'))
# print(AphiaID)
r#should output '[0] 127160'

##### Define AphiaMatch function #####

AphiaMatch <- function(x) {
  result<-NULL
  for (i in 1:length(x)) {
    AphiaRecord <- iface@functions$getAphiaID(x[i],1,('http://www.marinespecies.org/aphia.php?p=soap'))
    result<-c(result, AphiaRecord)
  }
  return(result)
}

##### Define getFullRecord function to get full classification string #####

getFullRecord <- function(x) {
  result<-NULL
  for (i in 1:length(x)) {
    AphiaRecord <- iface@functions$getAphiaRecordByID(x[i],('http://www.marinespecies.org/aphia.php?p=soap'))
    slotnames <- slotNames(AphiaRecord)
    slotlist <- data.frame(rbind(1:length(slotnames)))
    names(slotlist) <- slotnames
    for(y in slotnames) {
      #R cannot handle a slot name "class"
      if (y == "CLASS") {slotlist[1,y] <- '(empty)'}
      else {slotlist[1, y] <- slot(AphiaRecord,  y)}
    }
    result<-rbind(result, slotlist)
  }
  return(result)
}

##### Define SynResolv funcion to get accepted AphiaID's #####
SynResolv <- function(x) {
  result<-NULL
  for (i in 1:length(x)) {
    AphiaRecord <- iface@functions$getAphiaRecordByID(x[i],('http://www.marinespecies.org/aphia.php?p=soap'))
    result<-c(result, slot(AphiaRecord, "valid_AphiaID"))
  }
  return(result)
}

##### Creating a new dataframe from species list #####
class <- data.frame(list)
names(class) <- c("VerbatimScientificName")
class

##### Using the AphiaMatch function to match original AphiaIDs from names #####
class$OrigTaxID <- AphiaMatch(class$VerbatimScientificName)
View(class)

##### Filtering class df to include only species that had a match #####
unmatched <- class %>%
  filter(is.na(OrigTaxID) == T | OrigTaxID == "-999")

class <- class %>%
  filter(is.na(OrigTaxID) == F, OrigTaxID != "-999")

##### Returning the accepted AphiaIDs using the SynResolv function #####
class$AccTaxID<-SynResolv(class$OrigTaxID)
class <- class %>%
  filter(AccTaxID > 0, is.na(AccTaxID) == F)
class

##### Returning and binding the full classifcation to the matched names #####
AphiaRecords<-getFullRecord(class$AccTaxID)
class<-cbind(class, AphiaRecords)
View(class)
class$url

##### _____ Binding new entries to the existing taxonomic table #####
setwd("C:/rworking/digs/indata")
tax <- read.csv("20170608-1_taxonomy.csv", header=T, na.strings=c("","NA"))

# creating an empty tax table to populate
newtax <- tax[0,]
View(newtax)

#adding enough empty rows
newtax[1:length(class$VerbatimScientificName),] <- NA

# creating a matching newtax table
newtax$ScientificName <- class$valid_name
newtax$AphiaID <- class$AphiaID
newtax$Phylum <- class$phylum
newtax$Order <- class$order
newtax$Family <- class$family
newtax$Genus <- class$genus
newtax$ScientificNameAuthorship <- class$valid_authority
newtax$TaxonRank <- class$rank
View(newtax)

length(names(tax))
length(names(newtax))
setdiff(names(tax), names(newtax))

# combine new taxa with original file
tax2<-rbind(tax,newtax)

# exporting the new joint taxonomy table
setwd("C:/rworking/digs/outdata")
write.csv(tax2,"20170616-0_taxonomy_RPMcGuinn.csv", row.names = F, quote = T)

##### _____ Michelle Bachman date issues #####
##### bring in data (shapefile) #####
setwd("C:/data/Baselayers/New_England_Michelle_Bachman")
select <- readOGR(".", "151130_DSCRTP_subset")
names(select)
##### strip CatalogNumbers #####
cat <- select$CatalogNum
cat

##### get all records with matching CatalogNumbers from current NatDB ####
d <- indata[indata$CatalogNumber %in% cat, ]
x <- d[,c("CatalogNumber","ObservationDate", "ObservationYear")]
View(x)

##### Explore the records with missing ObservationYear
table(x$ObservationYear, useNA = "always")
x <- d %>%
  filter(ObservationYear == "-999")

table(factor(x$DataProvider),factor(x$Vessel))

y <- x %>%
  group_by(CatalogNumber, ScientificName, Latitude, Longitude, DepthInMeters, gisCRMDepth, gisGEBCODepth, DataProvider, PI, DataContact,
           Repository, Vessel, VehicleName, RecordType, SurveyID, EventID, SampleID, Station, ObservationYear, ObservationDate) %>%
  dplyr::summarize(n_records = n(),
            minyear = min(as.numeric(ObservationYear)),
            maxyear = max(as.numeric(ObservationYear))
  )
View(y)

setwd("C:/rworking/digs/outdata")
write.csv(y,"check.csv", row.names = F, quote = T)

##### viewing a merge #####
join <- merge(x, select, by.x = c("CatalogNumber"), by.y = c("CatalogNum"), keep = "all.x")
View(join)
View(join[,c("CatalogNumber", "ObservationDate", "ObservationYear", "Observatio")])

##### exporting selected records to shapefile #####
d <- d %>%
  filter(Flag == "0")
coordinates(d) <- c("Longitude", "Latitude")
proj4string(d) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/rworking/digs/outdata")
writeOGR(d, dsn="d",
         layer= "d",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### writing to table as well ####
setwd("C:/rworking/digs/outdata")
write.csv(d,"20170608_0_Subset_for_MB_NatDB_20170324_0_RPMcGuinn.csv", row.names = F, quote = T)

#####_____ Southeast ROV Surveys map #####

##### bring in data (shapefile) #####
setwd("C:/data/Baselayers/SoutheastDatabase")
rovdb <- readOGR(".", "ROV_SE_060917")
names(rovdb)
proj4string(rovdb)

##### create ObservationYear Variable #####
table(rovdb$Date_, useNA = "always")
rovdb$ObservationYear <- substr(rovdb$Date_, start = 1, stop = 4)

##### subset to 2007-2016 #####
rovdb_2007_2016 <- data.frame(rovdb) %>%
  filter(as.numeric(ObservationYear) > 2006)

##### write new shapefile from slection #####
coordinates(rovdb_2007_2016) <- c("Start_Long", "Start_Lati")
proj4string(rovdb_2007_2016) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

setwd("C:/data/Baselayers/SoutheastDatabase")
writeOGR(rovdb_2007_2016, dsn="rovdb_2007_2016",
         layer= "rovdb_2007_2016",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### _____ 20170515-0_Getting datasets for Janessy F. #####

x<-unique(indata[grepl("Okeanos", indata$Vessel), c("Vessel", "SurveyID", "DataProvider")])
View(x)
unique(d[grepl("Velero", d$Vessel), c("Vessel")])
unique(indata$Vessel)
head(indata$Vessel)
table(unique(indata$NavType))
unique(indata$DataProvider)
class(x)
x<-data.frame(x)

x <- indata %>%
  filter(Flag == "0", grepl("Okeanos", indata$Vessel) | grepl("Falkor", indata$Vessel))  %>%
  group_by(DataContact, DataProvider, Locality, Vessel, VehicleName, ObservationYear, Citation, Repository, DatasetID, SurveyID) %>%
  summarize(n_records = n(),
            flagsum = length(Flag[Flag == "1"]),
            minyear = min(as.numeric(ObservationYear)),
            maxyear = max(as.numeric(ObservationYear)),
            Latitude = mean(Latitude),
            Longitude = mean(Longitude)
  )
View(x)
x<-data.frame(x)

# #checking
# unique(x[,c("SurveyID", "DatasetID")])

##### Subset by Geography #####
x <- filter(x, as.numeric(Latitude) > 22,
                       as.numeric(Latitude) < 30,
                       as.numeric(Longitude) < -82,
                       as.numeric(Longitude) > -94)


##### make a shapefile for inspection #####
coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### writing the spefic subset to CSV file #####
x <- indata %>%
  filter(Flag == "0", grepl("Okeanos", indata$Vessel) | grepl("Falkor", indata$Vessel))
x <- data.frame(x)
x <- filter(x, as.numeric(Latitude) > 20,
              as.numeric(Latitude) < 35,
              as.numeric(Longitude) < -80,
              as.numeric(Longitude) > -98)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170615-0_GOMEX_Falkor_Okeanos_subset_NatDB_20170324-0_RPMcGuinn.csv", row.names = F, quote = T)

##### making a shapefile subset #####
##### make a shapefile for inspection #####
x <- dplyr::select(x, DepthInMeters, ImageURL,Latitude, Longitude, Vessel, SurveyID, ObservationYear, DatasetID)
coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### _____ Working on datasets #####
setwd("C:/rworking/digs/indata")
d <- read.csv("20170608-0_MCZ_MuseumSpec_Southeast_Wagner_1869_2017.csv", header = T)
list <- d$ScientificName
table(d$DataProvider)
table(d$Reporter)
table(indata$DataProvider)
table(d$ObservationDate)

# separate checks
table(d$VehicleName, useNA = "always")
table(d$IdentifiedBy, useNA = "always")
table(d$IdentificationQualifier, useNA = "aways")
table(d$IndividualCount, useNA = "always")
table(d$SampleID, useNA = "always")

# taxonomic checks
table(d$Phylum, useNA = "always")
table(d$Class, useNA = "always")
table(d$Order, useNA = "always")

y <- unique(d[is.na(d$Phylum) == T, c("Flag", "FlagReason", "ScientificName")])
list <- y$ScientificName
wid <- get_wormsid(query = list, searchterm = list,
                   searchtype = "scientific",
                   accepted = T, verbose = F)
class_list <- classification(wid, db = "eol")
class_list
x <- split(df, df$.id)

##### create a clean data frame #####
list2env(class_list,envir=.GlobalEnv)

df <- plyr::ldply(class_list, data.frame)
df
df <- melt(df)
View(df)
df.t <- t(df)
df.t <- as.data.frame(df.t)

x <- plyr::dlply(df, df$.id)
x

x <- "Iphiteon compressa"
tax[grepl(x, tax$ScientificName), c("ScientificName")]
taxfl[grepl(x, taxfl$ScientificName), c("ScientificName", "FlagReason")]
taxch[grepl(x, taxch$VerbatimScientificName), c("VerbatimScientificName","ScientificName")]

setwd("C:/rworking/digs/outdata")
write.csv(list,"list.csv", row.names = F, quote = T)

list

##### _____ Working on new accessions #####
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20170324-0.csv", header = T)
d <- read.csv("20170615-0_HBOI_FLOSEE_L2_NancyFoster_JohnReed_2011_2011.csv", header = T)

x <- d %>%
  filter(grepl("American", d$DataProvider))  %>%
  group_by(Flag, FlagReason, DataProvider, Purpose, SurveyID, DataContact) %>%
  summarise(n=n())
View(x)

unique(indata$IdentificationQualifier)
table(d$SamplingEquipment, useNA = "always")

##### _____ dealing with Yeckly set #####
setwd("C:/rworking/digs/indata")
d<-read.csv("d.csv", header = T)
names(d)

setwd("C:/rworking/digs/indata")
eventid<-read.csv("eventid.csv", header = T)
names(eventid)

eventid


d$ScientificName <- as.character(d$ScientificName)
s <- strsplit(d$ScientificName, split = ",")
df <- data.frame(EventID = rep(eventid$EventID, sapply(s, length)), ScientificName = unlist(s))

# Here is an example of how this is supposed to work.
df <- read.table(textConnection("1|a,b,c\n2|a,c\n3|b,d\n4|e,f"), header = F, sep = "|", stringsAsFactors = F)

s <- strsplit(df$V2, split = ",")
data.frame(V1 = rep(df$V1, sapply(s, length)), V2 = unlist(s))

##### or #####

df <- d %>%
  mutate(ScientificName = strsplit(as.character(ScientificName), ",")) %>%
  unnest(ScientificName)

setwd("C:/rworking/digs/outdata")
write.csv(df,"20170626-0_DSCRTP_NatDB_Submission_Sedberry_24Feb2017.csv", row.names = F, quote = T)


##### _____ Getting data for Peter Etnoyer #####
x <- indata %>%
  filter(grepl("Georgian", indata$Citation) | grepl("Georgian", indata$DataProvider) | grepl("Georgian", indata$DataContact),
                 as.numeric(ObservationYear) > 2004, Genus == "Leiopathes", Flag == "0")  %>%
  group_by(Flag, ObservationYear, DatasetID, DataProvider,DataContact, Reporter, SurveyID, Citation, Vessel, VehicleName, Purpose) %>%
  summarise(n=n())
View(x)

##### write output as CSV
setwd("C:/rworking/digs/outdata")
write.csv(x,"20170626-0_Sam_Georgian_Subset_of_DSCRTP_NatDB_20170324-0_RPMcGuinn.csv", row.names = F, quote = T)



coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="georgian_data",
         layer= "georgian",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)


##### _____ Looking at depth issue with Lophelia Data #####

x <- indata %>%
  filter(CatalogNumber ==  "614241") %>%
  dplyr::select(Flag, FlagReason, DepthMethod, DepthInMeters, gisEtopoDepth, gisGEBCODepth, gisCRMDepth, ObservationDate, SampleID, TrackingID, CatalogNumber, ObservationTime)

names(indata)

##### _____ Looking at Christopher Kelly data #####
x <- indata %>%
  filter(grepl("Kelley", indata$PI), grepl("Okeanos", indata$Vessel)) %>%
  group_by(DatasetID, AccessionID, SurveyID, Vessel, VehicleName, PI, DataContact, Reporter, EntryDate) %>%
  summarise(n=n())

View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170706-0_Kelley_Subset_Summary_from_DSCRTP_NatDB_20170421-0.csv", row.names = F, quote = T)

##### map it data using leaflet #####
#install.packages("leaflet")
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("SurveyID:", x$SurveyID, "<br>",
                                            "DatasetID:", x$DatasetID, "<br>",
                                            "ImageURL:", x$ImageURL, "<br>",
                                            "PI:", x$PI, "<br>",
                                    "Observation Year:", x$ObservationYear))
m


##### _____ Working on gis locality issues#####
x <- indata %>%
  filter(grepl("Kelley", indata$PI), grepl("Okeanos", indata$Vessel)) %>%
  group_by(Locality,gisNGIALocality,gisGEBCOLocality) %>%
  summarise(n=n())

View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20170706-0_Kelley_Subset_Summary_from_DSCRTP_NatDB_20170421-0.csv", row.names = F, quote = T)

# checking: table(indata$gisNGIALocality, useNA = "always")
# all gis based localities are missing, need to recreate.


tax$ScientificName[grepl("Hydroidolina", tax$ScientificName)]

##### Flagged Leiopathes in GOMEX #####

x <- filter(indata, as.numeric(Latitude) > 18.1509,
                       as.numeric(Latitude) < 30.40613,
                       as.numeric(Longitude) < -80.45932,
                       as.numeric(Longitude) > -97.858208)


setwd("C:/rworking/digs/outdata")
write.csv(x,"20170711-0_Flagged_Leiopathes_GOMEX_subset_from_DSCRTP_NatDB_20170608-0.csv", row.names = F, quote = T)

##### _____ Depth Check for Gulf of mexico records #####
##### filtering data #####
# GOMEX Filter
d <- filter(indata, as.numeric(Latitude) > 18.1509,
            as.numeric(Latitude) < 30.40613,
            as.numeric(Longitude) < -80.45932,
            as.numeric(Longitude) > -97.858208)

d <- filter(indata, as.numeric(Latitude) > 18,
            as.numeric(Latitude) < 23,
            as.numeric(Longitude) < -153,
            as.numeric(Longitude) > -160)

d <- filter(indata, ScientificName == "Corallium regale")

# checking
# table(is.na(d$gisEtopoDepth), useNA = "always")
# length(d$gisEtopoDepth == "-999")
#
# head(d$gisCRMDepth, n=10)
# head(d$gisGEBCODepth, n=10)
# head(d$gisCRMDepth, n=10)

##### setting thresholds for difference and creating flags#####
t <- 1200
##### building selection conditions for each depth value #####
#finding depth difference for CRM
d$CRMDepthDiff <- abs(d$DepthInMeters - d$gisCRMDepth)
#crm depth flags
crm <- d %>%
  filter(CRMDepthDiff > t & gisCRMDepth != "-999") %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)
#finding depth difference for Etopo
d$EtopoDepthDiff <- abs(d$DepthInMeters - d$gisEtopoDepth)
#etopo depth flags
etopo <- d %>%
  filter(EtopoDepthDiff > t & gisEtopoDepth != "-999") %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)
#finding depth difference for GEBCO
d$GEBCODepthDiff <- abs(d$DepthInMeters - d$gisGEBCODepth)

#GEBCO depth flags
gebco <- d %>%
  filter(GEBCODepthDiff > t & gisGEBCODepth != "-999") %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)
#depth flags from reported values
depth <- d %>%
  filter(as.numeric(DepthInMeters) == -999 | as.numeric(DepthInMeters) < 30) %>%
  dplyr::select(CatalogNumber, DatasetID, SampleID, TrackingID, ScientificName, Locality, DepthInMeters, gisEtopoDepth,
                gisGEBCODepth, gisCRMDepth, MinimumDepthInMeters, MaximumDepthInMeters,
                NavType, LocationAccuracy, LocationComments, Flag, FlagReason, SurveyID,
                Vessel, VehicleName, ObservationYear, Latitude, Longitude)


##### building joint file of all depth probs and de-dup
dx <- rbind(crm,etopo,gebco,depth)

##### testing for duplicated records #####
dx <- dx[!duplicated(dx), ]

#checking
length(dx$CatalogNumber)

##### removing flagged records if wanted #####
#dx <- dx[dx$Flag == "0",]

##### Plotting results of selections for each GIS assigned value #####
p <- ggplot(dx, aes(DepthInMeters, gisEtopoDepth))
p +
  geom_point(aes(shape = factor(Flag), size = as.numeric(Flag)), size = 5) +
  scale_shape_manual(values=c(1,4)) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(col = "gray60")

##### make a shapefile with the OGR driver #####
coordinates(dx) <- c("Longitude", "Latitude")
proj4string(dx) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(dx, dsn="dx",
         layer= "dx",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### making a KML file of the results #####

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points
setwd("C:/rworking/digs/outdata")
writeOGR(dx, dsn="dx.kml", layer= "DSC_RTP", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

## if you have Google Earth installed double click on the kml file you just created to open it.
#The points should be loaded as labelled pins on the map.If you click on the pin you will be able to see its full name and capacity.

##### export layer #####
setwd("C:/rworking/digs/outdata")
write.csv(dx,"dx.csv", row.names = F, quote = T)

##### making a leaflet map of the results #####


library(leaflet)
x <- indata %>%
  filter(grepl("Gulf of Mexico", FishCouncilRegion), Flag == "0") %>%
  select(CatalogNumber, SampleID, ScientificName,
         IndividualCount, Vessel, VehicleName, Latitude, Longitude, ObservationYear, DepthInMeters, gisEtopoDepth, ImageURL)

m <- leaflet()
m <- addProviderTiles(m, "CartoDB.DarkMatter") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=7,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("Scientific Name:", x$ScientificName, "<br>",
                                    "CRM:", x$gisCRMDepth, "<br>",
                                    "GEBCO:", x$gisGEBCODepth, "<br>",
                                    "ETOPO1:", x$gisEtopoDepth, "<br>",
                                    "DepthInMeters:", x$DepthInMeters,"<br>",
                                    "Locality:", x$Locality, "<br>",
                                    "Latitude:", x$Latitude, "<br>",
                                    "Longitude:", x$Longitude, "<br>",
                                    "Dataset ID:", x$DatasetID, "<br>",
                                    "Survey ID:", x$SurveyID, "<br>",
                                    "CatalogNumber:", x$CatalogNumber, "<br>",
                                    "Vessel:", x$Vessel, "<br>",
                                    "SampleID:", x$SampleID, "<br>",
                                    "ObservationYear:", x$ObservationYear, "<br>",
                                    "Flag:", x$Flag, "<br>",
                                    "Flag Reason:", x$FlagReason))
m <- addCircleMarkers(m, data=d,
                      radius=4,
                      weight=0,
                      fillColor= "black",
                      fillOpacity=1,
                      popup = paste("Scientific Name:", dx$ScientificName, "<br>",
                                    "CRM:", dx$gisCRMDepth, "<br>",
                                    "GEBCO:", dx$gisGEBCODepth, "<br>",
                                    "ETOPO1:", dx$gisEtopoDepth, "<br>",
                                    "DepthInMeters:", dx$DepthInMeters,"<br>",
                                    "Locality:", dx$Locality, "<br>",
                                    "Latitude:", dx$Latitude, "<br>",
                                    "Longitude:", dx$Longitude, "<br>",
                                    "Dataset ID:", dx$DatasetID, "<br>",
                                    "Survey ID:", dx$SurveyID, "<br>",
                                    "CatalogNumber:", dx$CatalogNumber, "<br>",
                                    "Vessel:", dx$Vessel, "<br>",
                                    "SampleID:", dx$SampleID, "<br>",
                                    "ObservationYear:", dx$ObservationYear, "<br>",
                                    "Flag:", dx$Flag, "<br>",
                                    "Flag Reason:", dx$FlagReason))

m <- addCircleMarkers(m, data=gebco,
                      radius=4,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = gebco$DepthInMeters
)
m <- addCircleMarkers(m, data=crm,
                      radius=2,
                      weight=0,
                      fillColor= "blue",
                      fillOpacity=1,
                      popup = crm$DepthInMeters
)
m

##### Exporting a final set for consideration #####

p <- ggplot(etopo, aes(DepthInMeters, gisCRMDepth))
p +
  geom_point(aes(shape = factor(Flag), size = as.numeric(Flag)), size = 5) +
  scale_shape_manual(values=c(1,4)) +
  geom_point(aes(colour = factor(CRMFlagDepth)), size = 2) +
  scale_color_manual(values=c("#3b2c68", "#f2070b")) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(col = "gray60")# +
#geom_label_repel(aes(label=CatalogNumber))

coordinates(depth)<- c("Longitude", "Latitude")
proj4string(depth)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(depth, dsn="yo.shp",
         layer= "DSC_RTP",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)




##### Fixing the Dates #####
df = data.frame(d$SampleID, d1$SampleID, d$ObservationDate, d1$ObservationDate)
View(df)

names(df)
df$d.ObservationDate <- as.Date(df$d.ObservationDate)
df$d1.ObservationDate <- as.Date(df$d1.ObservationDate)
table(df$d.ObservationDate - df$d1.ObservationDate)
df$diff <- df$d.ObservationDate - df$d1.ObservationDate
x <- df %>%
  filter(diff == "0")
View(x)

x <- df %>%
  filter(diff == "1462")
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### Exploring data that has zero in DepthInMeters #####
x <- indata %>%
  filter(grepl("Olympic", indata$DataProvider), DepthInMeters == "0")  %>%
  group_by(Flag, DataContact, DepthInMeters) %>%
  summarise(n=n())

y <- indata %>%
  filter(DepthInMeters == "0")  %>%
  group_by(Flag, DataContact, DepthInMeters) %>%
  summarise(n=n())

length(y$Flag)
View(y)

z <- indata %>%
  filter(DepthInMeters == "0") %>%
  dplyr::select(CatalogNumber, DepthInMeters)
length(z$CatalogNumber)
#[1] 392

setwd("C:/rworking/digs/outdata")
write.csv(z,"z.csv", row.names = F, quote = T)



## "Antrim, Liam: liam.antrim@noaa.gov; Research Coordinator: olympiccoast@noaa.gov"

##### schema work #####
d <- read.csv("x.csv", header = T)
d <- d[,1:18]
d$row <- rownames(d)

#View(d)

#row.names(d)
# # checking
# names(d)
# d$FieldName

d1 <- read.csv("rd.csv", header = T)

# setdiff(d$FieldName, d1$Field)
# setdiff(d1$Field, d$FieldName)
# length(d1$Field) + 76
# length(d$FieldName)

#merge
m <- merge(d, d1, by.x = "FieldName", by.y = "Field", sort = T, all.x = T)
m <-m[order(as.numeric(m$row)),]

# View(m)
# fieldlist <- m[,c("FieldName", "row")]

setwd("C:/rworking/digs/outdata")
write.csv(m,"m.csv", row.names = F, quote = T)
write.csv(fieldlist,"fieldlist.csv", row.names = F, quote = T)





##### looking at VehicleName #####
table(factor(d$VehicleName), useNA = "always")

x <- indata %>%
  filter(grepl("Delta", indata$VehicleName))  %>%
  group_by(Vessel, VehicleName, ObservationYear) %>%
  summarise(n=n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]))
View(x)

##### _____ Depth Summaries #####
##### filter data #####
indatafilter <- indata  %>%
  filter(Flag == "0", DepthInMeters != "-999", Genus == "Lophelia" |
           Genus == "Oculina" |
           Genus == "Solenosmilia" |
           Genus == "Enallopsammia",
         as.numeric(DepthInMeters) > 0,
         as.numeric(DepthInMeters) <= 6000,
         as.numeric(Longitude) > -81,
         as.numeric(Longitude) < -50,
         as.numeric(Latitude) > 22,
         as.numeric(Latitude) < 43
         )


##### boxplots in lattice #####
library(lattice)
bwplot(~as.numeric(DepthInMeters)| FishCouncilRegion, data=filt,
       type="percent",
       xlab="depth (meters)",
       #ylim=c(0,60),
       xlim=c(0,3000),
       main="Depth Distribution,\nby Genus",
       breaks=seq(from=0,to=3000,by=50),layout=c(1,4))

##### export layer #####
setwd("C:/rworking/digs/outdata")
write.csv(indatafilter,"x.csv", row.names = F, quote = T)

##### make a shapefile with the OGR driver #####
coordinates(indatafilter) <- c("Longitude", "Latitude")
proj4string(indatafilter) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(indatafilter, dsn="indatafilter",
         layer= "indatafilter",
         driver = "ESRI Shapefile",
         overwrite_layer = T)



##### ordering levels changes order of graph drawing in lattice #####
indata$VernacularNameCategory <-
  factor(indata$VernacularNameCategory,
         levels=c("demosponge", "glass sponge",
                  "calcareous sponge","sponge (unspecified)","sea pen","soft coral","lace coral",
                  "stoloniferan coral","lithotelestid coral", "stony coral (cup coral)",
                  "stony coral (unspecified)", "stony coral (branching)", "black coral",
                  "gorgonian coral"), order=TRUE)
table(filt$VernacularNameCategory)

##### histogram in lattice package #####
histogram(~as.numeric(DepthInMeters)| VernacularNameCategory, data=filt,
          type="percent",
          xlab="depth (meters)",
          ylim=c(0,60),
          xlim=c(0,6000),
          main="Depth Distribution,\nby Vernacular Name Category",
          breaks=seq(from=0,to=6000,by=50),layout=c(1,4))


##### kernel density plots by factor level (alternate layout)  #####

densityplot(~DepthInMeters|VernacularNameCategory, data = filt,
            main="Density Plot by Region",
            xlab="Depth (m)", layout=c(1,10)
)


##### Boxplot of depth by Family #####

qplot(factor(VernacularNameCategory),as.numeric(DepthInMeters), data=indata, geom=c("boxplot"),alpha=I(1),
      #qplot(as.numeric(DepthInMeters), data=indata, geom=c("boxplot"),alpha=I(1),
      main="Depth Distribution by Genus",
      xlab="Genus", ylab="Depth (meters)", ylim=c(0,3000)) + coord_flip() +
  theme(axis.text.x=element_text(size=9,angle=90,hjust=1,vjust=0.05),
        axis.text.y=element_text(size=9))

##### Summary Tables by Genus #####
### %>% pipe operator
sum_tbl <-
  indatafilter %>%
  #filter(Phylum == "Porifera") %>%
  group_by(Genus) %>%
  summarize(
    min.Depth = min(as.numeric(DepthInMeters)),
    max.Depth = max(as.numeric(DepthInMeters)),
    mean.Depth = mean(as.numeric(DepthInMeters)),
    mean.DepthGIS = mean(as.numeric(gisEtopoDepth)),
    n = n()) %>%
  arrange(desc(FishCouncilRegion))
sum_tbl

setwd("C:/rworking/digs/outdata")
write.csv(sum_tbl,"x.csv", row.names = F, quote = T)

##### setting up depth categories #####
indata$DepthCat[indata$DepthInMeters > 1000] <- ">1000 meters"
indata$DepthCat[indata$DepthInMeters > 50
                & indata$DepthInMeters <= 200] <- "50-200 meters"
indata$DepthCat[indata$DepthInMeters > 200
                & indata$DepthInMeters <= 1000] <- "201-1000 meters"

##### Summarizing number of Genera grouped by gisMEOW and DepthCat #####
sum_tbl <-
  indata %>%
  group_by(gisMEOW, DepthCat) %>%
  summarize(
    diversity = length(unique(na.omit(Genus))),
    n = n()
  )
sum_tbl

View(sum_tbl)

setwd("C:/rworking/digs/outdata")
write.csv(sum_tbl,"yo.csv", row.names = F, quote = T)

##### Porifera vs. Cnidaria #####

sum_tbl <-
  indata %>%
  filter(Flag == "0") %>%
  group_by(FishCouncilRegion) %>%
  summarize(
    min.Depth = min(as.numeric(DepthInMeters)),
    max.Depth = max(as.numeric(DepthInMeters)),
    mean.Depth = mean(as.numeric(DepthInMeters)),
    mean.DepthGIS = mean(as.numeric(gisEtopoDepth)),
    n = n())
sum_tbl

##### _____ NOAA, Northwest Fisheries Science Center NWFSC #####
x <- d %>%
  filter(grepl("Insufficient", d$FlagReason))%>%
  group_by(ScientificName, VernacularName) %>%
  summarise(n=n())
x

unique(d$ImageFilePath)
table(d$SamplingEquipment)

##### _____ Exporting dataset metadata Leiopathes, etc.#####
x <- indata %>%
  filter(SurveyID == "Lophelia II 2009 Deepwater Coral Expedition" | SurveyID == "RB-09-05"
         | SurveyID == "LOPHELIA II" | SurveyID == "USGS Discovre GOM 2009", grepl("Leiopathes", indata$ScientificName),
         duplicated([,c("Latitude", "Longitude")]))  %>%
  group_by(DataProvider, DatasetID, AccessionID, SurveyID, ScientificName) %>%
  summarize(n = n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])))
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"QA_Subset_of_DSCRTP_NatDB_20170807-1.csv", row.names = F, quote = T)

y <- duplicated(x[,c("Latitude", "Longitude")])

##### make a shapefile with the OGR driver #####
library(rgdal)
coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### _____ Exploring FloSEE #####
x <- indata %>%
  filter(grepl("SEE", indata$SurveyID), Genus == "Leiopathes")  %>%
  group_by(DataProvider, DataContact, DatasetID, AccessionID, SurveyID, ScientificName, EntryDate) %>%
  summarize(n = n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])))
View(x)


##### Exploring black corals #####

setwd("C:/rworking/digs/outdata")
write.csv(x,"20171002-1_QA_Subset_of_DSCRTP_NatDB_20170807-1.csv", row.names = F, quote = T)

x <- indata %>%
  filter(Order == "Antipatharia", Flag == "0",FishCouncilRegion == "South Atlantic") %>%
  group_by(DataProvider, DataContact, DatasetID, AccessionID, SurveyID, ScientificName, EntryDate) %>%
  summarize(n = n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            MinDepth = min(as.numeric(DepthInMeters)),
            MaxDepth = max(as.numeric(DepthInMeters)))
View(x)

names(indata)


##### Exploring query results from Peter #####

x <- d %>%
  group_by(DataProvider, SurveyID, ScientificName) %>%
  summarize(n = n(),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            MinDepth = min(as.numeric(DepthInMeters)),
            MaxDepth = max(as.numeric(DepthInMeters)))
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20171004-1_Summary_QA_Subset_of_DSCRTP_NatDB_20170724-0.csv", row.names = F, quote = T)

##### Summary of New Datasets ####

x <- indata %>%
  filter(Flag == "0", grepl("Alaska", indata$DataProvider), ObservationYear == "2003") %>%
  group_by(DatasetID) %>%
  summarise(n = n(),
            SurveyIDUnique = toString(unique(SurveyID)),
            SurveyIDNumber = length(unique(SurveyID)),
            VesselUnique = toString(unique(Vessel)),
            VesselNumber = length(unique(Vessel)),
            DataProviderUnique = toString(unique(DataProvider)),
            DataProviderNumber = length(unique(DataProvider)),
            GenusUnique = toString(unique(Genus)),
            GenusNumber = length(unique(Genus)),
            AccessionIDUnique = toString(unique(AccessionID)),
            AccessionIDNumber = length(unique(AccessionID)),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            MinDepth = min(as.numeric(DepthInMeters)),
            MaxDepth = max(as.numeric(DepthInMeters)))
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)
length(unique(indata$Genus))

##### creating convex hull around scientificname #####
x <- indata %>%
  filter(Flag == "0", Genus == "Lophelia") %>%
  dplyr::select(ScientificName, Latitude, Longitude, IndividualCount)


coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)
length(unique(indata$Genus))

install.packages("leaflet.extras")
library(leaflet.extras)
m <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addWebGLHeatmap(data=data.frame(lat=x$Latitude, lng=x$Longitude), size = 60000)
m


require(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addCircleMarkers(m, data=data.frame(lat=x$Latitude, lng=x$Longitude), radius=3, weight=0, fillColor=colors, fillOpacity=0.5)
m

##### Run a bunch of RMD reports on groups of data #####
# reference: http://www.reed.edu/data-at-reed/software/R/markdown_multiple_reports.html
d <- indata %>%
  filter(Flag == "0", DatasetID == "SBMNH" | DatasetID == "Seamounts")
unique(d$DatasetID)

for (dataset in unique(d$DatasetID)){
  rmarkdown::render('C:/rworking/digs/code/20171006-0_cruise_data_dashboard_RPMcGuinn.rmd',  # file 2
                    output_file =  paste("report_", dataset, '_', Sys.Date(), ".doc", sep=''),
                    output_dir = 'C:/rworking/digs/outdata/report')
}

##### _____ 20171010 Running Quarterly Report on New Data Data #####

x <- indata %>%
  filter(#grepl("", indata$Vessel),
         Flag == "0",
         DatasetID != "YPM-IZ",
         grepl("2017-10", indata$EntryDate)) %>%
  group_by(DatasetID, EntryDate) %>%
  summarise(n = n(),
            PIUnique = toString(unique(PI)),
            PINumber = length(unique(PI)),
            SurveyIDUnique = toString(unique(SurveyID)),
            SurveyIDNumber = length(unique(SurveyID)),
            AccessionIDUnique = toString(unique(AccessionID)),
            AccessionIDNumber = length(unique(AccessionID)),
            VesselUnique = toString(unique(Vessel)),
            VesselNumber = length(unique(Vessel)),
            DataProviderUnique = toString(unique(DataProvider)),
            DataProviderNumber = length(unique(DataProvider)),
            GenusUnique = toString(unique(Genus)),
            GenusNumber = length(unique(Genus)),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            MinDepth = min(as.numeric(DepthInMeters)),
            MaxDepth = max(as.numeric(DepthInMeters)))
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### _____ Tissot comparison #####
x <- indata %>%
  filter(grepl("Tissot", indata$PI) |
           (grepl("Tissot", indata$DataProvider) |
              (grepl("Tissot", indata$Citation) |
                 (grepl("Tissot", indata$DataContact)
                 ))))

y <- indata %>%
  filter(grepl("Yoklavich", indata$PI) |
           (grepl("Yoklavich", indata$DataProvider) |
              (grepl("Yoklavich", indata$Citation) |
                 (grepl("Yoklavich", indata$DataContact)
                 ))))

# subset y so that it has none of the same Catalog numbers as x
list <- setdiff(y$CatalogNumber, x$CatalogNumber)
y <- y[y$CatalogNumber %in% list, ]

z <- rbind(x,y)

# # cleaning
# rm(indata)
# rm(x)
# rm(y)

# length(unique(z$CatalogNumber))
# length(z$CatalogNumber)

# summary of x
zsummary <- z %>%
  group_by(DatasetID) %>%
  summarise(n = n(),
            PIUnique = toString(unique(PI)),
            PINumber = length(unique(PI)),
            SurveyIDUnique = toString(unique(SurveyID)),
            SurveyIDNumber = length(unique(SurveyID)),
            AccessionIDUnique = toString(unique(AccessionID)),
            AccessionIDNumber = length(unique(AccessionID)),
            VesselUnique = toString(unique(Vessel)),
            VesselNumber = length(unique(Vessel)),
            DataProviderUnique = toString(unique(DataProvider)),
            DataProviderNumber = length(unique(DataProvider)),
            GenusUnique = toString(unique(Genus)),
            GenusNumber = length(unique(Genus)),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            MinDepth = min(as.numeric(DepthInMeters)),
            MaxDepth = max(as.numeric(DepthInMeters)))

View(zsummary)

##### ouput #####
setwd("C:/rworking/digs/outdata")
write.csv(z,"z.csv", row.names = F, quote = T)
write.csv(zsummary,"zsummary.csv", row.names = F, quote = T)

#####adding spatial join table from ArcGIS #####
setwd("C:/rworking/digs/indata")
yo <- read.csv("yo.csv", header = T)

##### Listing names #####
names(yo[,order(colnames(yo))])

##### summarizing potential overlap areas #####
yosummary <- yo %>%
  group_by(AccssID, AccssID_1) %>%
  summarise(n = n(),
            PIUnique = toString(unique(PI)),
            AccssIDUnique = toString(unique(AccssID)),
            PIUnique = toString(unique(PI))
  )

#View(yosummary)

a <- unique(yosummary$AccssID)
b <- unique(yosummary$AccssID_1)
a <- as.character(a)
b <- as.character(b)

c <- subset(indata, AccessionID %in% a | AccessionID %in% b)
c <- subset(c, AccessionID != "NMNH_1868_2011")

# max(c$Latitude)
# table(factor(c$AccessionID), useNA = "always")



##### setting coordinates for x, y, and z ####
coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

coordinates(y) <- c("Longitude", "Latitude")
proj4string(y) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

coordinates(z) <- c("Longitude", "Latitude")
proj4string(z) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### leaflet map of results ####
require(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=c,
                      radius=8,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = x$AccessionID
)
m <- addCircleMarkers(m, data=y,
                      radius=5,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = x$AccessionID
)
m
##### make a shapefile with the OGR driver #####

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)


setwd("C:/rworking/digs/outdata")
writeOGR(y, dsn="y",
         layer= "y",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

setwd("C:/rworking/digs/outdata")
writeOGR(z, dsn="z",
         layer= "z",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### making a KML file of the results #####
setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x.kml", layer= "DSC_RTP",
         driver="KML", dataset_options=c("NameField=ScientificName"),
         overwrite_layer = T)

##### _____ Working on rendering RMD files for different subgroups (Example) #####
# REF: https://stackoverflow.com/questions/30422008/r-knitr-pdf-is-there-a-posssibility-to-automatically-save-pdf-reports-generate

# You need one .rmd "template" file. It could
# be something like this, save it as template.rmd.

# Here is what should be in the RMD file
# This is a subgroup report.


#Report Analysis
summary(subgroup)
```
# Then, you need an R script that will load the data you want,
# loop through the data subsets, and for each subset
# Define the subgroup object used inside the template
# render the template to the desired output
# So, in this separate script:

setwd("C:/rworking/digs/indata")
d <- indata %>%
  filter(grepl("2017-10", indata$EntryDate), AccessionID != "YPM_1842_2004")

d <- indata %>%
  filter(AccessionID == "NOAA_CINMS_SW-16-08_Etnoyer_2016_2016" |
           AccessionID == "UNCW_Ron_Brown_Ross_2010_2010",
         Flag == "0")

setwd("C:/rworking/digs/indata")
d <- read.csv("20170726-2_NOAA_SWFSC_Yoklavich_IMPACT_VeleroIV_2008_2009.csv", header = T)

version <- "20171027-0"

# check
# table(factor(d$AccessionID))
# table(factor(d$ObservationYear))
# table(factor(d$IdentificationDate))

library("rmarkdown")
for (id in unique(d$AccessionID)){
  sub <- d[d$AccessionID == id,]
  render("20171012-0_accession_qa_dashboard_RPMcGuinn.rmd",output_file = paste0(version, "_", id, '.html'))
}

#####_____ Looking at spots where VernacularNameCategory is blank in current global#####
x <- indata %>%
  filter(Flag == "0", VernacularNameCategory == "NA" | is.na(VernacularNameCategory)) %>%
  group_by(DatasetID, ScientificName, VernacularNameCategory) %>%
  summarize(n=n())
View(x)


setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)



#####_____ Inspecting DataProvider #####
table(indata$DataProvider, useNA = "always")

#####_____ Inspecting Johh Reed data #####

x <- indata %>%
  filter(grepl("Pisces", indata$Vessel)) %>%
  group_by(AccessionID) %>%
  summarise(n = n(),
            DatasetID_ = toString(unique(DatasetID)),
            SurveyIDUnique = toString(unique(SurveyID)),
            SurveyIDNumber = length(unique(SurveyID)),
            VesselUnique = toString(unique(Vessel)),
            VesselNumber = length(unique(Vessel)),
            DataProviderUnique = toString(unique(DataProvider)),
            DataProviderNumber = length(unique(DataProvider)),
            GenusUnique = toString(unique(Genus)),
            GenusNumber = length(unique(Genus)),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            Modified_ = toString(unique(Modified)),
            EntryDate_ = toString(unique(EntryDate)),
            MinDepth = min(as.numeric(DepthInMeters)),
            MaxDepth = max(as.numeric(DepthInMeters)))
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### Illustrating need for database #####

indatafilter <- filter(indata, Flag == "0",
                            #as.numeric(DepthInMeters) > 50,
                            #as.numeric(DepthInMeters) < 90,
                            # is.na(ImageURL) == F
                            as.numeric(Latitude) > 38,
                            as.numeric(Latitude) < 55,
                            as.numeric(Longitude) < -121,
                            as.numeric(Longitude) > -131
)


x <- indatafilter %>%
  filter(grepl("Museum", indatafilter$DataProvider))

y <- indatafilter %>%
  filter(grepl("ROV", indatafilter$SamplingEquipment) |
           grepl("AUV", indatafilter$SamplingEquipment))

z <- indatafilter %>%
  filter(grepl("trawl", indatafilter$SamplingEquipment))


##### make the map in leaflet #####
# install.packages("leaflet")
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addCircleMarkers(m, data=x,
                        radius=5,
                        weight=0,
                        fillColor= "blue",
                        fillOpacity=1,
                        popup = x$DataProvider
  )
m

m <- addCircleMarkers(m, data=y,
                      radius=3,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = y$DataProvider
)
m

m <- addCircleMarkers(m, data=z,
                      radius=1,
                      weight=0,
                      fillColor= "black",
                      fillOpacity=1,
                      popup = z$DataProvider
)
m


table(factor(indatafilter$SamplingEquipment), useNA = "always")

table(factor(indatafilter$DataProvider), useNA = "always")

length(unique(indatafilter$DataProvider))

length(unique(indata$DataProvider))

table(factor(indata$SamplingEquipment), useNA = "always")

##### drop camera #####

x <- indata %>%
  filter(grepl("drop camera", indata$SamplingEquipment))

table(factor(x$DataProvider), useNA = "always")


##### Adelogorgia species maps for Elizabeth Gugliotti #####
spec <- "Adelogorgia"
sub <- indata %>%
  filter(grepl(spec, ScientificName))

setwd("C:/rworking/digs/outdata")
write.csv(sub,"20171122_0_Adelogorgia_Matches_from_DSCRTP_NatDB_20171006-0.csv", row.names = F, quote = T)

##### Seaching for Specific Vessels #####
x <- indata %>%
  filter(grepl("Sea Storm", indata$Vessel)) %>%
  group_by(AccessionID) %>%
  summarise(n = n(),
            DatasetID_ = toString(unique(DatasetID)),
            SurveyIDUnique = toString(unique(SurveyID)),
            SurveyIDNumber = length(unique(SurveyID)),
            VesselUnique = toString(unique(Vessel)),
            VesselNumber = length(unique(Vessel)),
            DataProviderUnique = toString(unique(DataProvider)),
            DataProviderNumber = length(unique(DataProvider)),
            GenusUnique = toString(unique(Genus)),
            GenusNumber = length(unique(Genus)),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            N_coral = length(Phylum[Phylum == "Cnidaria"]),
            N_sponges = length(Phylum[Phylum == "Porifera"]),
            StartYear = min(as.numeric(ObservationYear[ObservationYear != -999])),
            EndYear = max(as.numeric(ObservationYear[ObservationYear != -999])),
            Modified_ = toString(unique(Modified)),
            EntryDate_ = toString(unique(EntryDate)),
            MinDepth = min(as.numeric(DepthInMeters)),
            MaxDepth = max(as.numeric(DepthInMeters)))
View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

##### make shapefile lines from xy coordinates and mapping with leaflet and points #####
setwd("C:/rworking/digs/indata")

x <- "DSCRTP_NatDB_OceanaSubmission_SouthernCA2016_points"
d2 <- read.csv(paste(x,".csv",sep=''),header = T)

x <- "DSCRTP_NatDB_OceanaSubmission_SouthernCA2016_lines"
d <- read.csv(paste(x,".csv",sep=''),header = T)

begin.coord <- data.frame(lon=d$StartLongitude, lat=d$StartLatitude)
end.coord <- data.frame(lon=d$EndLongitude, lat=d$EndLatitude)

l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}
lines<-SpatialLines(l)
class(lines)

##### map lines and points using leaflet #####
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addPolylines(m, data=lines,
                    fillColor= "green"

)
m <- addCircleMarkers(m, data=d2,
                      radius=1,
                      weight=0,
                      lng = d2$LongitudeInDD,
                      lat = d2$LatitudeInDD,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = d$ScientificName
)
m

##### getting line segments and calculatng length#####
# make the point file into a SpatialPointsDataFrame

d <- indata %>%
  filter(StartLatitude != "-999", StartLongitude != "-999", EndLatitude != "-999", EndLongitude != "-999")
d <- head(d, n = 2000)

length(d$CatalogNumber)

d2 <- d #%>%
  #filter(StartLatitude != "-999")
coordinates(d2) <- c("Longitude", "Latitude")
proj4string(d2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# creating spatial lines
begin.coord <- data.frame(lon=d2$StartLongitude, lat=d2$StartLatitude)
end.coord <- data.frame(lon=d2$EndLongitude, lat=d2$EndLatitude)

l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}
lines<-SpatialLines(l)
class(lines)

# this step turns the lines file into a SpatialLinesDataFrame for use with the writeOGR function
lines2 <- SpatialLinesDataFrame(lines, d2@data)
class(lines2)
names(lines2)

# adding a length variable

lines2$length <- SpatialLinesLengths(lines, longlat = T)
proj4string(lines2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
lines2
names(lines2)

setwd("C:/rworking/digs/outdata")
writeOGR(lines2, dsn="what",
         layer= "what",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

summary(lines2$length)

##### building a taxonomy table #####
x <- d %>%
  #filter(as.numeric(Flag) == 0)  %>%
  group_by(VernacularNameCategory,	VernacularName,	ScientificName,	TaxonRank,	AphiaID,	Phylum,	Class,	Subclass,	Order,	Suborder,	Family,	Subfamily,	Genus,
           Subgenus, Species,	Subspecies,	ScientificNameAuthorship, Synonyms) %>%
  summarize(n = n())

setwd("C:/rworking/digs/indata")
z <- read.csv("z.csv", header = T)

list <- z$ScientificName
setdiff(z$ScientificName,x$ScientificName)
xsub <- x[x$ScientificName %in% list, ]

setwd("C:/rworking/digs/outdata")
write.csv(xsub,"xsub.csv", row.names = F, quote = T)

#checking
yo <- indata %>%
  filter(grepl("492133", CatalogNumber)) %>%
  group_by(DatasetID, DataProvider, Citation,
           ScientificName, EndLatitude, EndLongitude, StartLatitude, StartLongitude) %>%
  summarize(n=n())
View(yo)


###### Locality diversity  #####
sum_tbl <-
  indata %>%
  group_by(DatasetID) %>%
  summarize(length = length(unique(Locality)),
            DataProvider = toString(unique(DataProvider)),
            PI = toString(unique(PI)),
            Reporter = toString(unique(Reporter)),
            Reporter_Email = toString(unique(ReporterEmail)),
            DataContact = toString(unique(DataContact)))

##### Geographic subsetting and shapefile #####

y <- filter(indata, as.numeric(Latitude) > 24,
                       as.numeric(Latitude) < 36,
                       as.numeric(Longitude) < -73,
                       as.numeric(Longitude) > -94)

coordinates(y) <- c("Longitude", "Latitude")
proj4string(y) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/rworking/digs/outdata")
writeOGR(y, dsn="y",
         layer= "y",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)


x <- setwd("C:/rworking/digs/indata")
indata<-read.csv("x.csv", header = T)
names(indata)

x$Long <- as.numeric(x$Long)
x$Lat <- as.numeric(y$Lat)

coordinates(x) <- c("Long", "Lat")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### Christopher Kelley #####
yo <- indata %>%
  filter(grepl("EX", SurveyID)) %>%
  group_by(PI, DatasetID, DataProvider, Citation, ScientificName) %>%
  summarize(n=n())
View(yo)


names(indata)


##### barplot using lattice #####
barchart(VernacularNameCategory, data=indata,
         par.strip.text=list(cex=2),
         stack=TRUE, col=c("green","red","blue"),
         xlim=c(0,100), layout=c(3,1),
         scales=list(cex=c(1.4,1.4), alternating=3),
         xlab=list(label="Percent of Respondents", fontsize=20),
         main="")

##### Heteropathes in National Database #####
x <- indata %>%
  filter(#is.na(ImageURL) == F,
         grepl("Heteropathes", ScientificName)) %>%
  dplyr::select(CatalogNumber, VernacularNameCategory, ScientificName, DepthInMeters, FishCouncilRegion, Locality, ImageURL, Latitude, Longitude)


x$Longitude <- as.numeric(x$Longitude)
x$Latitude <- as.numeric(x$Latitude)

coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="heteropathes.kml", layer= "DSC_RTP", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

setwd("C:/rworking/digs/outdata")
write.csv(x,"20171216_0_Heteropathes_NDB_20171006_0_RPMcGuinn.csv", row.names = F, quote = T)

View(x)
##### Exploring ggplot with families, temperature, depth, salinity, and oxygen
# Examining count and levels of Genus grouped by Family
x <- indata %>%
  filter(Flag == "0") %>%
  group_by(Family) %>%
  summarize(genus_n = length(unique(Genus)),
            n = n(),
            Genera = toString(unique(Genus)))
View(x)

# Examining each Genus within Family, by FishCouncilRegion
# Recall that not all records have Temperature
indata %>%
  filter(Flag == '0',
         #Salinity != "-999",
         #Salinity != "0",
         #DepthInMeters != "-999",
         #Oxygen != "-999",
    Temperature != "-999",
    Family == "Acarnidae") %>%
  ggplot(aes(x = Genus, y = Temperature)) +
  facet_wrap(~ FishCouncilRegion) +
  geom_boxplot() + coord_flip()

# Examining DepthInMeter within each Genus within a Family, faceted by FishCouncilRegion
# note: Recall that not all records have Temperature
indata %>%
  filter(Flag == '0',
         #Salinity != "-999",
         #Salinity != "0",
         DepthInMeters != "-999",
         #Oxygen != "-999",
         #Temperature != "-999",
         Family == "Stylasteridae",
         is.na(FishCouncilRegion) == T) %>%
  ggplot(aes(x = Genus, y = DepthInMeters)) +
  facet_wrap(~ FishCouncilRegion) +
  geom_boxplot() + coord_flip()


##### Using Lattice to explore DepthInMeters by FishCouncilRegion #####
library(lattice)
bwplot(~as.numeric(DepthInMeters)| FishCouncilRegion, data=indata,
       type="percent",
       xlab="DepthInMeters",
       #ylim=c(0,60),
       xlim=c(0,5000),
       #main="Depth Distribution,\nby Fish Council Region",
       breaks=seq(from=0,to=5000,by=50),layout=c(1,8))
```
##### Working on Size something is numeric? #####
x <- filt %>% filter(!is.na(as.numeric(as.character(filt$Size))))
y <- filt %>% filter(is.na(Size) == F)
length(x$CatalogNumber)
length(y$CatalogNumber)


##### Working with the the schema #####

setwd("C:/rworking/digs/indata")
s <- read.csv("20171220-0_DSCRTP_Schema.csv", header = T)
table(s$PointProgram, useNA = "always")

x <- s %>%
  filter(PointProgram == 'R'
         ) %>%
  group_by(OBIS_USA_Section) %>%
  summarize(Field_Names = toString(unique(FieldName)))
View(x)

sum_tbl <-
  s %>% filter(InternalUseOnly == '0') %>%
  group_by(FieldName) %>%
  summarize(Field_Definition = toString(unique(FieldDescription)),
            Valid_Values = toString(unique(ValidValues)))
View(sum_tbl)

##### testing valid values #####

yo <- filt %>%
  filter(CategoricalAbundance %in% c('present','1','2-10','11-20','21-50','51-100','>100'))
length(yo$CatalogNumber)

condition <- filt$CategoricalAbundance %in% c('present','1','2-10','11-20','21-50','51-100','>100')

x <- length(condition[condition == T])

y <-filter(filt, is.na(filt$CategoricalAbundance) == FALSE)
length(y$CatalogNumber)

z<-filter(filt, is.na(filt$CategoricalAbundance) == TRUE)
length(z$CatalogNumber)

t <- length(filt$CatalogNumber)

##### indata #####
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20171214-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0",
         ObservationYear != "-999",
         DepthInMeters != "-999",
         is.na(Phylum) == F)

setwd("C:/rworking/digs/indata")
s <- read.csv("20171220-0_DSCRTP_Schema.csv", header = T)


##### cumulative frequency line chart #####
x <- data.frame(A=replicate(200,sample(c("a","b","c"),1)),X=rnorm(200))
View(x)

ggplot(x,aes(x=X,color=A)) + geom_step(aes(y=..y..),stat="ecdf")

ggplot(filt,aes(x=as.Date(EntryDate),color=Phylum)) +
  stat_bin(data = subset(filt,Phylum =="Porifera"),aes(y=cumsum(..count..)),geom="line", binwidth = 2)+
  stat_bin(data = subset(filt,Phylum =="Cnidaria"),aes(y=cumsum(..count..)),geom="line", binwidth = 2)

filt2 <- filt %>%
  filter(as.Date(EntryDate) > as.Date('2010-01-01'))
options(scipen=10000)
ggplot(filt2,aes(x=as.Date(EntryDate))) +
       stat_bin(aes(y=cumsum(..count..)),geom="line", binwidth = 10) +
#  scale_x_date(limits = as.Date(c('2010-01-01','2017-01-01'))) +
  ylab("Cumulative Number of Records") + xlab("Date Record Added") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0)) +
  scale_color_manual(values=c("#999999", "#E69F00"))

filt2 <- filt %>%
  filter(as.Date(EntryDate) > as.Date('2010-01-01'))
options(scipen=10000)
ggplot(filt2,aes(x=as.Date(EntryDate), color=Phylum)) +
  stat_bin(data = subset(filt2, Phylum =="Porifera"),aes(y=cumsum(..count..)),geom="line", binwidth = 2) +
  stat_bin(data = subset(filt2, Phylum =="Cnidaria"),aes(y=cumsum(..count..)),geom="line", binwidth = 2) +
  #  scale_x_date(limits = as.Date(c('2010-01-01','2017-01-01'))) +
  ylab("Cumulative Number of Records") + xlab("Date Record Added") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0)) +
  scale_color_manual(values=c("#999999", "#E69F00")) + theme_bw()

summary(as.Date(filt2$EntryDate))

##### R Tips by Maria Nattestad #####
library(ggplot2)
basic <- ggplot(indata,aes(x=VernacularNameCategory,fill=Phylum)) +
  geom_bar() +
  labs(title="VernacularNameCategory", x = "VernacularNameCategory", y = "Record Count")

# Add theme with modifications to the basic plot, for instance with bigger text
basic + theme_gray(base_size = 20)

# But it only affects that plot, so the next plot we make is back to normal
basic

# You can also set a theme that will affect all the plots you make from now on
theme_set(theme_bw(base_size = 20))
basic

# To recover the default theme:
theme_set(theme_gray())
basic

# I prefer larger text myself
theme_set(theme_gray(base_size = 16))
basic

##### colors #####
# Color palettes:

library(RColorBrewer)
display.brewer.all()

# Color-blind safe palettes:
display.brewer.all(colorblindFriendly=TRUE)

basic + scale_fill_brewer(palette="Set1")
basic + scale_fill_brewer(palette="Pastel1")
basic + scale_fill_brewer(palette="YlOrRd")

# set colors manually
basic + scale_fill_manual(values = c("green","blue","red"))

# see all colors
colors()

##### using lots of colors #####
filt2 <- sample_n(filt, 100)
x <- ggplot(filt2,
            aes(x=VernacularNameCategory,fill=Genus)) +
  geom_bar()
x

# rainbow is confusing, but color palettes are too short:
x + scale_fill_brewer(type="qual",palette=1)

# to get the colors from a palette:
palette1 <- brewer.pal(9,"Set1")
palette1

palette2 <- brewer.pal(8,"Set2")
palette2

palette3 <- brewer.pal(9,"Set3")
palette3

palette4 <- brewer.pal(9,"Dark2")
palette4

palette5 <- brewer.pal(8,"Accent")
palette5

#see a bunch of color choices
brewer.pal.info

# We can use a quick pie chart to see the colors:
pie(rep(1,length(palette1)),col=palette1)
pie(rep(1,length(palette2)),col=palette2)
pie(rep(1,length(palette3)),col=palette3)
pie(rep(1,length(palette4)),col=palette4)
pie(rep(1,length(palette5)),col=palette5)

# We can just stick a few color palettes together
big_palette <- c(palette1,palette2,palette3, palette4, palette5)
big_palette

# Pie chart of all the colors together:
pie(rep(1,length(big_palette)),col=big_palette)
x + scale_fill_manual(values = big_palette) + theme_bw()

# To shuffle the colors:
x + scale_fill_manual(values = sample(big_palette))

# if you want to keep the colors the same every time you plot,
# use set.seed()
set.seed(5)
# use different numbers until you find your favorite colors
x + scale_fill_manual(values = sample(big_palette))

# This is possible, because:
# Fun fact: "Random" numbers from a computer aren't really random

# Color-blind safe palettes:
display.brewer.all(colorblindFriendly=TRUE)
# Mixing them might remove the color-friendly-ness so be careful
# Finding a set of 23 colors that a color-blind person can distinguish is a challenge
x + scale_fill_brewer(palette="Set2")

##### titles and fonts #####
basic <- ggplot(filt2,aes(x=VernacularNameCategory,
                      fill=Genus)) + geom_bar()
# Default:
theme_set(theme_gray())

# Basic, normal plot:
basic

# Two basic themes:
basic + theme_gray() # the default
basic + theme_bw() # black and white

# applying a different color pallete manually to the fill parameter
set.seed(5)
basic + scale_fill_manual(values = sample(big_palette))

# or
g + scale_fill_manual(values = sample(brewer.pal(8,"Dark2")))

# or specific colors

g + scale_fill_manual(values = brewer.pal(12, "Paired")[c(3,4)])





##### Fonts #####
# Font size for labels, tick labels, and legend separately
# install.packages("extrafont")
# library(extrafont)
# fonts()
# font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)

# Fonts and font sizes for everything at once
basic + theme_bw(base_size = 15, base_family = "Lucida Bright")

# Fonts on everything separately
# numbers on axes
basic + theme(axis.text=element_text(size=20))

# titles on axes
basic + theme(axis.title=element_text(size=20))

# legend title
basic + theme(legend.title=element_text(size=20))

# legend category labels (labels of levels)
basic + theme(legend.text=element_text(size=20,family="Times New Roman"))

# Mix and match
basic + theme(
  legend.text=element_text(size=20,family='Times New Roman'),
  axis.title=element_text(size=30, family = 'Times New Roman'),
  axis.text=element_text(size=20, family = 'Times New Roman')
)

# Change background color
basic + theme(panel.background = element_rect(fill="pink"))
basic + theme(panel.background = element_rect(fill="white"))

# Change grid-line colors
basic + theme(panel.grid.major = element_line(colour = "blue"),
              panel.grid.minor = element_line(colour = "red"))

# Remove all gridlines:
basic + theme(panel.grid.major = element_line(NA),
              panel.grid.minor = element_line(NA))

# Thin black major gridlines on y-axis, the others are removed
basic + theme(panel.grid.major.y = element_line(colour = "black",size=0.2),
              panel.grid.major.x = element_line(NA),
              panel.grid.minor = element_line(NA))

# Change tick-marks
basic # normal ticks
basic + theme(axis.ticks = element_line(size=2))
basic + theme(axis.ticks = element_line(NA))
basic + theme(axis.ticks = element_line(color="blue",size=2))
basic + theme(axis.ticks = element_line(size=2), # affects both x and y
              axis.ticks.x = element_line(color="blue"), # x only
              axis.ticks.y = element_line(color="red"))  # y only

# Place legend in different locations
basic + theme(legend.position="top")
basic + theme(legend.position="bottom")
basic + theme(legend.position=c(0,0)) # bottom left
basic + theme(legend.position=c(1,1)) # top right
basic + theme(legend.position=c(0.8,0.8)) # near the top right

# Remove legend title
basic + labs(fill="")
basic + labs(fill="") + theme(legend.position=c(0.8,0.8))

# Remove legend completely
basic + guides(fill=FALSE)

# clear background, axis lines but no box, no grid lines, basic colors, no legend
publication_style <- basic + guides(fill=FALSE) +
  theme(axis.line = element_line(size=0.5),panel.background = element_rect(fill=NA,size=rel(10)),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18))

publication_style

publication_style + scale_y_continuous(expand=c(0,0)) # to stop the bars from floating above the x-axis

# You can set the theme with all these changes and have it apply to all the future plots
theme_set(theme_gray()+
            theme(axis.line = element_line(size=0.5),
                  panel.background = element_rect(fill=NA,size=rel(20)),
                  panel.grid.minor = element_line(colour = NA),
                  axis.text = element_text(size=16), axis.title = element_text(size=18)))

basic

# These tweaks aren't part of the theme, so you will still have to add them separately to each plot
basic + scale_y_continuous(expand=c(0,0)) + guides(fill=FALSE)

# and you can always reset to defaults with:
theme_set(theme_gray())
basic

##### Other Graph Types #####
filt2 <- sample_n(filt, 1000)

# picking just the levels that you want
my_data <- filt2[filt2$Genus %in% c('Ellisella','Fanellia','Farrea','Swiftia', 'Stylaster'),]

# This is how you reorder levels in the order you want
my_data$Genus <- factor(my_data$Genus, levels=c('Fanellia','Ellisella','Farrea','Swiftia', 'Stylaster'))
my_data$Phylum <- factor(my_data$Phylum, levels=c('Porifera','Cnidaria'))

# Creating a bar plot
ggplot(my_data, aes(x=Genus,fill= Phylum)) + geom_bar()

# Creating a histogram
ggplot(filt2, aes(x=as.numeric(DepthInMeters))) + geom_bar()
ggplot(filt2, aes(x=as.numeric(DepthInMeters))) + geom_bar(binwidth=20) + xlim(0,500)

# Creating scatter plot colored by factor
filt2 <- filt %>% sample_n(10000) %>%
  filter(as.numeric(DepthInMeters) > 0,
         as.numeric(Temperature) > 0, as.numeric(Oxygen) > 0)
ggplot(filt2, aes(x = Temperature,y = DepthInMeters, color = Phylum)) +
  geom_point()

# Creating scatter plot colored by continuous
filt2 <- filt %>% sample_n(10000) %>%
  filter(as.numeric(DepthInMeters) > 0,
         as.numeric(Temperature) > 0, as.numeric(Oxygen) > 0)
ggplot(filt2, aes(x = Temperature,y = DepthInMeters, color = as.numeric(Oxygen))) +
  geom_point() +
  scale_colour_gradient(limits=c(0,5)) # use this to scale continous color gradient

# Creating box plots
filt2 <- filt %>%
  filter(as.numeric(DepthInMeters) > 300,
         as.numeric(Temperature) > 0,
         as.numeric(Oxygen) > 0) %>%
  sample_n(100)

ggplot(filt2, aes(x=Family,y=DepthInMeters,fill=Phylum)) + geom_boxplot()

# flipping coordinates
ggplot(filt2, aes(x=factor(ScientificName),y=DepthInMeters,fill=Phylum)) + geom_boxplot() + coord_flip()

# Creating violin plots
ggplot(filt2, aes(x=factor(Genus),y=DepthInMeters)) + geom_violin(adjust = 0.2) + facet_wrap(~Family)
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin() + ylim(0,1000) + guides(fill=FALSE)
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin(adjust=0.2) + ylim(0,1000) + guides(fill=FALSE)
# default adjust is 1, lower means finer resolution

# You can log-scale any numerical axis on any plot
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin() +
  scale_y_log10()

# Creating a density plot
ggplot(my_data, aes(x=size,fill=type)) + geom_density() + xlim(0,500)
# similar to this histogram:
ggplot(my_data, aes(x=size,fill=type)) + geom_bar(binwidth=5) + xlim(0,500)

ggplot(my_data, aes(x=size,fill=type)) + geom_density(position="stack") + xlim(0,500)
ggplot(my_data, aes(x=size,fill=type)) + geom_density(alpha=0.5) + xlim(0,500)

# Sneak peak at Lesson 7: multifaceted plots:
ggplot(my_data, aes(x=size,fill=type)) + geom_density() + xlim(0,500) + facet_grid(type ~ .)

# Creating dot plots
ggplot(my_data, aes(x=size,fill=type)) + geom_dotplot()
# a dot plot makes more sense with fewer observations where each individual item matters,
# so let's grab the largest events only
large_data <- my_data[my_data$size>5000,  ] # [rows,columns]
ggplot(large_data, aes(x=size,fill=type)) + geom_dotplot(method="histodot")
# careful, they don't stack automatically, so some of the dots are behind others
ggplot(large_data, aes(x=size,fill=type)) + geom_dotplot(method="histodot",stackgroups=TRUE)

# Time-course data for line plot
filename <- "Lesson-06/time_course_data.txt"
time_course <- read.csv(filename, sep=",", quote='', stringsAsFactors=TRUE,header=TRUE)
time_course

# line plot:
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line()
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line(size=3)

# For fun:
# Any plot can be made in polar coordinates:
# line plot
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line(size=3) + coord_polar()

# violin plot
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin(adjust=0.5) + ylim(0,1000) + coord_polar()
# bar plot
ggplot(my_data, aes(x=size,fill=type)) + geom_bar(binwidth=5) + xlim(0,500) + coord_polar()

# Pie charts
type_counts = summary(my_data$type)
type_counts

pie(type_counts)
pie(type_counts,col=brewer.pal(length(type_counts),"Set1"))

# Gene lists for Venn Diagram
listA <- read.csv("Lesson-06/genes_list_A.txt",header=FALSE)
A <- listA$V1
A

listB <- read.csv("Lesson-06/genes_list_B.txt",header=FALSE)
B <- listB$V1
B

listC <- read.csv("Lesson-06/genes_list_C.txt",header=FALSE)
C <- listC$V1
C

listD <- read.csv("Lesson-06/genes_list_D.txt",header=FALSE)
D <- listD$V1
D

length(A)
length(B)
length(C)
length(D)

# install package VennDiagram
library(VennDiagram)

# This function only works by saving directly to a file

venn.diagram(list("list C"=C, "list D"=D), fill = c("yellow","cyan"), cex = 1.5, filename="Lesson-06/Venn_diagram_genes_2.png")

venn.diagram(list(A = A, C = C, D = D), fill = c("yellow","red","cyan"), cex = 1.5,filename="Lesson-06/Venn_diagram_genes_3.png")

venn.diagram(list(A = A, B = B, C = C, D = D), fill = c("yellow","red","cyan","forestgreen"), cex = 1.5,filename="Lesson-06/Venn_diagram_genes_4.png")


##### revalues on subsets based on a range of conditions #####
filt2 <- sample_n(filt, 30)
table(factor(filt2$PI))
table(factor(filt2$Family))
filt2 %>% dplyr::select(Family, PI, DatasetID)

# This is how you use a conditional statement and revlue a factor using the mutate function
filt3 <- filt2 %>% mutate(PI = ifelse(DatasetID == "HURL" & PI == "Stone, RP", 'yo_homey', as.character(PI)))
filt3 %>% dplyr::select(Family, PI, DatasetID)

##### many plotting types using ggplot2 #####
#
#      Lesson 6 -- Plot anything
#      •   Bar plots
#      •   Histograms
#      •   Scatter plots
#      •   Box plots
#      •   Violin plots
#      •   Density plots
#      •   Dot plots
#      •   Line plots for time-course data
#      •   Pie charts
#      •   Venn diagrams (compare two or more lists of genes)


# Creating a bar plot
ggplot(my_data, aes(x=chrom,fill=type)) + geom_bar()

# Creating a histogram
ggplot(my_data, aes(x=size,fill=type)) + geom_bar()
ggplot(my_data, aes(x=size,fill=type)) + geom_bar() + xlim(0,500)
ggplot(my_data, aes(x=size,fill=type)) + geom_bar(binwidth=5) + xlim(0,500)

# Creating scatter plot
ggplot(my_data, aes(x=ref.dist,y=query.dist)) + geom_point()
# color by type (categorical)
ggplot(my_data, aes(x=ref.dist,y=query.dist,color=type)) + geom_point()
ggplot(my_data, aes(x=ref.dist,y=query.dist,color=type)) + geom_point() + xlim(-500,500) + ylim(-500,500)
# color by size (numerical)
ggplot(my_data, aes(x=ref.dist,y=query.dist,color=size)) + geom_point() + xlim(-500,500) + ylim(-500,500)

ggplot(my_data, aes(x=ref.dist,y=query.dist,color=size)) + geom_point() + xlim(-500,500) + ylim(-500,500) + scale_colour_gradient(limits=c(0,500))

# Creating box plots
ggplot(my_data, aes(x=type,y=size)) + geom_boxplot()
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_boxplot()
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_boxplot() + coord_flip()

# Creating violin plots
ggplot(my_data, aes(x=type,y=size)) + geom_violin()
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin() + ylim(0,1000) + guides(fill=FALSE)
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin(adjust=0.2) + ylim(0,1000) + guides(fill=FALSE)
# default adjust is 1, lower means finer resolution

# You can log-scale any numerical axis on any plot
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin() +
  scale_y_log10()

# Creating a density plot
ggplot(my_data, aes(x=size,fill=type)) + geom_density() + xlim(0,500)
# similar to this histogram:
ggplot(my_data, aes(x=size,fill=type)) + geom_bar(binwidth=5) + xlim(0,500)

ggplot(my_data, aes(x=size,fill=type)) + geom_density(position="stack") + xlim(0,500)
ggplot(my_data, aes(x=size,fill=type)) + geom_density(alpha=0.5) + xlim(0,500)

# Sneak peak at Lesson 7: multifaceted plots:
ggplot(my_data, aes(x=size,fill=type)) + geom_density() + xlim(0,500) + facet_grid(type ~ .)

# Creating dot plots
ggplot(my_data, aes(x=size,fill=type)) + geom_dotplot()
# a dot plot makes more sense with fewer observations where each individual item matters,
# so let's grab the largest events only
large_data <- my_data[my_data$size>5000,  ] # [rows,columns]
ggplot(large_data, aes(x=size,fill=type)) + geom_dotplot(method="histodot")
# careful, they don't stack automatically, so some of the dots are behind others
ggplot(large_data, aes(x=size,fill=type)) + geom_dotplot(method="histodot",stackgroups=TRUE)

# Time-course data for line plot
filename <- "Lesson-06/time_course_data.txt"
time_course <- read.csv(filename, sep=",", quote='', stringsAsFactors=TRUE,header=TRUE)
time_course

# line plot:
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line()
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line(size=3)

# For fun:
# Any plot can be made in polar coordinates:
# line plot
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line(size=3) + coord_polar()

# violin plot
ggplot(my_data, aes(x=type,y=size,fill=type)) + geom_violin(adjust=0.5) + ylim(0,1000) + coord_polar()
# bar plot
ggplot(my_data, aes(x=size,fill=type)) + geom_bar(binwidth=5) + xlim(0,500) + coord_polar()

# Pie charts
type_counts = summary(my_data$type)
type_counts

pie(type_counts)
pie(type_counts,col=brewer.pal(length(type_counts),"Set1"))

# Gene lists for Venn Diagram
listA <- read.csv("Lesson-06/genes_list_A.txt",header=FALSE)
A <- listA$V1
A

listB <- read.csv("Lesson-06/genes_list_B.txt",header=FALSE)
B <- listB$V1
B

listC <- read.csv("Lesson-06/genes_list_C.txt",header=FALSE)
C <- listC$V1
C

listD <- read.csv("Lesson-06/genes_list_D.txt",header=FALSE)
D <- listD$V1
D

length(A)
length(B)
length(C)
length(D)

# install package VennDiagram
library(VennDiagram)

# This function only works by saving directly to a file

venn.diagram(list("list C"=C, "list D"=D), fill = c("yellow","cyan"), cex = 1.5, filename="Lesson-06/Venn_diagram_genes_2.png")

venn.diagram(list(A = A, C = C, D = D), fill = c("yellow","red","cyan"), cex = 1.5,filename="Lesson-06/Venn_diagram_genes_3.png")

venn.diagram(list(A = A, B = B, C = C, D = D), fill = c("yellow","red","cyan","forestgreen"), cex = 1.5,filename="Lesson-06/Venn_diagram_genes_4.png")

##### filter and map #####

x <- filt %>%
  filter(
    #grepl("Seward", Vessel),
    #ObservationYear == "2009",
    #grepl("Ross", PI)
    SurveyID == 'Lophelia II USGS Discovre GOM 2009'
    #DatasetID == 'NOAA_SJ-09-08'
    ) # %>%
  # group_by(DatasetID,
  #          AccessionID,
  #          DataProvider,
  #          PI,
  #          Locality,
  #          Vessel,
  #          VehicleName,
  #          ObservationYear,
  #          Citation,
  #          Repository) %>%
  #
  # summarize(n_records = n(),
  #           flagsum = length(Flag[Flag == "1"]),
  #           minyear = min(as.numeric(ObservationYear)),
  #           maxyear = max(as.numeric(ObservationYear)),
  #           latitude = mean(Latitude),
  #           longitude = mean(Longitude)
  # )

#View(x)

#rm(x)

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap")
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "blue",
                      fillOpacity=1,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", x$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", x$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", x$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", x$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", x$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", x$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", x$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", x$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", x$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", x$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", x$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", x$EventID, "<br>",
                        "<b><em>","Image:","</b></em>",x$ImageURL)

)
m

##### corrections to Farrington set #####
#View(d4)
d4$VerbatimLatitude
# use negation in grepl selection.
d4a <- d4 %>% filter(!(grepl('W', VerbatimLongitude)))
d4b <- d4 %>% filter((grepl('W', VerbatimLongitude)))


table(factor(d4a$VerbatimLongitude))


##### looking at schema  #####
s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
s <- gs_read(s)

# query by field name
sum_tbl <-
  s %>% filter(#grepl('1', MapQuery),
               #is.na(OBIS_USA_Section) == T,
               #OldDSCRTPCategory == 'Flag',
               #DSCRTPGroup == 'Provenance'
               grepl('RecordType', FieldName)) %>%
  group_by(FieldName, FieldOrder, DSCRTPGroup) %>%
  summarize(Field_Definition = toString(unique(FieldDescription)),
            Valid_Values = toString(unique(ValidValues))) %>%
  arrange(FieldOrder)
View(sum_tbl)

table(s$OBIS_USA_Section, useNA = 'always')
table(s$OldDSCRTPCategory, useNA = 'always')
table(s$DSCRTPGroup, useNA = 'always')

# ordering factors by frequency of occurrence
s <- within(s, DSCRTPGroup <- factor(DSCRTPGroup, levels=names(sort(table(DSCRTPGroup), decreasing=FALSE))))
s <- within(s, DSCRTPGroup <- factor(DSCRTPGroup, levels=names(sort(table(DSCRTPGroup), decreasing=FALSE))))

setdiff(s$FieldName, names(indata))


# plotting

  #filter(#grepl('1', MapQuery),
    #is.na(OBIS_USA_Section) == T,
    #OldDSCRTPCategory == 'Flag',
    #DSCRTPGroup == 'Provenance',
    #grepl('Location', DSCRTPGroup),
    #PointProgram == 'R'
    # )
options(scipen=10000)
g <- ggplot(x, aes(DSCRTPGroup, fill = DSCRTPGroup)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(~PointProgram) +
  ylab("Number of Records") +
  theme_bw(base_size = 15, base_family = "Cambria")

#g + scale_fill_manual(values = brewer.pal(12, "Paired")[c(10,9,8,7,6)])

set.seed(7)
g + scale_fill_manual(values = sample(big_palette))

#looking at required fields
sum_tbl <-
  s %>% filter(#grepl('1', MapQuery),
    #is.na(OBIS_USA_Section) == T,
    #OldDSCRTPCategory == 'Flag',
    DSCRTPGroup == 'Provenance'
    #grepl('R', TrawlProgram)
    ) %>%
  group_by(DSCRTPGroup) %>%
  summarize(Fields = toString(unique(FieldName)))
View(sum_tbl)

##### building big pallette #####
# to get the colors from a palette:
palette1 <- brewer.pal(9,"Set1")
palette1
palette2 <- brewer.pal(8,"Set2")
palette2
palette3 <- brewer.pal(9,"Set3")
palette3
palette4 <- brewer.pal(9,"Dark2")
palette4
palette5 <- brewer.pal(8,"Accent")
palette5

#see a bunch of color choices
brewer.pal.info

# We can use a quick pie chart to see the colors:
pie(rep(1,length(palette1)),col=palette1)
pie(rep(1,length(palette2)),col=palette2)
pie(rep(1,length(palette3)),col=palette3)
pie(rep(1,length(palette4)),col=palette4)
pie(rep(1,length(palette5)),col=palette5)

# We can just stick a few color palettes together
big_palette <- c(palette1,palette2,palette3, palette4, palette5)
big_palette

# Pie chart of all the colors together:
pie(rep(1,length(big_palette)),col=big_palette)
x + scale_fill_manual(values = big_palette) + theme_bw()

# To shuffle the colors:
x + scale_fill_manual(values = sample(big_palette))

# if you want to keep the colors the same every time you plot,
# use set.seed()
set.seed(5)
# use different numbers until you find your favorite colors
x + scale_fill_manual(values = sample(big_palette))




##### working on the selection boxes for southeast #####
setwd("C:/rworking/digs/outdata")
#indata<-read.csv("DSCRTP_NatDB_20171214-0.csv", header = T)
indata<-read.csv('20170129_0_DSCRTP_NatDB_20171214_0_Subset_Plus_Ocean_Sciencies_RPMcGuinn.csv')

filt <- indata %>%
  filter(Flag == '0' | Flag == '2',
         ObservationYear != "-999",
         DepthInMeters != "-999",
         is.na(Phylum)== F)
options(digits = 1)

# make the clip
minLon <- -83
maxLon <- -74
minLat <- 23
maxLat <- 36

geofilt <-
  filt %>% filter(as.numeric(Latitude) > minLat,
                  as.numeric(Latitude) < maxLat,
                  as.numeric(Longitude) < maxLon,
                  as.numeric(Longitude) > minLon)

coordinates(geofilt) <- c("Longitude", "Latitude")
proj4string(geofilt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### make a shapefile with the OGR driver #####
setwd("C:/rworking/digs/outdata")
writeOGR(geofilt, dsn="geofilt",
         layer= "geofilt",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

# Northern Carolinean~32 degrees and North to Cape Hatteras
# (while this is a subset of the Carolinean region,
# the Charleston Bump is approximately at this latitude and it appears to act to push the Gulf Stream offshore

# Southern Carolinean
# This would extend from 32 deg. N south to the
# MEOW border with the Floridean bioregion (extending E to the edge of US EEZ)

# South Florida
# This would extend from the MEOW border with the Carolinean region
# around south FLorida to the end of the South Atlantic Council Region.

# make the clip again
minLon <- -83
maxLon <- -74
minLat <- 32.0001
maxLat <- 36

geofilt2 <-
  filt %>% filter(as.numeric(Latitude) > minLat,
                  as.numeric(Latitude) < maxLat,
                  as.numeric(Longitude) < maxLon,
                  as.numeric(Longitude) > minLon)

coordinates(geofilt2) <- c("Longitude", "Latitude")
proj4string(geofilt2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### make a shapefile with the OGR driver #####
setwd("C:/rworking/digs/outdata")
writeOGR(geofilt2, dsn="geofilt2",
         layer= "geofilt2",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)


# make the clip yet again
minLon <- -83
maxLon <- -74
minLat <- 28.5001
maxLat <- 32.0001

geofilt3 <-
  filt %>% filter(as.numeric(Latitude) > minLat,
                  as.numeric(Latitude) < maxLat,
                  as.numeric(Longitude) < maxLon,
                  as.numeric(Longitude) > minLon)

coordinates(geofilt3) <- c("Longitude", "Latitude")
proj4string(geofilt3) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### make a shapefile with the OGR driver #####
setwd("C:/rworking/digs/outdata")
writeOGR(geofilt3, dsn="geofilt3",
         layer= "geofilt3",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

# make the clip yet again again
minLon <- -83
maxLon <- -74
minLat <- 23.8201
maxLat <- 28.5001

geofilt4 <-
  filt %>% filter(as.numeric(Latitude) > minLat,
                  as.numeric(Latitude) < maxLat,
                  as.numeric(Longitude) < maxLon,
                  as.numeric(Longitude) > minLon)

coordinates(geofilt4) <- c("Longitude", "Latitude")
proj4string(geofilt4) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### make a shapefile with the OGR driver #####
setwd("C:/rworking/digs/outdata")
writeOGR(geofilt4, dsn="geofilt4",
         layer= "geofilt4",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### filter and map #####
x <- geofilt4

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap")
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "blue",
                      fillOpacity=1,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", x$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", x$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", x$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", x$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", x$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", x$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", x$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", x$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", x$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", x$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", x$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", x$EventID, "<br>",
                        "<b><em>","Image:","</b></em>",x$ImageURL)

)
m


##### ordering boxplots (factors) #####
a = rnorm(10,mean=3,sd=4)
b = rnorm(10,mean=-1,sd=2)
c = rnorm(10,mean=5,sd=6)
d = rnorm(10,mean=-3,sd=1)
e = rnorm(10,mean=0,sd=.5)

labs = c(rep("a",10),rep("b",10),rep("c",10),rep("d",10),rep("e",10))
mean = c(rep(mean(a),10),rep(mean(b),10),rep(mean(c),10),rep(mean(d),10),rep(mean(e),10))
data = c(a,b,c,d,e)
df = data.frame(labs,data,mean)
df = df[order(df$mean),]

ggplot(as.data.frame(geofilt),  aes(x = reorder(Genus, DepthInMeters, FUN=median), y = DepthInMeters)) + geom_boxplot()


##### filtering by catalognumber #####
filt <- indata %>%
  filter(VernacularNameCategory == 'black coral') %>%
           dplyr::select(CatalogNumber, FishCouncilRegion, ScientificName, DepthInMeters)
View(filt)

table(indata$VernacularNameCategory)
table(indata$FishCouncilRegion)
table(filt$FishCouncilRegion)

##### using mutate to change data #####
# This is how you use a conditional statement and revlue a factor using the mutate function
filt2 <- filt %>% mutate(Ecoregion = ifelse(Ecoregion == 'N_Carolinean', 'North Carolinean', as.character(Ecoregion)))
table(filt2$Ecoregion)
table(filt$Ecoregion)

##### subsetting data group by Ecoregion and Depth Class #####
setwd("C:/rworking/digs/indata")
#indata<-read.csv("DSCRTP_NatDB_20171214-0.csv", header = T)
indata<-read.csv('20180130_0_Genus_NoFlag_TH.csv')

filt <- indata %>%
  filter(Flag == '0' | Flag == '2',
         ObservationYear != "-999",
         DepthInMeters != "-999",
         is.na(Phylum)== F)
options(digits = 1)

# changing levels of 'Ecoregion'
filt2 <- filt %>% mutate(Ecoregion = ifelse(Ecoregion == 'N_Carolinean', 'North Carolinean', as.character(Ecoregion)))

minLon <- -84
maxLon <- -73
minLat <- 22
maxLat <- 37

geofilt <-
  filt2 %>% filter(as.numeric(Latitude) > minLat,
                   as.numeric(Latitude) < maxLat,
                   as.numeric(Longitude) < maxLon,
                   as.numeric(Longitude) > minLon)

coordinates(geofilt) <- c("Longitude", "Latitude")
proj4string(geofilt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

z <- 300

geofilt$DepthCat[geofilt$DepthInMeters > z] <- "deep"
geofilt$DepthCat[geofilt$DepthInMeters < z] <- "shallow"
geofilt$DepthCat <- factor(geofilt$DepthCat, levels = c('deep', 'shallow'))

##### make a shapefile with the OGR driver #####
setwd("C:/rworking/digs/outdata")
writeOGR(geofilt, dsn="geofilt",
         layer= "geofilt",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### start empty subset varialbe #####
geofilt$rep <- NA

##### North Carolinean #####
geofilt_a <- as.data.frame(geofilt) %>%
  filter(DepthCat == 'shallow',
         Ecoregion == 'North Carolinean')
geofilt_a$rep[1:(length(geofilt_a$CatalogNumber)/3)] <- 'shallow_nc_1'
geofilt_a$rep[((length(geofilt_a$CatalogNumber)/3) + 1):((length(geofilt_a$CatalogNumber)/3)*2)] <- 'shallow_nc_2'
geofilt_a$rep[(((length(geofilt_a$CatalogNumber)/3)*2)+1):(length(geofilt_a$CatalogNumber))] <- 'shallow_nc_3'

geofilt_b <- as.data.frame(geofilt) %>%
  filter(DepthCat == 'deep',
         Ecoregion == 'North Carolinean')
geofilt_b$rep[1:(length(geofilt_b$CatalogNumber)/3)] <- 'deep_nc_1'
geofilt_b$rep[((length(geofilt_b$CatalogNumber)/3) + 1):((length(geofilt_b$CatalogNumber)/3)*2)] <- 'deep_nc_2'
geofilt_b$rep[(((length(geofilt_b$CatalogNumber)/3)*2)+1):(length(geofilt_b$CatalogNumber))] <- 'deep_nc_3'

##### South Carolinean #####
geofilt_c <- as.data.frame(geofilt) %>%
  filter(DepthCat == 'shallow',
         Ecoregion == 'South Carolinean')
geofilt_c$rep[1:(length(geofilt_c$CatalogNumber)/3)] <- 'shallow_sc_1'
geofilt_c$rep[((length(geofilt_c$CatalogNumber)/3) + 1):((length(geofilt_c$CatalogNumber)/3)*2)] <- 'shallow_sc_2'
geofilt_c$rep[(((length(geofilt_c$CatalogNumber)/3)*2)+1):(length(geofilt_c$CatalogNumber))] <- 'shallow_sc_3'

geofilt_d <- as.data.frame(geofilt) %>%
  filter(DepthCat == 'deep',
         Ecoregion == 'South Carolinean')
geofilt_d$rep[1:(length(geofilt_d$CatalogNumber)/3)] <- 'deep_sc_1'
geofilt_d$rep[((length(geofilt_d$CatalogNumber)/3) + 1):((length(geofilt_d$CatalogNumber)/3)*2)] <- 'deep_sc_2'
geofilt_d$rep[(((length(geofilt_d$CatalogNumber)/3)*2)+1):(length(geofilt_d$CatalogNumber))] <- 'deep_sc_3'

##### Floridian #####
geofilt_e <- as.data.frame(geofilt) %>%
  filter(DepthCat == 'shallow',
         Ecoregion == 'Floridian')
geofilt_e$rep[1:450] <- 'shallow_fl_1'
geofilt_e$rep[451:901] <- 'shallow_fl_2'
geofilt_e$rep[902:1349] <- 'shallow_fl_3'

geofilt_f <- as.data.frame(geofilt) %>%
  filter(DepthCat == 'deep',
         Ecoregion == 'Floridian')
geofilt_f$rep[1:953] <- 'deep_fl_1'
geofilt_f$rep[954:1907] <- 'deep_fl_2'
geofilt_f$rep[1908:2858] <- 'deep_fl_3'

##### checking #####

table(geofilt_a$rep, useNA = 'always')
table(geofilt_b$rep, useNA = 'always')
table(geofilt_c$rep, useNA = 'always')
table(geofilt_d$rep, useNA = 'always')
table(geofilt_e$rep, useNA = 'always')
table(geofilt_f$rep, useNA = 'always')

##### summary table #####

sum_tbl <-
  as.data.frame(geofilt) %>%
  group_by(DepthCat,Ecoregion ) %>%
  summarize(Genus = toString(unique(Genus)),
    ScientificNames = toString(unique(ScientificName)),
    Records = n(),
    MinDepth = min(DepthInMeters),
    MedianDepth = median(DepthInMeters),
    MaxDepth = max(DepthInMeters)) %>%
  arrange(desc(Records))
View(sum_tbl)

##### Looking at chris kelley data #####
sum_tbl <- indata %>%
  filter(grepl('Kelley', PI) |
           grepl('Kelley', DataContact) |
           grepl('kelley', Reporter)
         ) %>%
  group_by(DatasetID) %>%
  summarize(SurveyID = toString(unique(SurveyID)),
    PI = toString(unique(PI)),
    DataContact = toString(unique(DataContact)),
    Vessel = toString(unique(Vessel)),
    ObservationYear = toString(unique(ObservationYear)),
    Records = n()
  ) %>%
  arrange(desc(Records))
View(sum_tbl)


##### Looking at John Reed data #####
sum_tbl <- filt %>%
  filter(grepl('Brooke', PI) |
           grepl('Brooke', DataContact) |
           grepl('Brooke', Reporter) |
           grepl('Brooke', Citation)
  ) %>%
  group_by(DatasetID) %>%
  summarize(SurveyID = toString(unique(SurveyID)),
            Citation = toString(unique(Citation)),
            PI = toString(unique(PI)),
            DataContact = toString(unique(DataContact)),
            Reporter = toString(unique(Reporter)),
            Vessel = toString(unique(Vessel)),
            ObservationYear = toString(unique(ObservationYear)),
            ObservationDates = toString(unique(ObservationDate)),
            EntryDate = toString(unique(EntryDate)),
            Records = n()
  ) %>%
  arrange(desc(Records))
View(sum_tbl)

##### map it data using leaflet #####
#install.packages("leaflet")
x <- filt %>%
  filter(DatasetID == 'NOAA_PI-11-05-L1'
  )
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("SurveyID:", x$SurveyID, "<br>",
                                    "DatasetID:", x$DatasetID, "<br>",
                                    "ImageURL:", x$ImageURL, "<br>",
                                    "PI:", x$PI, "<br>",
                                    "Observation Year:", x$ObservationYear))
m

setwd("C:/rworking/digs/outdata")
write.csv(x,"NOAA_PI-11-05-L1_from_DSCRTP_NatDB_20171214-0.csv", row.names = F, quote = T)


##### searching and summarizing RACE data #####
sum_tbl <- filt %>%
  filter(grepl('RACE', Citation)
           #grepl('Reed', DataContact) |
           #grepl('Reed', Reporter)
  ) %>%
  group_by(DatasetID) %>%
  summarize(DataProvider = toString(unique(DataProvider)),
            IdentificationQualifier = toString(unique(IdentificationQualifier)),
            IdentifiedBy = toString(unique(IdentifiedBy)),
            LocationAccuracy = toString(unique(LocationAccuracy)),
            NavType = toString(unique(NavType)),
            SampleID = toString(SampleID),
            SurveyID = toString(unique(SurveyID)),
            Citation = toString(unique(Citation)),
            PI = toString(unique(PI)),
            DataContact = toString(unique(DataContact)),
            Reporter = toString(unique(Reporter)),
            Vessel = toString(unique(Vessel)),
            ObservationYear = toString(unique(ObservationYear)),
            ObservationDates = toString(unique(ObservationDate)),
            EntryDate = toString(unique(EntryDate)),
            Records = n()
  ) %>%
  arrange(desc(Records))

View(sum_tbl)

##### Summary by Vessel and VehicleName #####
##### searching and summarizing #####
sum_tbl <- filt %>%
   filter(
     # CatalogNumber == '412472',
     grepl('USGS_CoWCoG', DatasetID)
     ) %>%
  group_by(DataProvider) %>%
  summarize(#DataProvider = toString(unique(Repository)),
            Repository = toString(unique(Repository)),
            Vessel = toString(unique(Vessel)),
            PI = toString(unique(PI)),
            SampleID = toString(unique(SampleID)),
            ObservationDate = toString(unique(ObservationDate)),
            ScientificName = toString(unique(ScientificName)),
            ImageFilePath = toString(unique(ImageFilePath)),
            Flag = toString(unique(Flag)),
            Records = n()
            ) %>%
  arrange(desc(Records))
View(sum_tbl)

setwd("C:/rworking/digs/outdata")
write.csv(sum_tbl,"20180302_0_Vessel_with_ZeussII_DSCRTP_NatDB_20171214-0.csv", row.names = F, quote = T)

##### filter and map #####
x <- sum_tbl #%>%
  # filter(#grepl('EX', Citation)
  #   #grepl('PC10-02', SurveyID)
  #   #grepl('Reed', DataContact) |
  #   #grepl('Reed', Reporter)
  # )

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap")
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "blue",
                      fillOpacity=1,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", x$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", x$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", x$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", x$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", x$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", x$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", x$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", x$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", x$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", x$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", x$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", x$EventID, "<br>",
                        "<b><em>","Image:","</b></em>",x$ImageURL)

)
m





##### table of schema #####
names(s)
sum_tbl <-
  s %>%
  #filter(PrimaryFields == '1')%>%
  group_by(DSCRTPCategory, DSCRTPGroup) %>%
  summarize(Field_Names = toString(unique(FieldName)))
# sum_tbl <- kable(sum_tbl,
#                  row.names = F,
#                  digits = 2,
#                  col.names = c('Field Group', 'Field Name'))
View(sum_tbl)

# setwd("C:/rworking/digs/outdata")
# write.csv(sum_tbl,"20180228_0_Field_Summary_Schema_OBISsection_RPMcGuinn.csv",
#           row.names = F,
#           quote = T)

##### _____ comparing new to old schema #####
##### download Google Sheet version of schema for use in R  documents #####
# Register and download Google Sheet
s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
schema<- gs_read(s)
#gs_browse(s)

setwd("C:/rworking/digs/indata")
write.csv(schema, "2018_DSCRTP_Schema.csv")

setwd("C:/rworking/digs/indata")
s <- read.csv("2018_DSCRTP_Schema.csv", header = T)
s2 <- read.csv("20170707_0_DSCRTP_Schema.csv", header = T)

setdiff(s$FieldName,s2$FieldName)

setdiff(s2$FieldName,s$FieldName)

##### _____ looking @bugs in version DSCRTP_NatDB_20171214-0 #####
x <- indata %>%
filter(grepl('Thoma', DataProvider)
  # grepl('PC10-02', SurveyID)
  # grepl('Reed', DataContact) |
  # grepl('Reed', Reporter)
)

# checking
table(factor(x$DataContact))

library(leaflet)

##### _____ Leaflet data exploration #####
##### Bringing in input datasets #####

#from csv
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20180327-4.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")


##### filter the big data #####
x <- indata %>%
  filter(Latitude > 24,
         Latitude < 25,
         Longitude < -83,
         Longitude > -84
        # DepthInMeters > 500,
        # DepthInMeters < 700,
        #Flag == "0",
    )

View(x)
#### making a predined pallete based on a certain domain of values (in this case colored by the Flag variable) #####

pal <- colorFactor(
  palette = 'Dark2',
  domain = x$Flag
)

##### making a leaflet map and coloring by the predefined pallet above #####
library(leaflet)
leaflet(x) %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addCircleMarkers(radius=5,
                   weight=0,
                   fillOpacity=1,
                   color = ~pal(Flag),
                   popup = paste(
                     "<b><em>","Flag:","</b></em>", x$Flag, "<br>",
                     "<b><em>","FlagReason:","</b></em>", x$FlagReason, "<br>",
                     "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                     "<b><em>","Record Type:","</b></em>", x$RecordType, "<br>",
                     "<b><em>","DatasetID:","</b></em>", x$DatasetID, "<br>",
                     "<b><em>","AccessionID:","</b></em>", x$AccessionID, "<br>",
                     "<b><em>","DataProvider:","</b></em>", x$DataProvider, "<br>",
                     "<b><em>","ObservationYear:","</b></em>", x$ObservationYear, "<br>",
                     "<b><em>","Vessel:","</b></em>", x$Vessel, "<br>",
                     "<b><em>","Locality:","</b></em>", x$Locality, "<br>",
                     "<b><em>","Scientific Name:","</b></em>", x$ScientificName, "<br>",
                     "<b><em>","Depth (meters):","</b></em>", x$DepthInMeters, "<br>",
                     "<b><em>","Survey ID:","</b></em>", x$SurveyID, "<br>",
                     "<b><em>","Event ID:","</b></em>", x$EventID, "<br>",
                     "<b><em>","Image:","</b></em>",x$ImageURL)

  )

table
##### help on colors and size with leaflet #####
# Fake data
df <- data.frame(lng = c(-5, -10, -15, -20, 25),
                 lat = c(8, 12, 33, 4, 18),
                 size = c(200000, 100000, 800000, 250000, 350000),
                 popup = c('A', 'B', 'C', 'D', 'E'),
                 type = c('A', 'B', 'C', 'D', 'E'),
                 stringsAsFactors = FALSE)

# If you want to set your own colors manually:
pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = df$type
)

# If you want to use predefined palettes in the RColorBrewer package:
# Call RColorBrewer::display.brewer.all() to see all possible palettes
pal <- colorFactor(
  palette = 'Dark2',
  domain = df$type
)

leaflet(df) %>%
  addTiles() %>%
  addCircles(lng = ~lng, lat = ~lat, weight = 1,
             radius = ~size, popup = ~popup, color = ~pal(type))

##### _____ Exploring new minimum and maximum size variable in the database #####
##### filter the big data #####
x <- filt %>%
  filter(MinimumSize > 0,
         MaximumSize > 0
  )

summary(filt$MaximumSize)
summary(x$MaximumSize)
x$SizeDiff <- x$MaximumSize-x$MinimumSize
table(x$SizeDiff == 0)


#####_____ spocc: SpatialPolygons/SpatialPolygonsDataFrame integration #####
#install.packages("spocc")
library(spocc)
library(sp)
## Single polygon in SpatialPolygons class
# 83 W and 84 W and 24 N and 25 N only within the 1800 to 2200 ft depth contours.
# [min-longitude, min-latitude, max-longitude, max-latitude].
bounds <- c(-84, 24, -83, 25)
out <- occ(geometry = bounds, limit=5000, from = 'obis')
yo <- as.data.frame(out$obis$data)

library(leaflet)
leaflet(yo) %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addCircleMarkers(radius=5,
                   weight=0,
                   fillOpacity=1)

summary(yo$longitude)
plot(yo$longitude)
names(yo)
View(yo)
table(yo$phylum)

yo %>% group_by(phylum)

##### make a shapefile with the OGR driver #####
library(rgdal)
coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)

##### _____ Creating polygon from bounding coordinates #####
# [(x_min, y_min), (x_max, y_min), (x_max, y_max), (x_max, y_min), (x_min, y_min)]

x_min <- -84
x_max <- -83
y_min <- 24
y_max <- 25

library(sp)
e <- as(raster::extent(x_min, x_max, y_min, y_max), "SpatialPolygons")
proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

data = data.frame(f=99.9)
spdf = SpatialPolygonsDataFrame(e,data)

setwd("C:/rworking/digs/outdata")
writeOGR(spdf, dsn="spdf",
         layer= "spdf",
         driver = "ESRI Shapefile",
         overwrite_layer = T)


##### getting marmap data #####
x_min <- -84
x_max <- -83
y_min <- 24
y_max <- 25


#install.packages("marmap")
library(marmap)
crab <- getNOAA.bathy(lon1 = -82, lon2 = -85,
                        lat1 = 23, lat2 = 26, resolution = 4)

# Creating color palettes
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))



plot(crab, image = TRUE, land = TRUE, lwd = 0.03,
     bpal = list(c(0, max(crab), greys),
                 c(min(crab), 0, blues)))


points(indata$Longitude, indata$Latitude, pch = 21, col = "black",
       bg = "yellow", cex = 1.3)
text(152, -7.2, "New Britain\nTrench", col = "white", font = 3)


points(yo$longitude, yo$latitude, pch = 21, col = "black",
       bg = "red", cex = .9)
text(152, -7.2, "New Britain\nTrench", col = "white", font = 3)


##### look at data with images
x <- indata %>%
  filter(is.na(ImageURL) == F)  %>%
  group_by(DataProvider, DatasetID, DataContact, Reporter, FlagReason) %>%
  summarize(
    n = n()
  )
View(x)
sum(x$n)


##### big gomex subset #####
sub <- subset(filt, as.numeric(Latitude) > 15.00000 &
                   as.numeric(Latitude) < 37.00000 &
                   as.numeric(Longitude) < -64.0000 &
                   as.numeric(Longitude) > -98.00000)

##### #### making a predined pallete based on a certain domain of values (in this case colored by the Flag variable) #####

pal <- colorFactor(
  palette = 'Dark2',
  domain = x$VernacularNameCategory
)

##### making a leaflet map and coloring by the predefined pallet above #####
library(leaflet)
leaflet(x) %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addCircleMarkers(radius=5,
                   weight=0,
                   fillOpacity=1,
                   color = ~pal(Flag),
                   popup = paste(
                     "<b><em>","Flag:","</b></em>", x$Flag, "<br>",
                     "<b><em>","FlagReason:","</b></em>", x$FlagReason, "<br>",
                     "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                     "<b><em>","Record Type:","</b></em>", x$RecordType, "<br>",
                     "<b><em>","DatasetID:","</b></em>", x$DatasetID, "<br>",
                     "<b><em>","AccessionID:","</b></em>", x$AccessionID, "<br>",
                     "<b><em>","DataProvider:","</b></em>", x$DataProvider, "<br>",
                     "<b><em>","ObservationYear:","</b></em>", x$ObservationYear, "<br>",
                     "<b><em>","Vessel:","</b></em>", x$Vessel, "<br>",
                     "<b><em>","Locality:","</b></em>", x$Locality, "<br>",
                     "<b><em>","Scientific Name:","</b></em>", x$ScientificName, "<br>",
                     "<b><em>","Depth (meters):","</b></em>", x$DepthInMeters, "<br>",
                     "<b><em>","Survey ID:","</b></em>", x$SurveyID, "<br>",
                     "<b><em>","Event ID:","</b></em>", x$EventID, "<br>",
                     "<b><em>","Image:","</b></em>",x$ImageURL)

  )


##### make a shapefile with the OGR driver #####
library(rgdal)
coordinates(x) <- c("Longitude", "Latitude")
proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x",
         layer= "x",
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"),
         overwrite_layer = T)



##### bring in table of obis data #####
setwd("C:/rworking/digs/indata")
x<-read.csv("x.csv", header = T)
names(x)


##### Write big #####

setwd("C:/rworking/digs/outdata")
write.csv(big,"GOMEX_subset_DSCRTP_NatDB_20180405-0.csv", row.names = F, quote = T)

##### depth checking with leaflet #####
##### making a leaflet map of the results #####
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=7,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("Scientific Name:", x$ScientificName, "<br>",
                                    "CRM:", x$gisCRMDepth, "<br>",
                                    "GEBCO:", x$gisGEBCODepth, "<br>",
                                    "ETOPO1:", x$gisEtopoDepth, "<br>",
                                    "DepthInMeters:", x$DepthInMeters,"<br>",
                                    "Locality:", x$Locality, "<br>",
                                    "Latitude:", x$Latitude, "<br>",
                                    "Longitude:", x$Longitude, "<br>",
                                    "Dataset ID:", x$DatasetID, "<br>",
                                    "Survey ID:", x$SurveyID, "<br>",
                                    "CatalogNumber:", x$CatalogNumber, "<br>",
                                    "Vessel:", x$Vessel, "<br>",
                                    "SampleID:", x$SampleID, "<br>",
                                    "ObservationYear:", x$ObservationYear, "<br>",
                                    "Flag:", x$Flag, "<br>",
                                    "Flag Reason:", x$FlagReason))
m

##### write a shapefile #####
library(rgdal)
d <- dx
coordinates(d) <- c("Longitude", "Latitude")
proj4string(d) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
writeOGR(obj=d, dsn= "tempdir", layer="y", driver="ESRI Shapefile")
#getwd()

setwd("C:/rworking/digs/outdata")
writeOGR(d, dsn="x.kml", layer= "DSC_RTP", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

##### filtering #####

x <- sub %>%
  filter(
    DepthInMeters == "7999"
    #grepl("Okeanos", Vessel),
    )  %>%
  #select(DataProvider, Repository, PI, DataContact, FlagReason)
  group_by(Longitude, Latitude, DataProvider, DatasetID, DataContact, Reporter, FlagReason, CatalogNumber, ImageURL) %>%
  summarize(n = n(),
            noflag = length(Flag[Flag == "0"]),
            flagged = length(Flag[Flag == "1"]),
            p_flagged = flagged/n,
            n_coral = length(Phylum[Phylum == "Cnidaria"]),
            n_sponges = length(Phylum[Phylum == "Porifera"]),
            start = min(as.numeric(ObservationYear[ObservationYear != -999])),
            end = max(as.numeric(ObservationYear[ObservationYear != -999])),
            n_orders = length(unique(Order)),
            imagefilepath = length(ImageFilePath[is.na(ImageFilePath) == F]),
            imageurl = length(ImageURL[is.na(ImageURL) == F]))
x
##### making a leaflet map and coloring by the predefined pallet above #####
library(leaflet)
leaflet(x) %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addCircleMarkers(radius=5,
                   weight=0,
                   fillOpacity=1,
                   popup = paste(
                     "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                     "<b><em>","Image:","</b></em>",x$ImageURL)

  )

##### general summary #####

sum_tbl <- filt %>%
  filter(
    # grepl('895396', CatalogNumber),
    grepl('Brown', Vessel)
    # grepl('Rooper', PI),
    # grepl('camera', RecordType),
    # grepl('kelley', Reporter)
  ) %>%
  group_by(DepthInMeters) %>%
  summarize(
    DataProvider = toString(unique(DataProvider)),
    Survey_ID = toString(unique(SurveyID)),
    Sampling_Equipment = toString(unique(SamplingEquipment)),
    Record_Type = toString(unique(RecordType)),
    PI_ = toString(unique(PI)),
    Data_Contact = toString(unique(DataContact)),
    Vessel_ = toString(unique(Vessel)),
    Observation_Year = toString(unique(ObservationYear)),
    Records = n()
  ) %>%
  arrange(desc(Records))
View(sum_tbl)


##### ScientificName #####

setwd("C:/rworking/digs/indata")
list <- read.csv("scientificname.csv", header = T)
names(list)

setwd("C:/rworking/digs/indata")
mcz <- read.csv("mcz.csv", header = T)
names(mcz)

tax <- subset(mcz, mcz$ScientificName %in% list$ScientificName, select = 4:22)
tax <- tax %>% distinct(ScientificName, .keep_all = T)
tax$ScientificName

setwd("C:/rworking/digs/outdata")
write.csv(tax,"tax.csv", row.names = F, quote = T)



##### Citations #####
sum_tbl <- filt %>%
#  filter(
#    DatasetID == 'BOEM_RB-09-05'
#  ) %>%
  group_by(DatasetID, AccessionID, Citation) %>%
  summarize(
    minObservationYear = min(as.numeric(ObservationYear)),
    maxObservationYear = max(as.numeric(ObservationYear)),
    SurveyID = toString(unique(SurveyID)),
    RecordType = toString(unique(RecordType)),
    PI = toString(unique(PI)),
    WebSite = toString(unique(WebSite)),
    DataProvider = toString(unique(DataProvider)),
    Repository = toString(unique(Repository)),
    DataContact = toString(unique(DataContact)),
    Reporter = toString(unique(Reporter)),
    Vessel = toString(unique(Vessel)),
    VehicleName = toString(unique(VehicleName)),
    n = n()
  )

sum_tbl$CitationMaker <- paste(sum_tbl$DataProvider,'. ',
                               sum_tbl$minObservationYear,' to ',
                               sum_tbl$maxObservationYear,'. ',
                               'Coral or sponge occurrence observations submitted to the NOAA National Database for Deep Sea Corals (www.deepseacoraldata.noaa.gov)', '. ',
                               'DSCRTP Dataset ID: ', sum_tbl$DatasetID, '. ',
                               'DSCRTP Accession ID: ',sum_tbl$AccessionID, '. ',
                               'Record type: ', sum_tbl$RecordType, '. ',
                               'Vessel(s): ', sum_tbl$Vessel,'. ',
                               'Sampling vehicle: ', sum_tbl$VehicleName,'. ',
                               'Survey ID: ', sum_tbl$SurveyID,'. ',
                               'Principle investigator: ', sum_tbl$PI,'. ',
                               'Data contact: ', sum_tbl$DataContact,'. ',
                               'Reporter: ', sum_tbl$Reporter,'. ',
                               'Repository: ', sum_tbl$Repository,'. ',
                               'Web site [last accessed on YYYY-MM-DD]: ', sum_tbl$WebSite,'.',
                               sep = '')

cite <- sum_tbl %>%
  ungroup() %>%
  select(DataProvider, DatasetID, AccessionID,Citation, CitationMaker, n)

setwd("C:/rworking/digs/outdata")
write.csv(cite,"20180702_0_CitationMaker_DSCRTP_NatDB_20180405-0.csv", row.names = F, quote = T)

##### _____datasetid exploration #####

x <- indata %>%
  filter(DatasetID == "BOEM_RB-09-07"
         #Flag == "0"#,
         #Oxygen != "-999"
  )  %>%
  group_by(ObservationDate, DatasetID, DataProvider, Repository, Citation,
           DataContact, Reporter, PI, Vessel, VehicleName, SurveyID, Locality, ObservationYear) %>%
  summarize(
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    n_records = n()
            # MinYear = min(as.numeric(ObservationYear)),
            # MaxYear = max(as.numeric(ObservationYear)),
            # MinOxygen = min(as.numeric(Oxygen)),
            # MaxOxygen = max(as.numeric(Oxygen))
            #             MinSalinity = min(as.numeric(Salinity)),
            #             MaxSalinity = max(as.numeric(Salinity)),
            #             Min_pH = min(as.numeric(pH)),
            #             # Max_pH = max(as.numeric(pH))

  )

View(x)

##### map  data using leaflet #####
#install.packages("leaflet")
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("SurveyID:", x$SurveyID, "<br>",
                                    "DatasetID:", x$DatasetID, "<br>",
                                    # "ImageURL:", x$ImageURL, "<br>",
                                    "PI:", x$PI, "<br>",
                                    "Observation Year:", x$ObservationYear))
m

##### _____taxa exploration #####
x <- indata %>%
  filter(grepl("Eugorgia rubens", ScientificName)
  )  %>%
  group_by(ObservationDate, DatasetID, DataProvider, Repository, Citation,
           DataContact, Reporter, PI, Vessel, VehicleName, SurveyID, Locality,
           ObservationYear, ScientificName, DepthInMeters, ImageURL
  ) %>%
  summarize(
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    n_records = n()
    # MinYear = min(as.numeric(ObservationYear)),
    # MaxYear = max(as.numeric(ObservationYear)),
    # MinOxygen = min(as.numeric(Oxygen)),
    # MaxOxygen = max(as.numeric(Oxygen))
    #             MinSalinity = min(as.numeric(Salinity)),
    #             MaxSalinity = max(as.numeric(Salinity)),
    #             Min_pH = min(as.numeric(pH)),
    #             # Max_pH = max(as.numeric(pH))

  )

#View(x)

z <- indata %>%
  filter(grepl("Coenocyathus bowersi", ScientificName)) %>%
  group_by(ObservationDate, DatasetID, DataProvider, Repository, Citation,
           DataContact, Reporter, PI, Vessel, VehicleName, SurveyID, Locality,
           ObservationYear, ScientificName, DepthInMeters, ImageURL
  ) %>%
  summarize(
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    n_records = n()
    # MinYear = min(as.numeric(ObservationYear)),
    # MaxYear = max(as.numeric(ObservationYear)),
    # MinOxygen = min(as.numeric(Oxygen)),
    # MaxOxygen = max(as.numeric(Oxygen))
    #             MinSalinity = min(as.numeric(Salinity)),
    #             MaxSalinity = max(as.numeric(Salinity)),
    #             Min_pH = min(as.numeric(pH)),
    #             # Max_pH = max(as.numeric(pH))
  )

##### write it out #####
setwd("C:/rworking/digs/outdata")
write.csv(z,"20180803_0_Coenocyathus_bowersi_DSCRTP_NatDB_20180718-0.csv", row.names = F, quote = T)

write.csv(x,"20180803_0_Eugorgia_rubens_DSCRTP_NatDB_20180718-0.csv", row.names = F, quote = T)

##### map it data using leaflet #####
#install.packages("leaflet")
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("ScientificName:", x$ScientificName, "<br>",
                                    "SurveyID:", x$SurveyID, "<br>",
                                    "DatasetID:", x$DatasetID, "<br>",
                                    "ImageURL:", x$ImageURL, "<br>",
                                    "PI:", x$PI, "<br>",
                                    "Observation Year:", x$ObservationYear))
m

##### _____converting between coordinate units#####
install.packages("measurements")
library(measurements)

##### deg_dec_min to dec_deg #####
x <- read.table(
text =
"lat long
1 30°25.264 9°01.331",
header = TRUE, stringsAsFactors = FALSE)

# change the degree symbol to a space
x$lat = gsub('°', ' ', x$lat)
x$long = gsub('°', ' ', x$long)

# convert from decimal minutes to decimal degrees
x$lat = measurements::conv_unit(x$lat, from = 'deg_dec_min', to = 'dec_deg')
x$long = measurements::conv_unit(x$long, from = 'deg_dec_min', to = 'dec_deg')

##### ____ creating AOI dot deg_min_sec to dec_deg #####
y <- read.table(
  text =
    "lat long
  1 37°20'48.009  -122°38'34.114",
  header = TRUE, stringsAsFactors = FALSE)

# change the degree symbol to a space
y$lat = gsub('°', ' ', y$lat)
y$long = gsub('°', ' ', y$long)
y$lat = gsub('\'', ' ', y$lat)
y$long = gsub('\'', ' ', y$long)

# convert from decimal minutes to decimal degrees
y$lat = measurements::conv_unit(y$lat, from = 'deg_min_sec', to = 'dec_deg')
y$long = measurements::conv_unit(y$long, from = 'deg_min_sec', to = 'dec_deg')

# convert to numbers
y$lat <- as.numeric(y$lat)
y$long <- as.numeric(y$long)

##### _____map both AOI and data using leaflet #####
#install.packages("leaflet")
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("ScientificName:", x$ScientificName, "<br>",
                                    "DepthInMeters:", x$DepthInMeters, "<br>",
                                    "SurveyID:", x$SurveyID, "<br>",
                                    "DatasetID:", x$DatasetID, "<br>",
                                    "ImageURL:", x$ImageURL, "<br>",
                                    "PI:", x$PI, "<br>",
                                    "Observation Year:", x$ObservationYear))

m <- addCircleMarkers(m, data=z,
                      radius=5,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = paste("ScientificName:", z$ScientificName, "<br>",
                                    "DepthInMeters:", x$DepthInMeters, "<br>",
                                    "SurveyID:", z$SurveyID, "<br>",
                                    "DatasetID:", z$DatasetID, "<br>",
                                    "ImageURL:", z$ImageURL, "<br>",
                                    "PI:", z$PI, "<br>",
                                    "Observation Year:", z$ObservationYear))
m <- addCircleMarkers(m, data=y,
                      radius=8,
                      weight=0,
                      fillColor= "black",
                      fillOpacity=1)


m


##### _____google earth kmls #####
x$Longitude <- as.numeric(x$Longitude)
x$Latitude <- as.numeric(x$Latitude)
z$Longitude <- as.numeric(z$Longitude)
z$Latitude <- as.numeric(z$Latitude)

library(rgdal)
coordinates(x)<- c("Longitude", "Latitude")
proj4string(x)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"

coordinates(z)<- c("Longitude", "Latitude")
proj4string(z)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/digs/outdata")
writeOGR(x, dsn="x.kml", layer= "DSC_RTP", driver="KML",overwrite_layer = T)
setwd("C:/rworking/digs/outdata")
writeOGR(z, dsn="z.kml", layer= "DSC_RTP", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

##### _____joining ObservationDate to AKFIN data #####
##### loading cruise (survey) table #####
setwd("C:/rworking/digs/indata")
cruise <- read.csv("cruise_table.csv", header = T)

##### loading vessel table #####
setwd("C:/rworking/digs/indata")
vessel <- read.csv("vesselcodes.csv", header = T)

##### loading newrace data #####
setwd("C:/rworking/digs/indata")
newrace <- read.csv("20180806-0_RACE_Trawl_Survey_2013_2016.csv", header = T)

# versions
# 20180517-0
# 20180806-0
# 20180212_0

##### checking newrace #####
# names(newrace)
# length(newrace$SamplingEquipment)
# table(race$SamplingEquipment, useNA = 'always')
# table(newrace$VerbatimScientificName, useNA = 'always')
# table

# head(
  x <- newrace %>%
    filter(is.na(VerbatimScientificName) == TRUE | VerbatimScientificName == '') %>%
    # dplyr::select(ScientificName,
    #               VerbatimScientificName,
    #               DepthInMeters,SurveyID,
    #               Vessel, EventID, ObservationDate) %>%
    group_by(SurveyID) %>%
    summarize(n = n(),
              # Vessel = toString(unique(Vessel)),
              EventID = toString(unique(EventID)),
              ObservationDate = toString(unique(ObservationDate))
              )
  # , n = 50
# )


##### getting RACE data from existing db #####

race <- filt %>%
  filter(
    # grepl('RACE', SurveyID) |
    # grepl('RACE', Purpose) |
    grepl('NOAA_AFSC_Bottom_Trawl_Survey',DatasetID))
  # ) %>%
  # group_by(SurveyID, Vessel, Locality) %>%
  # summarize(n = n())


longline <- indata %>%
  filter(
    # grepl('RACE', SurveyID) |
    # grepl('RACE', Purpose) |
    grepl('NOAA_AFSC_Longline_Survey',DatasetID )
  )


# check
table(factor(race$DatasetID))
table(factor(race$SurveyID))
table(factor(longline$DatasetID))
summary(race$ObservationYear)
summary(longline$ObservationYear)
View(race)

##### merging back vessel number #####
join <- merge(newrace, vessel, by.x = "Vessel", by.y = "vesselname", all.x = T)
join <- merge(join, cruise, by.x=c("vesselnum", "SurveyID"), by.y=c("vesselnum", "surveyid"))
##### put the ObservationDates on #####
join$ObservationDate <- substring(join$start,1,10)
##### strip column names that don't conform #####
newrace2<-join[,c(names(newrace))]

##### add reporter comments #####
newrace2$ReporterComments <- "ObservationDate is set as the survey start date"
names(newrace2)

##### write the new file #####
setwd("C:/rworking/digs/outdata")
write.csv(newrace2, '20180806-0_RACE_Trawl_Survey_2013_2016.csv')

##### _____ Comprehensive data search for Peter Etnoyer and Bob Podolosky #####
##### restricted selection #####
ecoregions <- c('Hawaii', 'Gulf of Alaska', 'Oregon, Washington, Vancouver Coast and Shelf',
                'Northern California', 'Southern California Bight', 'Northern Gulf of Mexico',
                'Floridian', 'Gulf of Maine/Bay of Fundy', 'Scotian Shelf',
                'Virginian', 'Carolinian')

x <- filt %>%
  filter(
    #is.na(Habitat) == FALSE,
    gisMEOW %in% ecoregions,
    ObservationYear != '-999',
    Temperature != "-999",
    Oxygen != "-999",
    Salinity != "-999",
    as.numeric(Oxygen) > 0,
    as.numeric(Oxygen) < 30
  )   %>%
  dplyr::select(gisMEOW,
                CatalogNumber,
                DatasetID,
                DataProvider,
                SampleID,
                SurveyID,
                Vessel,
                VehicleName,
                RecordType,
                ObservationYear,
                TaxonRank,
                Class, Subclass, Order, Family, Genus, Species, ScientificName, AphiaID,
                VernacularNameCategory,
                Latitude,
                Longitude,
                DepthInMeters,
                IndividualCount,
                CategoricalAbundance,
                Habitat, Substrate,
                Salinity,
                Oxygen,
                Temperature,
                ImageURL,
                WebSite
  )

View(x)
setwd("C:/rworking/digs/outdata")
write.csv(x, '20180827_1_Restricted_Oxygen_Salinity_Temp_Podolsky_subset_DSCRTP_NatDB_20180806-0_RPMcGuinn.csv')


y <- x %>%
  group_by(
    gisMEOW
  ) %>%
  summarize(
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    n_records = n(),
    MinDepth = min(as.numeric(DepthInMeters)),
    MaxDepth = max(as.numeric(DepthInMeters)),
    MinYear = min(as.numeric(ObservationYear)),
    MaxYear = max(as.numeric(ObservationYear)),
    MinOxygen = min(as.numeric(Oxygen)),
    MaxOxygen = max(as.numeric(Oxygen)),
    MinTemperature = min(as.numeric(Temperature)),
    MaxTemperature = max(as.numeric(Temperature)),
    MinSalinity = min(as.numeric(Salinity)),
    MaxSalinity = max(as.numeric(Salinity))
    # Habitat = toString(unique(Habitat))
  )

View(y)
setwd("C:/rworking/digs/outdata")
write.csv(y, '20180827_1_Summary_Restricted_Oxygen_Salinity_Temp_Podolsky_subset_DSCRTP_NatDB_20180806-0_RPMcGuinn.csv')


##### full selection #####
ecoregions <- c('Hawaii', 'Gulf of Alaska', 'Oregon, Washington, Vancouver Coast and Shelf',
                'Northern California', 'Southern California Bight', 'Northern Gulf of Mexico',
                'Floridian', 'Gulf of Maine/Bay of Fundy', 'Scotian Shelf',
                'Virginian', 'Carolinian')

# Creating full data selection
x <- filt %>%
  filter(
    gisMEOW %in% ecoregions,
    ObservationYear != '-999'
  )  %>%
  dplyr::select(gisMEOW,
                CatalogNumber,
                DatasetID,
                DataProvider,
                SampleID,
                SurveyID,
                Vessel,
                VehicleName,
                RecordType,
                ObservationYear,
                TaxonRank,
                Class, Subclass, Order, Family, Genus, Species, ScientificName, AphiaID,
                VernacularNameCategory,
                Latitude,
                Longitude,
                DepthInMeters,
                IndividualCount,
                CategoricalAbundance,
                Habitat, Substrate,
                Salinity,
                Oxygen,
                Temperature,
                ImageURL,
                WebSite
                )
#View(x)
setwd("C:/rworking/digs/outdata")
write.csv(x, '20180827_1_Podolsky_subset_DSCRTP_NatDB_20180806-0_RPMcGuinn.csv')

# Creating the group summary

y <- x %>%
  group_by(gisMEOW) %>%
  summarize(
    n_records = n(),
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    MinDepth = min(as.numeric(DepthInMeters)),
    MaxDepth = max(as.numeric(DepthInMeters)),
    MinYear = min(as.numeric(ObservationYear)),
    MaxYear = max(as.numeric(ObservationYear))
  )

#View(y)
setwd("C:/rworking/digs/outdata")
write.csv(y, '20180827_1_summary_by_MEOW_Podolsky_subset_DSCRTP_NatDB_20180806-0_RPMcGuinn.csv')


##### map it data using leaflet #####
#install.packages("leaflet")
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("ScientificName:", x$ScientificName, "<br>",
                                    "SurveyID:", x$SurveyID, "<br>",
                                    "DatasetID:", x$DatasetID, "<br>",
                                    "ImageURL:", x$ImageURL, "<br>",
                                    "PI:", x$PI, "<br>",
                                    "Observation Year:", x$ObservationYear))
m

##### write the new file #####
setwd("C:/rworking/digs/outdata")
write.csv(x, '20180807-0_Extract_from_DSCRTP_NatDB_20180718-0_RPMcGuinn.csv')

##### _____ DatasetID dashboard project #####
##### set the datasetID dashboard url #####
DatasetID <- "BOEM_NF-08-14-L2"
dash <- paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/",DatasetID,".html", sep = '')
browseURL(dash)

##### _____ Looking at specific DatasetID #####
d <- indata %>%
  filter(
    # Flag == "0",
    # DatasetID == "Thoma_J_2013" |
    # DatasetID == "MCZ-IZ"
    grepl('MCZ', DatasetID)
      # DatasetID == "NOAA_EX-15-04-L2" |
      # DatasetID == "NOAA_AFSC_Longline_Survey"
  )
##### map it data using leaflet #####
#install.packages("leaflet")
library(leaflet)
m <- leaflet() %>% setView(lng = -97,lat = 38, zoom = 2)
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=d,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("ScientificName:", d$ScientificName, "<br>",
                                    "SurveyID:", d$SurveyID, "<br>",
                                    "DatasetID:", d$DatasetID, "<br>",
                                    "ImageURL:", d$ImageURL, "<br>",
                                    "PI:", d$PI, "<br>",
                                    "Observation Year:", d$ObservationYear))
m



##### _____ exploring HURL data #####
##### extracting HURL data #####
x <- filt %>%
  filter(
    grepl("HURL", DatasetID)
  ) %>%
  group_by(DatasetID) %>%
  summarize(n=n())
View(x)

##### _____ making a new datasetID key file
##### bringing in and transforming the old key #####
library(tidyr)
setwd("C:/rworking/digs/indata")
oldkey <- read.csv('20171214-0_DatasetID_key.csv', header = TRUE)
#get rid of some bad data
oldkey <- oldkey[-46,1:2]
# get rid of NA values
oldkey <- oldkey[is.na(oldkey$DatasetID) == F,]
oldkey <- oldkey %>%
  separate(Full.DatasetID, c("class", "title"), ":")

##### extracting key values from current database #####
key <- filt %>%
  group_by(DatasetID) %>%
  summarise(n = n())

key <- ungroup(key)

# class(key)
key <- data.frame(key)

# # check
# setdiff(oldkey$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, oldkey$DatasetID)
# length(oldkey$DatasetID)
# length(key$DatasetID)

# making the full key by matching the titles and classes from the oldkey
# missing classes assigned NA for now
key <- merge(key, oldkey, all.x = T)
# View(x)

##### _____ DatasetID explorations #####
##### download google sheet (output: key) #####
key <- gs_key('1CTaYLwSDqA6ZL_GzlO-oYWo9k_NgAkNTvwkzl1KW_6w')
key <- gs_read(key)
#gs_browse(s)

key <- data.frame(key)

##### compare with DatasetID in DB #####
setdiff(unique(filt$DatasetID), key$DatasetID)
setdiff(key$DatasetID, unique(filt$DatasetID))
##### _____ intake patch for with Alaska Groundfish Survey Data #####
##### bring in data ######
# working with the file 'goa2015_2017.csv'
# from https://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
setwd("C:/rworking/digs/indata")
d <- read.csv("bsslope2002_2016.csv", header = T) # bsslope2002_2016.csv
d1 <- read.csv("goa2015_2017.csv", header = T)
##### checker #####
summary(d)
names(d)
table(d$BOT_TEMP, useNA = 'always')
setdiff(names(d1), names(d))
setdiff(names(d), names(d1))

##### apply filter to temperature data #####
d1 <- d %>%
  filter(BOT_TEMP != -9999,
         SURF_TEMP != -9999)

##### checker #####
summary(d1$BOT_TEMP)
summary(d1$SURF_TEMP)
summary(d1$BOT_DEPTH)

##### reassign column names #####
names(d)
names(d) <- c('Latitude', 'Longitude', 'Station', 'STRATUM',
                  'ObservationYear', 'ObservationDate', 'WTCPUE', 'NUMCPUE',
                  'VernacularName', 'ScientificName', 'SID', 'DepthInMeters',
                  'Temperature', 'SURF_TEMP', 'Vessel', 'SurveyID', 'EventID')
##### checker #####
names(d)
head(d$ScientificName)

##### filter to specific species #####
head(d$ScientificName)

d2 <- d %>% filter(ScientificName == 'Pagurus rathbuni')

##### _____ map it data using leaflet #####
#install.packages("leaflet")
x <- d2

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste("ScientificName:", x$ScientificName, "<br>",
                                    "SurveyID:", x$SurveyID, "<br>",
                                    "TrackinID:", x$TrackingID, "<br>",
                                    "Station:", x$Station, "<br>",
                                    "Observation Year:", x$ObservationYear))
m






##### _____ Boxplot of depth by FishCouncilRegion #####
filt2 <- filt %>%
  filter(DepthInMeters != "-999")

g <- ggplot(filt2, aes(factor(FishCouncilRegion), as.numeric(DepthInMeters), color = Phylum)) +   geom_boxplot() +
  scale_y_reverse() +
  ylab("Depth (meters)") +
  xlab("FishCouncilRegion") +
  theme_bw(base_size = 15, base_family = "Cambria") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#display.brewer.all(colorblindFriendly=TRUE)
g + scale_color_manual(values = brewer.pal(12, "Paired")[c(10,9)])


##### _____ histograms in ggplot2 #####
ggplot(mbari, aes(x=DepthInMeters, color=Order)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")+
  scale_x_continuous(limits = c(-5, 50))+
  scale_y_continuous(limits = c(0,10))

##### _____ mbari data checker #####
yo <- pac %>% filter(DepthInMeters < 30, DepthInMeters > 25) %>%
  dplyr::select(CatalogNumber, DepthInMeters, ShallowFlag) %>%
  arrange(DepthInMeters)
yo

##### _____ flower garden banks data check #####
x <- filt %>% filter(grepl('Flower Garden', DataProvider)) %>%
group_by(DatasetID, DataProvider, Repository, Citation,
         DataContact, Reporter, PI, Vessel, VehicleName, SurveyID) %>%
  summarize(
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    n_records = n(),
    MinYear = min(as.numeric(ObservationYear)),
    MaxYear = max(as.numeric(ObservationYear))
  )

View(x)

setwd("C:/rworking/digs/outdata")
write.csv(x, '20181009_0_FGB_Summary_Extract_from_DSCRTP_NatDB_20181005-0_RPMcGuinn.csv')

##### _____ Sandra Brooke data#####

sum_tbl <- filt %>%
  filter(grepl('Brooke, Sandra, sbrooke@fsu.edu', DataProvider)) %>%
  group_by(DatasetID, DataProvider, Citation) %>%
  summarize(#DataProvider = toString(unique(Repository)),
    Repository = toString(unique(Repository)),
    Vessel = toString(unique(Vessel)),
    PI = toString(unique(PI)),
    SampleID = toString(unique(SampleID)),
    ObservationDate = toString(unique(ObservationDate)),
    ScientificName = toString(unique(ScientificName)),
    Flag = toString(unique(Flag)),
    Records = n()
  ) %>%
  arrange(desc(Records))
View(sum_tbl)

##### _____ tidy plus kable equals good tables #####
library(broom)
tidy(summary(indata$DepthInMeters))

##### or make a kable table ####
x<-tidy(summary(indata$DepthInMeters))
as.data.frame(x)

##### _____ For ESRI Ocean Forums Map #####
##### extract from NDB #####
x <- filt %>%
  filter(FishCouncilRegion == 'Pacific') %>%
  select(CatalogNumber, DatasetID, ScientificName, VernacularNameCategory, DepthInMeters, DataProvider, Vessel, ObservationYear, Latitude, Longitude)

##### write file ####
setwd('C:\\rworking\\digs\\outdata\\')
write.csv(x, 'x.csv')

##### make shapefile #####

coordinates(x) <- c('Longitude', 'Latitude')
proj4string(x) <- '+proj=longlat +ellps=WGS84 +datum=WGS84'

# Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options
# argument allows us to specify the labels displayed by each of the points

setwd('C:/rworking/digs/outdata')
writeOGR(x, dsn='x',
         layer= 'x',
         driver = 'ESRI Shapefile',
         dataset_options=c('NameField=CatalogNumber'),
         overwrite_layer = T)


##### _____ Writing to feature class using arc to R arc.write #####
fgdb_path <- 'C:/data/aprx/GoldenCrab/GoldenCrab.gdb'

arc.write(file.path(fgdb_path, 'natdb/yo'), data=big, coords=c('Longitude', 'Latitude', 'DepthInMeters'),
          shape_info=list(type='Point',hasZ=TRUE, WKID=4326), overwrite = TRUE)

##### reading from feature class (after some GIS selection), creates 'arc.feature_impl'object #####
z <- arc.open(file.path(fgdb_path, 'natdb/what'))

##### transforming to (arc.data + data.frame) object #####
d <- arc.select(z)
e <- d %>% filter(Latitude > 28.87)

##### creating a spatial points dataframe   #####
sp.df <- arc.data2sp(e)

##### write back out to GDB #####
arc.write(file.path(fgdb_path, 'natdb/new'), data=sp.df)
##### _____ copy files from one directory to another #####
y <- d
y$ImageFilePath2<- gsub('\\\\', ';;', y$ImageFilePath)
y$ImageFilePath2 <- gsub(';;', '/', y$ImageFilePath2)
y$ImageFilePath2 <- gsub('F:/NF-17-08/05_ROV files/ROV photos/',
                         'P:/Deep Coral Ecology Lab/CRUISES/NF-17-08/05_ROVfiles/ROVphotos/',
                         y$ImageFilePath2)
y$ImageFilePath2 <- gsub('E:/NF-17-08/05_ROV files/ROV photos/',
                         'P:/Deep Coral Ecology Lab/CRUISES/NF-17-08/05_ROVfiles/ROVphotos/',
                         y$ImageFilePath2)

currentfiles <- y$ImageFilePath2
newlocation <- 'P:/Deep Coral Ecology Lab/CRUISES/NF-17-08/05_ROVfiles/ROVphotos/temp'

file.copy(from=currentfiles, to=newlocation, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)

#?file.copy
y$letter <- substr(y$ImageFilePath, start = 1, stop = 2)
y$letter <- revsubstr(y$ImageFilePath, start = 1, stop = 8)
table(y$letter, useNA = 'always')

##### creating the proper path using the 'substr' function ####
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
x <- as.character(y$ImageFilePath)
z <- substrRight(x, 12)
head(z)
setwd('P:/Deep Coral Ecology Lab/CRUISES/NF-17-08/05_ROVfiles/ROVphotos/temp')
files <- list.files(pattern = '\\.jpg$')
setdiff(files, z)
setdiff(z, files)

##### _____ explore workflow R to ArcGIS pro to ArcGIS online #####
##### filter data #####
x <- filt %>% filter(
                     #grepl('Umbellula', ScientificName),
                     #is.na(ImageURL) == F,
                     MinimumSize != '-999',
                     FishCouncilRegion == 'Gulf of Mexico'
) %>%
  dplyr::select(ScientificName, VernacularNameCategory, DepthInMeters, ImageURL, DataProvider, PI, SamplingEquipment, CatalogNumber,
                DatasetID, SampleID,gisMEOW, FishCouncilRegion, Locality, Longitude, Latitude)

##### checking #####
#library(lattice)

# hist(x$MinimumSize, breaks = 100)
# y <- sample_n(x, 20)
# length(x$Locality)
# table(factor(x$Locality))
# table(factor(x$gisMEOW))
# table(factor(x$ScientificName))
# summary(x$DepthInMeters)

##### writing to ArcGIS geodatabase #####
fgdb_path <- 'C:/data/aprx/explore/explore.gdb'
arc.write(file.path(fgdb_path, 'test5'), data=sub, coords=c('Longitude', 'Latitude'),
          shape_info=list(type='Point',hasZ=FALSE, WKID=4326), overwrite = TRUE)


##### filter data #####
x <- filt %>% filter(
  #grepl('Umbellula', ScientificName),
  #is.na(ImageURL) == F,
  MinimumSize != '-999',
  FishCouncilRegion == 'Gulf of Mexico'
) %>%
  dplyr::select(ScientificName, VernacularNameCategory, DepthInMeters, ImageURL, DataProvider, PI, SamplingEquipment, CatalogNumber,
                DatasetID, SampleID,gisMEOW, FishCouncilRegion, Locality, Longitude, Latitude)

##### create spatial points data.frame #####

coords <- subset(x, select = c('Longitude', 'Latitude'))

# making coords vectors numeric
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)

# creating SpatialPointsDataFrame from the subset.
spdf<-SpatialPointsDataFrame(coords, x,
                             proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'),
                             match.ID = TRUE)

fgdb_path <- 'C:/data/aprx/explore2/explore2.gdb'
arc.write(file.path(fgdb_path, 'yo2'), data=spdf)


# http://spatialreference.org/ref/epsg/4326/

##### _____ get all images into a folder #####
##### filter data #####
x <- filt %>% filter(grepl('South', FishCouncilRegion),
                     is.na(ImageURL) == F
                     #FishCouncilRegion == 'Gulf of Mexico'
) %>%
  dplyr::select(ImageURL, CatalogNumber, DatasetID, SampleID, ScientificName,VernacularNameCategory, gisMEOW, FishCouncilRegion, Locality,
                DepthInMeters, VernacularNameCategory, Longitude, Latitude)

x <- sample_n(x, 100)
##### exporting all images to a folder #####
# getting urls from file

urls <- x$ImageURL

setwd('C:/rworking/digs/outdata/imageset')

for (url in urls) {
  download.file(url, destfile = basename(url), mode = 'wb')
}
##### _____ histograms in lattice ##########

histogram(~as.numeric(MinimumSize)| VernacularNameCategory, data=x,
          type='percent',
          xlab='MinimumSize',
          ylim=c(0,100),
          xlim=c(0,100),
          main='histo',
          breaks=seq(from=0,to=1000,by=2),layout=c(1,4))

histogram(~as.numeric(MaximumSize)| VernacularNameCategory, data=x,
          type='percent',
          xlab='MaximumSize',
          ylim=c(0,100),
          xlim=c(0,100),
          main='histo',
          breaks=seq(from=0,to=1000,by=2),layout=c(1,4))

##### _____ making ggplot density diagrams comparing depth zones #####
x <- filt %>% filter(
  Order == 'Gorgonacea',
  # ScientificName == 'Antipathes furcata',
  #   ScientificName == 'Madracis pharensis',
  gisMEOW == 'Greater Antilles',
  # ScientificName == 'Madracis sp.'
  is.na(ImageURL) == F
  # MinimumSize != '-999'
  # MinimumSize != '-999'
  # DepthInMeters != '-999',
  # as.numeric(DepthInMeters) < 250
  # FishCouncilRegion == 'Gulf of Mexico'
)

ggplot(x, aes(as.numeric(DepthInMeters), fill = factor(Order))) +
  geom_density(alpha = 0.2) +
  xlim(0,500)

##### exporting all images to a folder #####
# getting urls from file

urls <- sample_n(x$ImageURL, 15)

setwd('C:/rworking/digs/outdata/imageset')

for (url in urls) {
  download.file(url, destfile = basename(url), mode = 'wb')
}

##### _____ manipulating colors #####
##### building big pallette #####
# display.brewer.all()
# to get the colors from a palette:
palette1 <- brewer.pal(9,"Set1")
palette2 <- brewer.pal(8,"Set2")
palette3 <- brewer.pal(9,"Set3")
palette4 <- brewer.pal(8,"Dark2")
palette5 <- brewer.pal(8,"Accent")

# We can just stick a few color palettes together
big_palette <- c(palette1,palette2,palette3, palette4, palette5)

image(1:40,1,as.matrix(1:40),col=big_palette,xlab="big",
      ylab="",xaxt="n",yaxt="n",bty="n")

set.seed(10)
image(1:40,1,as.matrix(1:40),col=sample(big_palette),xlab="big",
      ylab="",xaxt="n",yaxt="n",bty="n")

# Classic palette BuPu, with 4 colors
coul4 = brewer.pal(4, "BuPu")
image(1:4,1,as.matrix(1:4),col=coul4,xlab="coul",
      ylab="",xaxt="n",yaxt="n",bty="n")

# I can add more tones to this palette :
coul25 = colorRampPalette(coul4)(25)
image(1:25,1,as.matrix(1:25),col=coul25,xlab="coul25",
      ylab="",xaxt="n",yaxt="n",bty="n")

# display.brewer.all()

# Classic palette BuPu, with 4 colors
set4 = brewer.pal(4, "Set1")
image(1:4,1,as.matrix(1:4),col=set4,xlab="set4",
      ylab="",xaxt="n",yaxt="n",bty="n")

# I can add more tones to this palette :
set50 = colorRampPalette(set4)(50)
image(1:50,1,as.matrix(1:50),col=set50,xlab="set50",
      ylab="",xaxt="n",yaxt="n",bty="n")
##### _____ working on data for species lists
##### Bringing in input datasets #####
setwd('C:/rworking/digs/indata')
indata<-read.csv('DSCRTP_NatDB_20181029-0.csv', header = T)
filt <- indata %>%
  filter(Flag == '0')

##### _____ making a table for species list comparisons for Tom Hourigan #####
x <- filt %>%
  filter(
    DepthInMeters != '-999',
    Phylum == 'Cnidaria'
         ) %>%
  group_by(FishCouncilRegion, Phylum, Class, Order, Family, Genus, ScientificName, AphiaID) %>%
  summarize(n=n(),
            VerbatimScientificName = toString(unique(VerbatimScientificName)),
            MinDepth = min(DepthInMeters),
            MedianDepth = median(DepthInMeters),
            MaxDepth = max(DepthInMeters))

setwd('C:\\rworking\\digs\\outdata\\')
write.csv(x, 'x.csv')

##### just the western pacific stuff #####
# Hawaiian Archipelago = Hawaii
# Johnston = Hawaii
# Wake = Marshall Islands
# Marianas = Mariana Islands
# American Samoa = Samoa Islands
x <- filt %>%
  filter(
    DepthInMeters != '-999',
    Phylum == 'Cnidaria',
    gisMEOW == 'Hawaii'|
      gisMEOW == 'Marshall Islands'|
      gisMEOW == 'Samoa Islands' |
      gisMEOW == 'Mariana Islands'|
      FishCouncilRegion == 'Western Pacific'
  ) #%>%
  # group_by(gisMEOW, Phylum, Class, Order, Family, Genus, ScientificName, AphiaID) %>%
  # summarize(n=n(),
  #           VerbatimScientificName = toString(unique(VerbatimScientificName)),
  #           MinDepth = min(DepthInMeters),
  #           MedianDepth = median(DepthInMeters),
  #           MaxDepth = max(DepthInMeters))

# setwd('C:\\rworking\\digs\\outdata\\')
# write.csv(x, 'x.csv')

fgdb_path <- 'C:/data/aprx/explore/explore.gdb'
arc.write(file.path(fgdb_path, 'natdb/hawaii'), data=x, coords=c('Longitude', 'Latitude', 'DepthInMeters'),
          shape_info=list(type='Point',hasZ=TRUE, WKID=4326), overwrite = TRUE)


##### reading from feature class (after some GIS selection process), creates 'arc.feature_impl'object #####
fgdb_path <- 'C:/data/aprx/explore/explore.gdb'
mariana_sub <- arc.open(file.path(fgdb_path, 'mariana_sub'))
hawaii_sub <- arc.open(file.path(fgdb_path, 'hawaii_sub'))
marshall_sub <- arc.open(file.path(fgdb_path, 'marshall_sub'))
johnston_sub <- arc.open(file.path(fgdb_path, 'johnston_sub'))
samoa_sub <- arc.open(file.path(fgdb_path, 'samoa_sub'))

##### checking #####
class(samoa_sub)

##### transforming to (arc.data + data.frame) object #####
mariana_sub <- arc.select(mariana_sub)
hawaii_sub <- arc.select(hawaii_sub)
marshall_sub <- arc.select(marshall_sub)
johnston_sub <- arc.select(johnston_sub)
samoa_sub <- arc.select(samoa_sub)

##### working on species summaries from each subset #####

# mariana

x <- mariana_sub %>%
  filter(
    DepthInMeters != '-999',
    Phylum == 'Cnidaria'
  ) %>%
  group_by(FishCouncilRegion, Phylum, Class, Order_, Family, Genus, ScientificName, AphiaID) %>%
  summarize(n=n(),
            VernacularNameCategory = toString(unique(VernacularNameCategory)),
            VerbatimScientificName = toString(unique(VerbatimScientificName)),
            MinDepth = min(DepthInMeters),
            MedianDepth = median(DepthInMeters),
            MaxDepth = max(DepthInMeters))


setwd('C:\\rworking\\digs\\outdata\\')
write.csv(x, '20181112_0_mariana_species_list_RPMcGuinn.csv')

# hawaii

x <- hawaii_sub %>%
  filter(
    DepthInMeters != '-999',
    Phylum == 'Cnidaria'
  ) %>%
  group_by(FishCouncilRegion, Phylum, Class, Order_, Family, Genus, ScientificName, AphiaID) %>%
  summarize(n=n(),
            VernacularNameCategory = toString(unique(VernacularNameCategory)),
            VerbatimScientificName = toString(unique(VerbatimScientificName)),
            MinDepth = min(DepthInMeters),
            MedianDepth = median(DepthInMeters),
            MaxDepth = max(DepthInMeters))

setwd('C:\\rworking\\digs\\outdata\\')
write.csv(x, '20181112_0_hawaii_species_list_RPMcGuinn.csv')

# samoa

x <- samoa_sub %>%
  filter(
    DepthInMeters != '-999',
    Phylum == 'Cnidaria'
  ) %>%
  group_by(FishCouncilRegion, Phylum, Class, Order_, Family, Genus, ScientificName, AphiaID) %>%
  summarize(n=n(),
            VernacularNameCategory = toString(unique(VernacularNameCategory)),
            VerbatimScientificName = toString(unique(VerbatimScientificName)),
            MinDepth = min(DepthInMeters),
            MedianDepth = median(DepthInMeters),
            MaxDepth = max(DepthInMeters))

setwd('C:\\rworking\\digs\\outdata\\')
write.csv(x, '20181112_0_samoa_species_list_RPMcGuinn.csv')

# johnston

x <- johnston_sub %>%
  filter(
    DepthInMeters != '-999',
    Phylum == 'Cnidaria'
  ) %>%
  group_by(FishCouncilRegion, Phylum, Class, Order_, Family, Genus, ScientificName, AphiaID) %>%
  summarize(n=n(),
            VernacularNameCategory = toString(unique(VernacularNameCategory)),
            VerbatimScientificName = toString(unique(VerbatimScientificName)),
            MinDepth = min(DepthInMeters),
            MedianDepth = median(DepthInMeters),
            MaxDepth = max(DepthInMeters))

setwd('C:\\rworking\\digs\\outdata\\')
write.csv(x, '20181112_0_johnston_species_list_RPMcGuinn.csv')

# marshall

x <- marshall_sub %>%
  filter(
    DepthInMeters != '-999',
    Phylum == 'Cnidaria'
  ) %>%
  group_by(FishCouncilRegion, Phylum, Class, Order_, Family, Genus, ScientificName, AphiaID) %>%
  summarize(n=n(),
            VernacularNameCategory = toString(unique(VernacularNameCategory)),
            VerbatimScientificName = toString(unique(VerbatimScientificName)),
            MinDepth = min(DepthInMeters),
            MedianDepth = median(DepthInMeters),
            MaxDepth = max(DepthInMeters))

setwd('C:\\rworking\\digs\\outdata\\')
write.csv(x, '20181112_0_marshall_species_list_RPMcGuinn.csv')


##### creating a spatial points dataframe   #####
sp.df <- arc.data2sp(mariana_sub_filt)

##### write back out to GDB #####
arc.write(file.path(fgdb_path, 'natdb/new_sub2'), data=sp.df)

##### _____ VernacularNameCategory analysis #####
d <- tabledap('deep_sea_corals',
                   fields=c('VernacularNameCategory', 'CatalogNumber'),
                   url = 'https://ecowatch.ncddc.noaa.gov/erddap/')

kable(table(d$VernacularNameCategory, useNA = 'always'))

d$CatalogNumber[is.na(d$VernacularNameCategory) == TRUE]

##### _____ Exploring Latitude distribution #####
x <- filt %>%
  filter(
    # DatasetID!='MBARI',
    as.numeric(Latitude) > 0)

qplot(as.numeric(x$Latitude), binwidth = 1)

##### _____ Exploring DataProvider and DatasetID #####
x <- filt %>%
  filter(
    grepl('Wagner', DataProvider)
  ) %>%
  dplyr::select(DatasetID, DataProvider, SurveyID, DataContact, Reporter, Citation) %>%
  group_by(DatasetID, DataProvider, SurveyID, DataContact, Reporter, Citation) %>%
  summarize(n=n())

View(x)

# setwd('C:/rworking/digs/outdata')
# write.csv(x, '2018112_0_datasetid_Hexacoral_RPMcGuinn.csv', row.names = F, quote = T)

##### _____ Exploring temperature ranges for species #####
##### _____ Creating a summary table
x <- filt %>%
  filter(
    #grepl('Farallones', DataProvider)
    #ScientificName == 'Heteropolypus ritteri',
    Temperature != '-999'
  ) %>%
  group_by(ScientificName) %>%
  summarize(n=n()) %>%
  arrange(-n)
View(x)

##### exploring specific species #####
spec <- 'Chromoplexaura marki'#'Paragorgia arborea' #'Aphrocallistes vastus'
x <- filt %>%
  filter(
    #grepl('Farallones', DataProvider)
    ScientificName == spec,
    Temperature != '-999'
    #is.na(ImageURL) == FALSE
  )

qplot(as.numeric(x$DepthInMeters), binwidth = 20, color = factor(x$FishCouncilRegion))
qplot(as.numeric(x$Latitude), binwidth = .05, color = factor(x$FishCouncilRegion))
qplot(as.numeric(x$Temperature), binwidth = .07, color = factor(x$FishCouncilRegion))
qplot(x$DepthInMeters, x$Temperature, color = factor(x$FishCouncilRegion))
# browseURL(as.character(x$ImageURL[1]))


##### using erddap to download data #####
yo <- tabledap('deep_sea_corals',
                   fields=c('latitude','longitude','ScientificName','ImageURL', 'ETOPODepth'),
                   url = 'https://ecowatch.ncddc.noaa.gov/erddap/')

##### mapping with marmap and ggplot #####

spdf<-sub
coordinates(spdf) <- c("Longitude", "Latitude", "DepthInMeters")
proj4string(spdf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
x<-bbox(spdf)
# install.packages('marmap')
# library(marmap)
zoom <- 2 # as number gets bigger, wider extent
cont <- getNOAA.bathy(lon1 = x[1,1]-zoom, lon2 = x[1,2]+zoom,
                      lat1 = x[2,1]-zoom, lat2 = x[2,2]+zoom, resolution = 1,
                      keep = FALSE, antimeridian = FALSE)

# topographical color scale, see ?scale_fill_etopo
g <- autoplot(cont, geom=c("raster", "contour")) +
  scale_fill_etopo(name="Depth\n(meters)") +
  labs(x = 'Longitude') +
  labs(y = 'Latitude')

# add sampling locations

g + geom_point(aes(x=Longitude, y=Latitude), data=sub, alpha=0.5)

##### looking at the critical spatial envelope of any data subset #####
# filter data in some way
sub <- filt %>%
  filter(
    #Genus == 'Anthomastus',
    ScientificName == 'Lophelia pertusa',
    is.na(ImageURL) == FALSE
  )
  #   ) %>%
  # dplyr::select(Longitude, Latitude, Class, Order, Family, Genus, Species, ScientificName, ImageURL, DepthInMeters) %>%
  # group_by(ScientificName) %>%
  # summarize(n = n())

# look the 3D bounding box. convex hull
spdf<-sub
coordinates(spdf) <- c("Longitude", "Latitude", "DepthInMeters")
proj4string(spdf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
x<-bbox(spdf)
x


##### _____ Creating a spatial points dataframe from list of coordinates #####
library(sp)

dat_orig <- read.table(text="a lat lng
                       data   22.352728  114.251733", header=TRUE, stringsAsFactors=FALSE)
spdf <- dat_orig
coordinates(spdf) <- ~lng+lat
proj4string(spdf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
x<-bbox(spdf)

# install.packages('marmap')
# library(marmap)
zoom <- 0 # as number gets bigger, wider extent
cont <- getNOAA.bathy(lon1 = x[1,1]-zoom, lon2 = x[1,2]+zoom,
                      lat1 = x[2,1]-zoom, lat2 = x[2,2]+zoom, resolution = 1,
                      keep = FALSE, antimeridian = FALSE)

# topographical color scale, see ?scale_fill_etopo
g <- autoplot(cont, geom=c("raster", "contour")) +
  scale_fill_etopo(name="Depth\n(meters)") +
  labs(x = 'Longitude') +
  labs(y = 'Latitude')


g + geom_point(aes(x=lng, y=lat), colour = 'red', size = 2, data=dat_orig, alpha=1)


##### send poitns to ArcGIS #####
fgdb_path <- 'C:/data/aprx/explore/explore2.gdb'
arc.write(file.path(fgdb_path, 'china'), data=dat_orig, coords=c('lng', 'lat'),
          shape_info=list(type='Point',hasZ=FALSE, WKID=4326), overwrite = TRUE)






