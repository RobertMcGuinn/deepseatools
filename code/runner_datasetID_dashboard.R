##### Header #####
# Author: Robert P. McGuinn
# Date started: 2018-08-14
# Purpose: Execute RMarkdown documents on dashboards for each DatasetID.
#   4 groups: Cruise, Literature, Program, Repository, and then Repository-MBARI
#(because it is so huge it can't have an interactive map)

##### set an option #####
options(lifecycle_disable_warnings = TRUE)

##### _____ Bringing in database #####
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20190920-0.csv", header = T) #DSCRTP_NatDB_20181005-0.csv # DSCRTP_NatDB_20181211-2.csv
filt <- indata %>%
  filter(Flag == "0")

##### define database version (this variable called in RMD files) #####
version <- unique(indata$DatabaseVersion)
version <- as.character(version)

##### bringing in datasetID key from xls stored on drive #####
# create a list of files (or single file) that meets title query
x <- drive_find(q = "name contains '20190920-1_DatasetID_Key_DSCRTP'")

# # browse to it
# x %>% drive_browse()

# getting the id as a character string
y <- x$id

# this downloads the file to the specified path
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/20190920-1_DatasetID_Key_DSCRTP.xlsx", overwrite = TRUE)

# read the file into R as a data frame
key <- read.xlsx(dl$local_path)

# clean up
rm(y)
rm(x)

##### checking #####
setdiff(filt$DatasetID, key$DatasetID)
setdiff(key$DatasetID, filt$DatasetID)

##### subsetting of indata to d (optional step for testing purposes) #####
d <- indata %>%
  filter(
    DatasetID == 'MCZ_IZ' | # Repository
    DatasetID == 'Hall-Spencer_etal_2007' | # Literature
    DatasetID == 'SOI_FK006C' | #Cruise
    DatasetID == 'NOAA_AFSC_Longline_Survey' #Program
  )

##### ***OR*** full run set indata to d #####
d <- indata

##### checking #####
# table(unique(factor(d$DataProvider)))
# table(unique(factor(d$RecordType)))
# table(unique(factor(d$DataContact)))
# table(factor(d$DatasetID), useNA = 'always')
#
# yo <- key %>%
#   # filter(class == 'Cruise') %>%
#   group_by(class) %>%
#   summarise(DatasetID = paste(unique(DatasetID), collapse = " | "),
#             n=n())
# View(yo)


##### checking #####
# x <- d %>%
#   arrange(ObservationYear) %>%
#   filter(DatasetID %in% setdiff(indata$DatasetID, filt$DatasetID)) %>%
#   group_by(DatasetID) %>%
#   summarize(
#       RecordType_list = toString(unique(RecordType)),
#       N_Records=n(),
#       # DatasetID_list = toString(unique(DatasetID)),
#       DataProvider_list = toString(unique(DataProvider)),
#       Repository_list = toString(unique(Repository)),
#       ObservationYear_list = toString(unique(ObservationYear)),
#       Vessel_list = toString(unique(Vessel)),
#       VehicleName_list = toString(unique(VehicleName)),
#       WebSite_list = toString(unique(WebSite)),
#       #ImageURL = toString(unique(ImageURL)),
#       PI_list = toString(unique(PI)),
#       PIAffiliation_list = toString(unique(PIAffiliation)),
#       Citation_list = toString(unique(Citation)),
#       DataContact_list = toString(unique(DataContact)),
#       Reporter_list = toString(unique(Reporter)),
#       SurveyID_list = toString(unique(SurveyID))
#     )
#
# View(x)

##### Fixing DatasetID Problems (optional) #####
# d <- d %>% mutate(DatasetID =
#                     ifelse(DatasetID == "MCZ_IZ_1968_1880",
#                            'MCZ_IZ',
#                            as.character(DatasetID)))
# d <- d %>% mutate(DatasetID =
#                     ifelse(DatasetID == "NMNH-IZ",
#                            'NMNH_IZ',
#                            as.character(DatasetID)))
#
# d <- d %>% mutate(DatasetID =
#                     ifelse(DatasetID == "CT-13-07",
#                            'NOAA_CT-13-07',
#                            as.character(DatasetID)))

##### checking #####
setdiff(d$DatasetID, key$DatasetID)
setdiff(key$DatasetID, d$DatasetID)

##### merge d (primary subset) with key  #####
d <- merge(d, key, all.x = TRUE)

##### check #####
# table(d$class, useNA = 'always')
#
# x <- d %>%
#   group_by(class) %>%
#   summarize(DatasetsbyClass = length(unique(DatasetID)),
#     n = n())
# View(x)

# setdiff(d$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, d$DatasetID)
# #
# x <- d #%>%
#   # filter(
#   #   is.na(class) == T,
#   #   #DataProvider == "Temple University",
#   #   #class == "Literature"
#   #   )
#
# table(factor(x$class), useNA = 'always')

##### *** run the reports *** #####
## create the folders for each type of report

dir.create('C:/rworking/deepseatools/reports/datasetid/cruise')
dir.create('C:/rworking/deepseatools/reports/datasetid/literature')
dir.create('C:/rworking/deepseatools/reports/datasetid/program')
dir.create('C:/rworking/deepseatools/reports/datasetid/repository')

## cruise: run RMD on each unique DatasetID group
#cruise subset
yo <- d %>%
  filter(
    class == 'Cruise'
  )

# run RMD
library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_cruise.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/cruise')
}

## literature: run RMD on each unique DatasetID group
yo <- d %>%
  filter(
    class == 'Literature'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_literature.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/literature')
}


## program: run RMD on each unique DatasetID group
yo <- d %>%
  filter(
    class == 'Program'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_program.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/program')
}


## repository: run RMD on each unique DatasetID group
yo <- d %>%
  filter(
    class == 'Repository'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_repository.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/repository')
}

## repository:MBARI
yo <- d %>%
  filter(
    DatasetID == 'MBARI'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_repository_MBARI.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/repository')
}

##### checking #####
cruise <- list.files('C:/rworking/deepseatools/reports/datasetid/cruise/')
literature <- list.files('C:/rworking/deepseatools/reports/datasetid/literature/')
program <- list.files('C:/rworking/deepseatools/reports/datasetid/program/')
repository <- list.files('C:/rworking/deepseatools/reports/datasetid/repository/')

length(cruise)
length(literature)
length(program)
length(repository)

biglist <- c(cruise, literature, program, repository)
biglist <- gsub(pattern = "\\.html$", "", biglist)

setdiff(biglist, unique(filt$DatasetID))
length(unique(filt$DatasetID))
length(biglist)

length(unique(indata$DatasetID))
length(unique(filt$DatasetID))
setdiff(unique(indata$DatasetID), unique(filt$DatasetID))

##### publish a list of datasetID's for review sheet #####
x <- d %>% group_by(DatasetID, class) %>%
  summarize(n = n()) %>%
  arrange(class, DatasetID)

View(x)

##### write CSV of x #####
setwd("C:/rworking/deepseatools/indata")
x %>%
  write.csv(paste("20190718-0_DatasetID_Dashboard_Review",".csv", sep = ''), row.names = FALSE)









##### updating DatasetID key with new 'n' #####

# build a frequency table by DatasetID
x <- filt %>% group_by(DatasetID) %>% summarize(n=n())
names(key)

# strip n from fieds
y <- key[,1:8]
names(y)

# merge new numbers
z <- merge(y,x)
names(z)

# write out result
write.xlsx(z, "C:/rworking/deepseatools/indata/20190920-0_DatasetID_Key_DSCRTP.xlsx",
overwrite = TRUE)

