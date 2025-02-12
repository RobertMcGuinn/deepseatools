##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20241010
## purpose:

##### linkage #####
filename <- 'mod_geome' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
# install.packages("devtools")
# library(devtools)
# install_github("biocodellc/fimsR-access")
library(geomedb)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### help #####
# help("geomedb")
#
# View(listProjects())
#
# projectId <- 348
# View(listExpeditions(projectId))
#
# listEntities()

##### query the API #####
acaoli <- queryMetadata(entity = "Sample",
                        query = "genus = Acanthurus AND specificEpithet = olivaceus")

acaoli_seqs <- querySanger(projects = 1,
                           locus= "CYB",
                           query = "genus = Acanthurus AND specificEpithet = olivaceus")

acaoli_sra <- queryMetadata(entity = "fastqMetadata",
                            query = "genus = Acanthurus AND specificEpithet = olivaceus AND _exists_:bioSample",
                            select=c("Event","Sample"))

##### extract specific df's #####
sample <- acaoli_sra$Sample
event <- acaoli_sra$Event
fastqMetadata <- acaoli_sra$fastqMetadata

# sample <- acaoli$Sample
# event <- acaoli$Event
# fastqMetadata <- acaoli$fastqMetadata

# sample <- acaoli_seqs$Sample
# event <- acaoli_seqs$Event
# fastqMetadata <- acaoli_seqs$fastqMetadata

##### check #####
# names(acaoli_seqs)
# str(acaoli_seqs)
View(sample)
View(event)
View(fastqMetadata)
#
# intersect(names(sample), names(event))
# intersect(names(fastqMetadata), names(sample))
# intersect(names(fastqMetadata), names(event))

###### create list of all variables.
all_names <- union(names(sample), names(event))
all_names <- union(all_names, names(fastqMetadata))

##### check #####
grep('material', all_names, value = T)
grep('material', names(sample), value = T)
grep('material', names(fastqMetadata), value = T)
grep('material', names(event), value = T)

sort(names(sample))

grep('id', all_names, value = T)

##### look at a specific sequence #####
acaoli_seqs$`https://n2t.net/ark:/21547/BEC2Acaoli_904_CYB [marker = CYB] [tissueID = Acaoli_904] [genus = Acanthurus] [specificEpithet = olivaceus]`










##### look at USNM SampleID #####
filt %>% filter(grepl("NMNH", DatasetID)) %>%
  pull(SampleID) %>% unique()

filt %>% filter(grepl("NMNH", DatasetID)) %>%
  select(DatasetID, SampleID, TrackingID) %>% View()



