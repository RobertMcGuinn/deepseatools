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
help("geomedb")

View(listProjects())

projectId <- 348
View(listExpeditions(projectId))

listEntities()

##### workflow example #####
acaoli <- queryMetadata(entity = "Sample",
                        query = "genus = Acanthurus AND specificEpithet = olivaceus")

acaoli_seqs <- querySanger(projects = 1,
                           locus= "CYB",
                           query = "genus = Acanthurus AND specificEpithet = olivaceus")

acaoli_sra <- queryMetadata(entity = "fastqMetadata",
                            query = "genus = Acanthurus AND specificEpithet = olivaceus AND _exists_:bioSample",
                            select=c("Event","Sample"))

##### look at tables #####
sample <- acaoli_sra$Sample
event <- acaoli_sra$Event
fastqMetadata <- acaoli_sra$fastqMetadata

View(sample)
View(event)
View(fastqMetadata)










