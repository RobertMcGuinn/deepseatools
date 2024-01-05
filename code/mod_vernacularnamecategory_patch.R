##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240103
## purpose: crosswalk between AphiaIDs and VernacularNameCategory

##### linkage #####
## manual input here
filename <- 'mod_vernacularnamecategory_patch.R' ## entry: name of the current file.  make sure to include '.R'
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)

##### check #####
## find where AphiaID are missing
filt %>% filter(AphiaID == -999) %>%
  pull(ScientificName) %>%
  unique()

##### add a few missing AphiaIDs to the NDB ######
filt_fixed <- filt %>%
  mutate(AphiaID = ifelse(ScientificName == "Bathypathes pseudoalternata",
                          1578714,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Parantipathes pluma",
                          1521953,
                         AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Kophobelemnon biflorum",
                          1391784,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Lithophytum roseum",
                          1392576,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Iridogorgia densispicula",
                          1635628,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Iridogorgia squarrosa",
                          1635629,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Sibogagorgia californica",
                          1647390,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Aphanostichopathes",
                          1514479,
                          AphiaID)) %>%
  mutate(AphiaID = ifelse(ScientificName == "Callogorgia cracentis",
                          1317063,
                          AphiaID))

## The following taxa remain without AphiaID matches in worms as of 20240104
# -999 # [2] "Leptogorgia porrecta"
# -999 # [4] "Pseudothesea pailoloensis"
# -999 # [5] "Tribrachium symmetra"
# -999 # [6] "Atlantisella alenuihaha"
# -999 # [7] "Lepidisis cornucopia"
# -999 # [10] "Chrysogorgia arbuscula"
# -999 # [11] "Halisarca membrana"
# -999 # [12] "Neopelta aberrans"
# -999 # [13] "Acromuricea hirtella"
# -999 # [14] "Acromuricea alatispina"

##### check #####
filt_fixed %>% filter(AphiaID == -999) %>%
  pull(ScientificName) %>%
  unique()

filt_fixed %>%
  filter(ScientificName == 'Callogorgia cracentis') %>%
  pull(AphiaID)

##### grab all AphiaIDs #####
aphiaIDs <- unique(filt_fixed$AphiaID)

##### check #####
x <- filt_fixed %>%
  filter(AphiaID < 1) %>%
  pull(ScientificName) %>%
  unique()
x

##### create a crosswalk #####
crosswalk <- filt %>%
  group_by(AphiaID, VernacularNameCategory, ScientificName, VerbatimScientificName) %>%
  summarize(n=n())

##### check #####
View(crosswalk)
length(aphiaIDs)
dim(crosswalk)
length(aphiaIDs) - dim(crosswalk)
names(crosswalk)
class(crosswalk$n)



