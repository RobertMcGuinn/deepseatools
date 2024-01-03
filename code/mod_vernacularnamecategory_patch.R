##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20240103
## purpose: crosswalk between AphiaIDs and VernacularNameCategory

##### linkage #####
## manual input here
filename <- 'mod_vernacularnamecategory_patch.R' ## entry: name of the current file.  make sure to include '.R'
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)

##### grab all AphiaIDs #####
aphiaIDs <- unique(filt$AphiaID)

##### additional AphiaIDs ######
## these are where Aphia was -999 at version
filt %>% filter(AphiaID == -999) %>%
  pull(ScientificName) %>%
  unique()

1578714 # [1] "Bathypathes pseudoalternata"
-999 # [2] "Leptogorgia porrecta"
1521953 # [3] "Parantipathes pluma"
-999 # [4] "Pseudothesea pailoloensis"
-999 # [5] "Tribrachium symmetra"
-999 # [6] "Atlantisella alenuihaha"
-999 # [7] "Lepidisis cornucopia"
1391784 # [8] "Kophobelemnon biflorum"
1392576 # [9] "Lithophytum roseum"
-999 # [10] "Chrysogorgia arbuscula"
-999 # [11] "Halisarca membrana"
-999 # [12] "Neopelta aberrans"
-999 # [13] "Acromuricea hirtella"
-999 # [14] "Acromuricea alatispina"
1635628 # [15] "Iridogorgia densispicula"
1635629 # [16] "Iridogorgia squarrosa"
1647390 # [17] "Sibogagorgia californica"
1514479 # [18] "Aphanostichopathes"
1317063 # [19] "Callogorgia cracentis"




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







##### find all entries where crosswalk has n > 1 #####

