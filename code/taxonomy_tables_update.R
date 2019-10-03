##### Header#####
# started: 20170628
# Robert McGuinn
# robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
# 843-460-9696, 843-830-8845

##### Installation/Loading of Packages #####
#install.packages('xlsx')
library(xlsx)
#install.packages("beanplot")
library(beanplot)
#install.packages("stringr")
library(stringr)
#install.packages("knitr")
library(knitr)
#install.packages("tidyr")
library(tidyr)
#install.packages("sp")
library(sp)
#install.packages("maptools")
library(maptools)
#install.packages("maps")
library(maps)
#install.packages("reshape")
library(reshape)
#install.packages("reshape2")
library(reshape2)
#install.packages("psych")
library(psych)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("data.table")
library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("car")
library(car)
#install.packages("gdata")
library(gdata)
#install.packages("digest")
library(digest)
#install.packages("rgdal")
library(rgdal)
#install.packages("ggmap")
library(ggmap)
#install.packages("rerddap")
library(rerddap)
#install.packages("raster")
library(raster)
#install.packages("rworldxtra")
library(rworldxtra)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("xtable")
library(xtable)
library(taxize)
library(rgdal)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)


##### *** RUN *** #####
##### *** loading files *** #####
##### load current taxonomy tables #####
# see Google Drive folder: https://drive.google.com/drive/folders/0B9c2c_XdhpFBT29NQmxIeUQ4Tlk
setwd("C:/rworking/digs/indata")
tax <- read.csv("20181011-1_taxonomy.csv", header = T)
taxch <- read.csv("20181011-1_taxonomy_to_change.csv", header = T)
taxfl <- read.csv("20181011-1_taxonomy_to_flag.csv", header = T)

##### clean up taxfl #####
# this step removes 'NA'
taxfl <- taxfl %>% filter(Flag == "1")
#View(taxfl)
##### clean up original taxonomy table where Phylum is null (n=2) #####
tax <- tax %>%
  filter(is.na(tax$Phylum) == F)

##### load data set of interest (d) #####
setwd("C:/rworking/digs/indata")
d <- read.csv("20181109-0_NOAA_OER_EX1702_EX1703_EX1705_EX1706_Kelley_West_Pacific_2017_2017.csv", header = T)

##### filter data #####
cors <- d %>%
  filter(
    Phylum == 'Cnidaria' |
      Phylum == 'Porifera'
  )

##### setdiff to get corals and sponges out of nomatch list #####
not_cors <- setdiff(d$ScientificName, cors$ScientificName)
length(cors$ScientificName)
length(d$ScientificName)
length(not_cors)
length(nomatch$ScientificName)
cors_nomatch <- setdiff(nomatch$ScientificName, not_cors)
listmatch


##### load taxa of interest via CSV #####
#csv of ScientificNames of interest
setwd("C:/rworking/digs/indata")
nomatch <- read.csv('nomatch.csv', header = T)

# #check
# table(nomatch$ScientificName)

#create a list from the filtered data
list <- factor(nomatch$ScientificName)

##### -OR- load a single species to a list #####
#list <- "Cladocora arbuscula"
##### *** END *** #####
##### *** RUN *** #####
##### *** add matched records from worms to taxonomic table *** #####
##### bringing in matched list from WoRMS (output: listmatch) #####
# list matching function at WoRMS: http://www.marinespecies.org/aphia.php?p=match
setwd("C:/rworking/digs/indata")
listmatch <- read.csv("nomatch_matched.csv",header=T, na.strings=c("","NA"))

##### modifying listmatch to include only coral and sponge records ####
listmatch <- listmatch %>% filter(listmatch$ScientificName %in% cors_nomatch)

# # # check
# names(listmatch)
# View(listmatch)
# table(factor(listmatch$Match.type), useNA = 'always')
# listmatch[listmatch$Taxon.status == "unaccepted",] %>% dplyr::select(ScientificName, ScientificName.1, ScientificName_accepted, Genus, Species)
# setdiff(listmatch$ScientificName, listmatch$ScientificName_accepted)

##### working on 'taxa to change' list #####
# creating an empty taxch table
newtaxch <- taxch[0,]

#adding enough empty rows
newtaxch[1:length(listmatch$ScientificName),] <- NA
newtaxch$ScientificName <- listmatch$ScientificName_accepted
newtaxch$VerbatimScientificName <- listmatch$ScientificName
newtaxch$Genus <- listmatch$Genus
newtaxch$Species <- listmatch$Species
genus_fix <- newtaxch %>%
  filter(is.na(Species) == T,
         is.na(Genus) == F
  )

genus_nofix <- newtaxch %>%
  filter(is.na(Species) == F,
         is.na(Genus) == F
  )

# check
# genus_fix %>% dplyr::select(Genus, Species)
# genus_nofix %>% dplyr::select(Genus, Species)


# apply changes to genus_fix and genus_nofix
genus_fix$ScientificName <- paste(genus_fix$ScientificName, "sp.")

# combine
newtaxch <- rbind(genus_fix, genus_nofix)
newtaxch <- newtaxch[,1:2]
#newtaxch

# combine new taxa with original file (output: taxch2)
taxch2 <- rbind(taxch, newtaxch)

# this makes the levels match between the two varibles you are comparing
levels <- sort(unique(unlist(taxch2[, c('VerbatimScientificName', 'ScientificName')])))
taxch2$ScientificName <- factor(taxch2$ScientificName, levels = levels)
taxch2$VerbatimScientificName <- factor(taxch2$VerbatimScientificName, levels = levels)

# this actually makes the comparison and removes the rows that match
taxch2 <- taxch2[which(taxch2[,1] != taxch2[,2]), ]

# writing the new joint taxonomy table to disc (output: new csv of taxonomy table)
setwd("C:/rworking/digs/outdata")
write.csv(taxch2,"20180927-0_taxonomy_to_change.csv", row.names = F, quote = T)

##### split: splitting matched file between taxa with accepted names unaccepted #####
listmatch_acc <- listmatch %>%
  filter(Taxon.status == "accepted")

listmatch_unacc <- listmatch %>%
  filter(Taxon.status == "unaccepted")

# checking
# listmatch_acc %>% dplyr::select(Genus, Species)
# listmatch_unacc %>% dplyr::select(Genus, Species)

##### creating an empty taxonomy table (using information from accepted names) to populate (output: newtax) #####
newtax <- tax[0,]

#adding enough empty rows
newtax[1:length(listmatch_acc$ScientificName),]<-NA

#checking
#View(newtax)

##### adding information from (listmatch_acc from Worms) to taxonomy table
newtax$ScientificName <- listmatch_acc$ScientificName_accepted
newtax$AphiaID <- listmatch_acc$AphiaID_accepted
newtax$ScientificNameAuthorship <- listmatch_acc$Authority_accepted
newtax$Phylum <- listmatch_acc$Phylum
newtax$Class <- listmatch_acc$Class
newtax$Order <- listmatch_acc$Order
newtax$Family <- listmatch_acc$Family
newtax$Genus <- listmatch_acc$Genus
newtax$Subgenus <- listmatch_acc$Subgenus
newtax$Species <- listmatch_acc$Species
newtax$Subspecies <- listmatch_acc$Subspecies

# # view it
# View(newtax)

# get rid of non-matched where NA in ScientificName
newtax <- newtax[is.na(newtax$AphiaID) == FALSE, ]

# # view itView(newtax)
#

##### split/apply/combine: work on Genus 'sp.' issue and TaxonRank in the accepted 'newtaxa' (output: 'newtax') #####

genus_fix <- newtax %>%
  filter(is.na(Species) == T,
         is.na(Genus) == F
         )

genus_nofix <- newtax %>%
  filter(is.na(Species) == F,
         is.na(Genus) == F
  )

# check
# genus_fix %>% dplyr::select(Genus, Species)
# genus_nofix %>% dplyr::select(Genus, Species)


# apply changes to genus_fix and genus_nofix

genus_fix$ScientificName <- paste(genus_fix$ScientificName, "sp.")
genus_fix$TaxonRank <- 'genus'
genus_nofix$TaxonRank <- 'species'

# combine

newtax <- rbind(genus_fix, genus_nofix)

# #check
# View(genus_fix)
# View(genus_nofix)
# View(newtax)
# newtax %>% dplyr::select(Genus, Species)

##### filter out duplicates with existing taxonomy table #####
newtax_nodups <- newtax[newtax$ScientificName %in% tax$ScientificName == F,]
newtax_nodups$SynonymAphiaID <- '-999'
##### *** Export list of accepted ScientificNames from original unaccepted names for matching at Worms *** #####
list <- factor(listmatch_unacc$ScientificName_accepted)

# Extact only taxa that are not duplicated in the original taxonomic file
list <-  list[list %in% tax$ScientificName == F]

# export list for matching
setwd("C:/rworking/digs/outdata")
write.csv(list, "list.csv", row.names = F)

##### bringing in matched list from WoRMS (from unaccepted list) (output: listmatch2) #####
# list matching function at WoRMS: http://www.marinespecies.org/aphia.php?p=match
setwd("C:/rworking/digs/indata")
listmatch2 <- read.csv("list_matched.csv",header=T, na.strings=c("","NA"))

# # # check
# names(listmatch)
# View(listmatch)
# table(factor(listmatch$Match.type), useNA = 'always')
# listmatch[listmatch$Taxon.status == "unaccepted",] %>% dplyr::select(ScientificName, ScientificName.1, ScientificName_accepted, Genus, Species)
# setdiff(listmatch$ScientificName, listmatch$ScientificName_accepted)

##### creating an empty taxonomy table (using information from accepted names) to populate (output: newtax_un) #####
newtax_un <- tax[0,]

#adding enough empty rows
newtax_un[1:length(listmatch2$ScientificName),]<-NA

#checking
#View(newtax_un)

##### adding information from (listmatch_acc from Worms) to taxonomy table
newtax_un$ScientificName <- listmatch2$ScientificName_accepted
newtax_un$AphiaID <- listmatch2$AphiaID_accepted
newtax_un$ScientificNameAuthorship <- listmatch2$Authority_accepted
newtax_un$Phylum <- listmatch2$Phylum
newtax_un$Class <- listmatch2$Class
newtax_un$Order <- listmatch2$Order
newtax_un$Family <- listmatch2$Family
newtax_un$Genus <- listmatch2$Genus
newtax_un$Subgenus <- listmatch2$Subgenus
newtax_un$Species <- listmatch2$Species
newtax_un$Subspecies <- listmatch2$Subspecies

# # view it
# View(newtax_un)

# get rid of non-matched where NA in ScientificName
newtax_un <- newtax_un[is.na(newtax_un$AphiaID) == FALSE, ]

# # view it
# View(newtax)

##### split/apply/combine: work on Genus 'sp.' issue and TaxonRank in the unaccepted 'newtax_un' (output: 'newtax_un') #####

genus_fix <- newtax_un %>%
  filter(is.na(Species) == T,
         is.na(Genus) == F
  )

genus_nofix <- newtax_un %>%
  filter(is.na(Species) == F,
         is.na(Genus) == F
  )

# check
# genus_fix %>% dplyr::select(Genus, Species)
# genus_nofix %>% dplyr::select(Genus, Species)


# apply changes to genus_fix and genus_nofix

genus_fix$ScientificName <- paste(genus_fix$ScientificName, "sp.")
genus_fix$TaxonRank <- 'genus'
genus_nofix$TaxonRank <- 'species'

# combine

newtax_un <- rbind(genus_fix, genus_nofix)

# #check
# View(genus_fix)
# View(genus_nofix)
# View(newtax_un)
# newtax %>% dplyr::select(Genus, Species)

##### filter out duplicates with existing taxonomy table #####
newtax_un_nodups <- newtax_un[newtax_un$ScientificName %in% tax$ScientificName == F,]
newtax_un_nodups$SynonymAphiaID <- '-999'

##### combine new taxa with original file (output: tax2) #####
tax2<-rbind(tax, newtax_nodups)#, newtax_un_nodups)
tax2<-rbind(tax, newtax)#, newtax_un_nodups)
##### writing the new joint taxonomy table to disc (output: new csv of taxonomy table) #####
setwd("C:/rworking/digs/outdata")
write.csv(tax2,"20181109-0_taxonomy.csv", row.names = F, quote = T)

##### *** END *** #####
##### write 'list' to CSV file output to csv file #####
setwd("C:/rworking/digs/outdata")
write.csv(list,"list.csv", row.names = F, quote = T)

##### get full classification databases using taxize #####
classification(list, db = "eol")
classification(list, db = "ncbi")
classification(list, db = "col")
classification(list, db = "gbif")
classification(list, db = "tropicos")
classification(list, db = "gbif")
classification(list, db = "nbn")
classification(list, db = "natserv")
classification(list, db = "worms")

##### get full classification just from WoRMS #####
classification(list, db = "worms")

##### resolve a taxonomic name in a list (get ScientificNameAuthorship) #####
x <- gnr_resolve(names = list)
View(x)

##### ***** making comparisons **** #####
##### compare list of taxa of interest to current taxonomy tables #####
# # number not in master list
# length(setdiff(list, tax$ScientificName))
# # number not in change list
# length(setdiff(list, taxch$VerbatimScientificName))
# # number not on flagged list
# length(setdiff(list, taxfl$ScientificName))

# choose one comparison for final list
list <- setdiff(list, tax$ScientificName)
list <- setdiff(list, taxch2$VerbatimScientificName)
list <- setdiff(list, taxfl$ScientificName)

##### filtering existing data to include only taxa in the list #####
d2 <- d %>%
  filter(
    ScientificName %in% list
  )

unique(d2$ScientificName)
length(d2$ScientificName)



##### creating a taxonomy table from existing data #####
x <- d2 %>%
  group_by(VernacularNameCategory, VernacularName, ScientificName,
           TaxonRank, AphiaID, Phylum, Class, Subclass, Order, Suborder, Family,
           Subfamily, Genus, Subgenus, Species, Subspecies, ScientificNameAuthorship, Synonyms) %>%
  summarize(n = n())

View(x)

newtax <- x[,1:18]
newtax$SynonymAphiaID <- "-999"
newtax$HigherTaxonNameAuthorship <- "NA"
#View(newtax)
newtax <- ungroup(newtax)
newtax <- data.frame(newtax)

# combine new taxa with original file
tax2<-rbind(tax,newtax)

# exporting the new joint taxonomy table
setwd("C:/rworking/digs/outdata")
write.csv(tax2,"20180830-0_taxonomy_RPMcGuinn.csv", row.names = F, quote = T)

##### adding records to Taxa_to_Flag #####
# #checking
# names(taxfl)
# table(taxfl$FlagReason, )

#making list into a data frame
listdf <- data.frame(list)

#setting variable names
names(listdf) <- c("ScientificName")

#creating Flag and FlagReason Varialbes
listdf$Flag <- "1"
listdf$FlagReason <- "Outside of taxonomic scope"

#binding the new records from the list to taxfl
taxfl2 <- rbind(taxfl,listdf)
taxfl2 <- taxfl2 %>% arrange(ScientificName)

#writing out new taxfl file
setwd("C:/rworking/digs/outdata")
write.csv(taxfl2,"20180321-1_taxa_to_flag.csv", row.names = F, quote = T)

