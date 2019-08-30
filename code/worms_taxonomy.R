##### Header #####
# purpose: Pulling WoRMS taxonomic information into NOAA's National Database for Deep Sea Corals
# author: Robert P. McGuinn, rpm@alumni.duke.edu
# date started: 
# 

##### resources and references #####
# https://cran.r-project.org/web/packages/worms/worms.pdf

##### installing 'worms' package ##### 
#install.packages('worms')
library(worms)

##### load the most current taxonomy from Google Sheets ##### 

taxfl <- gs_title('20190304-0_taxonomy_to_flag')
taxfl <- gs_read(taxfl)

taxch <- gs_title('20190304-0_taxonomy_to_change')
taxch <- gs_read(taxch)

tax <- gs_title('20190304-0_taxonomy')
tax <- gs_read(tax)


# ##### load most current taxonomic tables by CSV #####
# setwd("C:/rworking/digs/indata")
# tax <- read.csv("20181130-0_taxonomy.csv", header = T)
# taxch <- read.csv("20181130-0_taxonomy_to_change.csv", header = T)
# taxfl <- read.csv("20181130-0_taxonomy_to_flag.csv", header = T)
# 
# setwd("C:/rworking/digs/indata")
# tax2 <- read.csv("20181127-0_taxonomy.csv", header = T)
# taxch2 <- read.csv("20181127-0_taxonomy_to_change.csv", header = T)
# taxfl2 <- read.csv("20181127-0_taxonomy_to_flag.csv", header = T)

##### get rid of taxa with cf in front ##### 
cleantax <- tax[!grepl("^cf.", tax$ScientificName),]

##### create the taxon names list with the clean data #####
taxon_names1 <- as.character(tax$ScientificName[1:1499])

##### write taxon names #####
setwd("C:/rworking/digs/outdata")
write.csv(taxon_names1,"20181204_0_Taxonomy_Table_names1_version_20181130-0.csv", row.names = F, quote = T)

##### worms matching #####

taxa <- 'Coralidae'
match <- wormsbymatchnames(taxa, ids = FALSE, verbose = TRUE,
             chunksize = 50, marine_only = "true",
             sleep_btw_chunks_in_sec = 0.2)

View(match)






