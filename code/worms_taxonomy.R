##### Header #####
# purpose: Pulling WoRMS taxonomic information into NOAA's National Database for Deep Sea Corals
# author: Robert P. McGuinn, rpm@alumni.duke.edu
# date started:20200312

##### resources and references #####
# https://cran.r-project.org/web/packages/worms/worms.pdf

##### load packages #####
library(worrms)
library(rmarkdown)
library(tidyverse)
library(googlesheets4)
library(googledrive)

##### load taxonomy 1#####
# manual change required: set version variable.
version <- '20201001-0'
taxtoflag <- paste(version,'_taxonomy_to_flag', sep = '')
taxtochange <- paste(version,'_taxonomy_to_change', sep = '')
taxonomyall <- paste(version,'_taxonomy_all', sep = '')

# find the file in google drive by name
x <- drive_find(q = paste("name contains ", "'", taxtoflag, "'", sep = ''))
y <- x$id
taxfl <- read_sheet(y)

# find the file in google drive by name
x <- drive_find(q = paste("name contains ", "'", taxtochange, "'", sep = ''))
y <- x$id
taxch <- read_sheet(y)

# find the file in google drive by name
x <- drive_find(q = paste("name contains ", "'", taxonomyall, "'", sep = ''))
y <- x$id
tax <- read_sheet(y)

##### create the taxon names list #####

## OR get from ScientificName list
# taxa <- as.character(tax$ScientificName[1:10])

## OR get from manual list
# taxa <- c("Adelogorgia cf. phyllosclera", "Putamayo", "Adelogorgia", "Lophelia pertusa")

## OR check against current sub list
taxa <- as.character(setdiff(unique(sub$ScientificName), tax$ScientificName))

## OPTIONAL break up for worms API (optional for long lists)
# taxa1 <- taxa[1:50]
# taxa2 <- taxa[51:66]

##### match taxa #####
x <- wm_records_taxamatch(name = taxa,
                          ids = TRUE,
                          verbose = TRUE,
                          marine_only = TRUE
                          #sleep_btw_chunks_in_sec = 0.2
                          )
z <- bind_rows(x, .id = "column_label")
# View(z)

## check
# unique(z$scientificname)
# setdiff(unique(z$scientificname), tax$ScientificName)
# length(unique(tax$ScientificName))
# z$scientificname

##### merge back to get original submitted names #####
match <- merge(taxa, z, by.x = "row.names", by.y = 'row.names')

##### creating an empty taxonomy tableopulate (output: newtax_un) #####
newtax_un <- tax[0,]

#adding enough empty rows
newtax_un[1:length(match$scientificname),] <- NA

##### adding information from (match from Worms) to taxonomy table #####
newtax_un$ScientificName <- match$valid_name
newtax_un$AphiaID <- match$valid_AphiaID
newtax_un$ScientificNameAuthorship <- match$valid_authority
newtax_un$Phylum <- match$phylum
newtax_un$Class <- match$class
newtax_un$Order <- match$order
newtax_un$Family <- match$family
newtax_un$Genus <- match$genus
newtax_un$TaxonRank <- match$rank
newtax_un$SynonymAphiaID <- "-999"
newtax_un$HigherTaxonNameAuthorship <- NA

##### extracting only taxa that don't match the current taxonomy sheet #####
diff <- setdiff(newtax_un$ScientificName, tax$ScientificName)
newtax_un <- newtax_un %>% filter(ScientificName %in% diff)

##### binding new taxonomy table with existing #####
newtax <- rbind(tax, newtax_un)
## checking
#
# length(tax$VernacularNameCategory)
# length(newtax_un$Class)
# length(newtax$VernacularNameCategory)

##### write out new file #####
setwd("C:/rworking/deepseatools/indata")
newtax %>%
  write.csv('newtax.csv', row.names = FALSE)

##### checking #####
# x <- tax %>% filter(grepl('Chromoplexaura', Genus)) %>% pull(VernacularNameCategory)
# x <- tax %>% filter(grepl('Plexauridae', Family)) %>%
#   group_by(Phylum,Class,Subclass,Order,Suborder,Family,Subfamily,Genus) %>%
#   summarize(n=n())
# x

##### publishing change list #####
x <- match$x
y <- match$valid_name
x_name <- "VerbatimScientificName"
y_name <- "ScientificName"

df <- data.frame(x,y)
names(df) <- c(x_name,y_name)
diff <- setdiff(taxa, newtax$ScientificName)
change_taxonomy <- df %>% filter(VerbatimScientificName %in% diff)
change_taxonomy

newtaxch <- rbind(taxch, change_taxonomy)
names(taxch)
names(change_taxonomy)

setwd("C:/rworking/deepseatools/indata")
newtaxch %>%
  write.csv('newtaxch.csv', row.names = FALSE)
