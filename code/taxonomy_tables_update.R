##### Header#####
# started: 20170628
# Robert McGuinn
# robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
# 843-460-9696, 843-830-8845

##### packages #####

library(tidyverse)
library(googledrive)
library(googlesheets)

##### load the most current taxonomy from Google Sheets #####
# https://drive.google.com/open?id=0B9c2c_XdhpFBT29NQmxIeUQ4Tlk

n <- '20191217-2'

taxfl <- gs_title(paste(n, '_taxonomy_to_flag',sep = ''))
#gs_browse(taxfl)
taxfl <- gs_read(taxfl)

taxch <- gs_title(paste(n, '_taxonomy_to_change', sep = ''))
#gs_browse(taxch)
taxch <- gs_read(taxch)

tax <- gs_title(paste(n, '_taxonomy', sep = ''))
#gs_browse(tax)
tax <- gs_read(tax)

##### replace sp.#####

tax$ScientificName <- str_replace(tax$ScientificName, " sp.", "")
taxch$ScientificName <- str_replace(taxch$ScientificName, " sp.", "")

## getting rid of duplicates now that ' .sp' is gone
taxch <- taxch[!(taxch$VerbatimScientificName == taxch$ScientificName),]

## write out results
setwd("C:/rworking/deepseatools/indata")

taxch %>%
  write.csv(paste("20200303-0", '_taxonomy_to_change', ".csv", sep = ''), row.names = FALSE)

tax %>%
  write.csv(paste("20200303-0", '_taxonomy', ".csv", sep = ''), row.names = FALSE)

## check
tax %>% dplyr::select(ScientificName, ScientificNameNew) %>% View()

##### load in subset alone without running QA dash #####
x <- "20191216-0_UnpurgedRecords_THourigan"
setwd("C:/rworking/deepseatools/indata")
sub <- read.csv(paste(x,'.csv', sep = ''), header = T)

##### load in subset from Excel #####
setwd("C:/rworking/deepseatools/indata")
sub <- read.xlsx('20191216-0_UnpurgedRecords_THourigan.xlsx', sheet = 1)

##### get records where mismatch to master taxonomy table #####

sub1 <- sub %>% filter(FlagReason == "Insufficient taxonomic information")

##### checking #####

length(setdiff(unique(sub1$ScientificName), tax$ScientificName))

##### create a character vector of non-matching taxa #####

taxa <- as.character(setdiff(unique(sub1$ScientificName), tax$ScientificName))

# taxa
# length(taxa)

##### -OR- just bring in a list of mismatches #####

setwd("C:/rworking/deepseatools/indata")
taxa <- read.csv('taxa.csv', header = F)
taxa$V1 <- gsub("'", '', taxa$V1)

##### -OR- bring in a single taxa

taxa <- as.character("Taiaroa")

#### __OPTIONAL__ break them into chunks for WoRMs interface #####
taxa1 <- taxa[1:50]
taxa2 <- taxa[51:66]
taxa1 <- taxa

##### checking #####

length(taxa1)
length(taxa2)

##### match chunks with with WoRMS database 50 at a time then rbind them #####

x <- wm_records_taxamatch(name = taxa,
                          ids = TRUE,
                          verbose = TRUE,
                          marine_only = TRUE,
                          sleep_btw_chunks_in_sec = 0.2
)

# create a proper data frame from the list

x <- bind_rows(x, .id = "column_label")

# merge back to get original submitted names

y <- merge(taxa, x, by.x = "row.names", by.y = 'column_label')


##### do next block of records #####
x <- wm_records_taxamatch(name = taxa2,
                          ids = TRUE,
                          verbose = TRUE,
                          marine_only = TRUE,
                          sleep_btw_chunks_in_sec = 0.2
)

# create a proper data frame from the list

x <- bind_rows(x, .id = "column_label")

# merge back to get original submitted names

z <- merge(taxa2, x, by.x = "row.names", by.y = 'column_label')

# bind the two files together

match <- rbind(y,z)

# OR
match <- y

##### getting rid of unaccepted taxa #####

names(match)
table(match$status)
match <- match %>% filter(status != 'nomen dubium')

##### creating an empty taxonomy table (using information from accepted names) to populate (output: newtax_un) #####
newtax_un <- tax[0,]

#adding enough empty rows
newtax_un[1:length(match$scientificname),] <- NA

#checking
#View(newtax_un)

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

##### binding new taxonomy table with existing #####

newtax <- rbind(tax, newtax_un)

# # checking
#
# length(tax$VernacularNameCategory)
# length(newtax_un$Class)
# length(newtax$VernacularNameCategory)


##### write out new file #####
setwd("C:/rworking/deepseatools/indata")

newtax %>%
  write.csv('newtax.csv', row.names = FALSE)

##### _____working with rGBIF #####

# Search by type of record, all observational in this case
occ_count(basisOfRecord='OBSERVATION')

# Records for Puma concolor with lat/long data (georeferened) only. Note that hasCoordinate in occ_search() is the same as georeferenced in occ_count().
occ_count(taxonKey=2435099, georeferenced=TRUE)
#> [1] 3747

# All georeferenced records in GBIF
occ_count(georeferenced=TRUE)

# Records from Denmark
denmark_code <- isocodes[grep("Denmark", isocodes$name), "code"]
occ_count(country=denmark_code)

# Number of records in a particular dataset
# this key is Smithsonian: https://www.gbif.org/dataset/821cc27a-e3bb-4bc5-ac34-89ada245069d
occ_count(datasetKey='821cc27a-e3bb-4bc5-ac34-89ada245069d')

# All records from 2012
occ_count(year=2012)
#> [1] 44688340

# Records for a particular dataset, and only for preserved specimens
smithsonianKey <- '821cc27a-e3bb-4bc5-ac34-89ada245069d'
occ_count(datasetKey = smithsonianKey)

# Looking up dates
out <- name_lookup(query='mammalia')
out$meta
View(out$data)

x <- occ_download_get("0000796-171109162308116") %>% occ_download_import()

##### this is how you search gbif and extract data.frames#####
# for institutionCode = USNM, catalogNumber matches USNM#
x <- occ_search(scientificName = "Lophelia pertusa", limit = 100)
View(x)
class(x)
y <- data.frame(x$data)

y %>% #filter(institutionCode == "USNM") %>%
  group_by(scientificName, catalogNumber, occurrenceID) %>%
  summarise(n = n())

x<-occ_search(institutionCode = "USNM")
y<-data.frame(x$data)

z<-y %>% filter(grepl("EX",recordNumber))
View(y)



