##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231201
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '122876' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename,'.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(worrms)
library(taxize)

##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20231121
## purpose: see redmine issue

##### linkage #####
## manual input here
filename <- '20231121-0_Laurence_Helene_De_Clippele_RPMcGuinn.R' ## for this code .R
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
## manual input here
issuenumber <- '122876'
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)
othercode <- c('C:/rworking/deepseatools/code/20231121-0_Laurence_Helene_De_Clippele_RPMcGuinn.R')

##### packages #####
library(tidyverse)
library(sf)
library(googlesheets4)

##### authorizations #####
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### *** ORIGINAL *** #####
setwd('c:/rworking/deepseatools/indata')
filename <- 'JC073_ROV_species_enviromental_variables_40m.tab'
sub <- read.delim(filename,
                  sep='\t',
                  header = T,
                  skip = 50)

##### check #####
names(sub)

##### import schema from google drive #####
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

##### transform data set #####
sub2 <- sub %>%
  pivot_longer(cols = Antipatharia.sp.......Antipatharia.sp..1.:Bathynectes.sp.....,
               names_to = "ScientificName",
               values_to = "IndividualCount")

##### check #####
# View(sub2)

##### fix data #####
## for a certain subset, the lat and long has been switched
## identify records where latitude and longitude are flipped
flipped_records <- sub2$Latitude < 55

# Swap latitude and longitude for flipped records
sub2[flipped_records, c("Latitude", "Longitude")] <- sub2[flipped_records, c("Longitude", "Latitude")]

##### create sf object #####
points <- st_as_sf(sub2, coords = c("Longitude", "Latitude"), crs = 4326)

##### export shapefile #####
st_write(points,
         "C:/rworking/deepseatools/indata/geo3.shp",
         append = F)

##### check #####
# filt %>% filter(grepl('James Cook', Vessel)) %>%
#   pull(Vessel) %>% table()
#
# filt %>% filter(grepl('James Cook', Vessel)) %>%
#   pull(SurveyID) %>% table()
#
# filt %>% filter(grepl('Roberts', PI)) %>%
#   pull(PI) %>% table()
#
# filt %>% filter(RecordType == 'literature') %>%
#   pull(DataProvider) %>% unique()
#
# filt %>% filter(RecordType == 'literature') %>%
#   pull(DatasetID) %>% unique()

##### add information from DSCRTP schema #####
sub2$Vessel <- "James Cook R/V"
sub2$SurveyID <- "JC073"
sub2$VehicleName <- "Holland 1"
sub2$SamplingEquipment <- "ROV"
sub2$Citation <- "https://www.frontiersin.org/articles/10.3389/fmars.2019.00184/full | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/reports/jc073.pdf"
sub2$WebSite <- "https://www.frontiersin.org/articles/10.3389/fmars.2019.00184/full | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/reports/jc073.pdf | https://www.bodc.ac.uk/resources/inventories/cruise_inventory/report/11421/ | | http://rvinfobase.eurocean.org/spec/vessel.jsp?id=4140 | https://doi.pangaea.de/10.1594/PANGAEA.909118"
sub2$RecordType <- "video observation"
sub2$NavType <- "USBL"
sub2$LocationAccuracy <- "20m"
sub2$ObservationDate <- "2012-05-18" ## May 18–June 15, 2012
sub2$Locality <- "Logachev Mound Province"
sub2$Habitat <- sub2$Stones......Dropstone.
sub2$DepthInMeters <- sub2$Depth.water..m.
sub2$MinimumDepthInMeters <- sub2$Depth.water..m.
sub2$MaximumDepthInMeters <- sub2$Depth.water..m.
sub2$PI <- "De Clippele, Laurence Helene"
sub2$PIAffiliation <- "University of Edinburgh, Edinburgh, United Kingdom"
sub2$DataProvider <- "De Clippele et al. 2019"
sub2$DatasetID <- "De_Clippele_etal_2019"
sub2$DataContact <- "De Clippele, Laurence Helene"
sub2$Repository <- "National Oceanography Centre, British Oceanographic Data Centre"
sub2 <- sub2 %>% mutate(CategoricalAbundance = ifelse(IndividualCount == 0, 'absent', 'present'))
sub2$VerbatimScientificName <- sub2$ScientificName
sub2$ScientificName <- 'NA'
sub2$SampleID <- 'NA'
sub2$DepthMethod <- 'reported'
sub2$IdentifiedBy <- 'De Clippele, Laurence Helene'
sub2$EventID <- 'NA'
sub2$Modified <- '2023-11-22'
sub2$Reporter <- 'McGuinn, Robert P.'
sub2$ReporterEmail <- 'robert.mcguinn@noaa.gov'
sub2$AccessionID <- 'National_Oceanography_Centre_James_Cook_JC073_2012'

##### add AphiaID #####
sub2 <- sub2 %>%
  mutate(
    AphiaID = case_when(
      ScientificName == "Antipatharia.sp.......Antipatharia.sp..1." ~ "22549",
      ScientificName == "Antipatharia.sp.......Antipatharia.sp..2." ~ "22549",
      ScientificName == "Paramuricea.sp....." ~ "125311",
      ScientificName == "Trissopathes.sp.......Trissopathes.sp..1." ~ "267926",
      ScientificName == "Acanella.sp....." ~ "125303",
      ScientificName == "Leiopathes.sp.......small." ~ "103305",
      ScientificName == "Leiopathes.sp.......medium." ~ "103305",
      ScientificName == "Leiopathes.sp.......large." ~ "103305",
      ScientificName == "Bathypathes.sp....." ~ "103304",
      ScientificName == "Stichopathes......Stichopathes.cf..Gravieri." ~ "103308",
      ScientificName == "Parantipathes.sp.......Parantipathes.sp..1." ~ "103306",
      ScientificName == "Parantipathes.sp.......Parantipathes.sp..2." ~ "103306",
      ScientificName == "Shrimps...." ~ "106674",
      ScientificName == "Hydrozoa...." ~ "1337",
      ScientificName == "Gastroptyctus.sp....." ~ "106832",
      ScientificName == "Shark.egg...." ~ "1517375",
      ScientificName == "Asteroidea.sp....." ~ "123080",
      ScientificName == "P..cuvieri...." ~ "107264",
      ScientificName == "Ophiuroidea...." ~ "123084",
      ScientificName == "Sponge.r......Massive.red.sponge." ~ "558",
      ScientificName == "Crinoidea......Crinoidea.sp..1." ~ "123081",
      ScientificName == "Crinoidea......Crinoidea.sp..2." ~ "123081",
      ScientificName == "Crinoidea......Crinoidea.sp..3." ~ "123081",
      ScientificName == "C..cidaris...." ~ "124257",
      ScientificName == "Munida.sp....." ~ "106835",
      ScientificName == "C..affinis...." ~ "107369",
      ScientificName == "Bathynectes.sp....." ~ "106920",
      TRUE ~ ""  # default case, if none of the above conditions are met
    )
  )

##### check #####
# table(sub2$AphiaID, useNA = 'always')
# setdiff(names(sub2), s$FieldName)
# x <- s %>% filter(PointNew == "R") %>% pull(FieldName)
# setdiff(x, names(sub2))

##### get rid of un-wanted variables #####
sub2 <- sub2 %>%
  select(-c(
    "BPI...i6xo9..",
    "BPI...i3xo6..",
    "Rugosity...i9xo9..",
    "Rugosity...i3xo3..",
    "Aspect..arbitrary.units...Northness.",
    "Aspect..arbitrary.units...Eastness.",
    "Aspect..arbitrary.units.",
    "Backsc",
    "Depth.water..m.",
    "Stones......Dropstone."
  ))

##### check ####
# setdiff(s$FieldName, names(sub2))
# setdiff(names(sub2),s$FieldName)

##### write CSV #####
write.csv(sub2,
          "c:/rworking/deepseatools/indata/20231122-0_National_Oceanography_Centre_James_Cook_JC073_2012.csv")



##### load original data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20231122-2_National_Oceanography_Centre_James_Cook_JC073_2012.csv')
##### check #####

##### *** NEW VERSION *** #####
##### load data #####
setwd('c:/rworking/deepseatools/indata')
sub <- read.csv('20231122-2_National_Oceanography_Centre_James_Cook_JC073_2012.csv')

##### check #####
# table(sub$AphiaID, useNA = 'always')
# table(sub$VerbatimScientificName, useNA = 'always')
# length(unique(sub$AphiaID))
# table(sub$DataProvider, useNA = 'always')
# table(sub$Repository, useNA = 'always')
# table(sub$WebSite, useNA = 'always')
# table(sub$Citation, useNA = 'always')

##### add AphiaID #####
sub <- sub %>%
  mutate(
    AphiaID = case_when(
      VerbatimScientificName == "Antipatharia.sp.......Antipatharia.sp..1." ~ "22549",
      VerbatimScientificName == "Antipatharia.sp.......Antipatharia.sp..2." ~ "22549",
      VerbatimScientificName == "Paramuricea.sp....." ~ "125311",
      VerbatimScientificName == "Trissopathes.sp.......Trissopathes.sp..1." ~ "267926",
      VerbatimScientificName == "Acanella.sp....." ~ "125303",
      VerbatimScientificName == "Leiopathes.sp.......small." ~ "103305",
      VerbatimScientificName == "Leiopathes.sp.......medium." ~ "103305",
      VerbatimScientificName == "Leiopathes.sp.......large." ~ "103305",
      VerbatimScientificName == "Bathypathes.sp....." ~ "103304",
      VerbatimScientificName == "Stichopathes......Stichopathes.cf..Gravieri." ~ "103308",
      VerbatimScientificName == "Parantipathes.sp.......Parantipathes.sp..1." ~ "103306",
      VerbatimScientificName == "Parantipathes.sp.......Parantipathes.sp..2." ~ "103306",
      VerbatimScientificName == "Shrimps...." ~ "106674",
      VerbatimScientificName == "Hydrozoa...." ~ "1337",
      VerbatimScientificName == "Gastroptyctus.sp....." ~ "106832",
      VerbatimScientificName == "Shark.egg...." ~ "10193",
      VerbatimScientificName == "Asteroidea.sp....." ~ "123080",
      VerbatimScientificName == "P..cuvieri...." ~ "107264",
      VerbatimScientificName == "Ophiuroidea...." ~ "123084",
      VerbatimScientificName == "Sponge.r......Massive.red.sponge." ~ "558",
      VerbatimScientificName == "Crinoidea......Crinoidea.sp..1." ~ "123081",
      VerbatimScientificName == "Crinoidea......Crinoidea.sp..2." ~ "123081",
      VerbatimScientificName == "Crinoidea......Crinoidea.sp..3." ~ "123081",
      VerbatimScientificName == "C..cidaris...." ~ "124257",
      VerbatimScientificName == "Munida.sp....." ~ "106835",
      VerbatimScientificName == "C..affinis...." ~ "107369",
      VerbatimScientificName == "Bathynectes.sp....." ~ "106920",
      TRUE ~ ""  # default case, if none of the above conditions are met
    )
  )

sub$AphiaID <- as.numeric(sub$AphiaID)

##### check #####
# sub %>% pull(AphiaID) %>% table(useNA = 'always')
# length(unique(sub$AphiaID))
# length(unique(sub$VerbatimScientificName))
# length(unique(sub$ScientificName))

##### make any corrections #####

##### check #####
# dim(sub)

##### load the taxonomy table from CSV #####
tax <- read.csv("C:/rworking/deepseatools/indata/tax.csv")

##### create vector from incoming AphiaIDs #####
my_vector <- as.numeric(unique(sub$AphiaID))

##### check #####
# length(my_vector)

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list_original <- df

##### check #####
# dim(species_list_original)
# View(species_list_original)
#
# table(species_list_original$AphiaID, useNA = 'always')
#
# table(sub$AphiaID, useNA = 'always')
#
# sub %>% filter(AphiaID == -999) %>% pull(ScientificName)
#
# species_list_original %>% filter(is.na(AphiaID) == T) %>%
#   pull(scientificname)
#
# table(sub$AphiaID, useNA = 'always')
#
# setdiff(species_list_original$AphiaID, sub$AphiaID)
#
# setdiff(sub$AphiaID, species_list_original$AphiaID)
#
# View(species_list_original)
#
# table(species_list_original$status, useNA = 'always')
# species_list_original %>% filter(status != 'accepted') %>% View()

##### create a complete valid AphiaID list #####
species_list_original <- species_list_original %>%
  mutate(valid_AphiaID_complete = ifelse(is.na(valid_AphiaID) == T,
                                         AphiaID,
                                         valid_AphiaID))

##### check #####
# species_list_original %>% filter(status != 'accepted') %>%
#   group_by(AphiaID, valid_AphiaID, valid_AphiaID_complete) %>%
#   summarize(n=n()) %>% View()

##### create vector from valid AphiaIDs #####
my_vector <- as.numeric(unique(species_list_original$valid_AphiaID_complete))

## make groups of 50 (because the API limit is 50)
my_groups <- split(my_vector, ceiling(seq_along(my_vector)/50))

##### loop to get records by the valid AphiaID #####
species_list <- wm_records_name("Caryophyllia corrugata", fuzzy = FALSE)
df <- species_list[0,]

for (i in seq_along(my_groups)){
  species_list <- wm_record(my_groups[[i]])
  df <- rbind(df, species_list)
}
species_list <- df

##### check #####
# View(species_list)
# table(is.na(species_list$AphiaID))
# table(species_list$status)
# dim(species_list)

##### loop to get classification #####
df <- data.frame(
  Domain = character(),
  Kingdom = character(),
  Subkingdom = character(),
  Phylum = character(),
  Subphylum = character(),
  Superclass = character(),
  Class = character(),
  Subclass = character(),
  Infraclass = character(),
  Superorder = character(),
  Order = character(),
  Suborder = character(),
  Infraorder = character(),
  Superfamily = character(),
  Family = character(),
  Subfamily = character(),
  Tribe = character(),
  Subtribe = character(),
  Genus = character(),
  Subgenus = character(),
  Species = character(),
  Subspecies = character(),
  Variety = character(),
  stringsAsFactors=FALSE)

## loop to get full classification
for (i in my_vector){
  try(classification <- wm_classification(i))
  classification_wide <- classification %>%
    select(rank,scientificname) %>%
    pivot_wider(
      names_from = rank,
      values_from = scientificname
    )
  classification_wide$AphiaID <- i
  df <- bind_rows(df, classification_wide)
}

classification <- df

##### loop to get vernacular name #####
## initialize an empty list to store successful results
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  vernaculars <- try({
    wm_result <- wm_common_id(i)
    wm_result %>% filter(language == 'English')
  }, silent = TRUE) ## Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_common_id()
  if (!inherits(vernaculars, "try-error")) {
    ## If no error, proceed to append the data to the result_list
    vernaculars_list <- paste(vernaculars$vernacular, collapse = " | ")
    AphiaID <- i
    vernaculars_wide <- data.frame(AphiaID, vernaculars_list)
    result_list[[length(result_list) + 1]] <- vernaculars_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
vernaculars <- df

##### check #####
# wm_common_id(1567760)

##### loop to get synonyms #####
## initialize a results list
result_list <- list()

## loop through my_vector
for (i in my_vector) {
  ## use try() to handle errors
  synonyms <- try({
    wm_result <- wm_synonyms(i) # i = 423632
  }, silent = TRUE) # Use silent = TRUE to suppress error messages

  ## check if there was an error in wm_synonyms()
  if (!inherits(synonyms, "try-error")) {
    ## if no error, proceed to append the data to the result_list
    synonyms_list <- paste(synonyms$scientificname, collapse = " | ")
    AphiaID <- i
    synonyms_wide <- data.frame(AphiaID, synonyms_list)
    result_list[[length(result_list) + 1]] <- synonyms_wide
  }
}

## Combine the successful results into the final data frame df
df <- do.call(rbind, result_list)
synonyms <- df

##### check #####
# View(classification)
# View(vernaculars)
# View(classification)
# View(synonyms)
#
# dim(species_list)
# dim(classification)
# dim(vernaculars)
# dim(synonyms)
#
# names(species_list)
# names(classification)
# names(vernaculars)
# names(synonyms)
#
# head(species_list$AphiaID)
# head(classification$AphiaID)
# head(vernaculars$AphiaID)
# head(synonyms$AphiaID)
#
# tail(tax_tom_enhanced$AphiaID)
# tail(classification$AphiaID)
# tail(vernaculars$AphiaID)
# tail(synonyms$AphiaID)
#
# class(tax_tom_enhanced$AphiaID)
# class(classification$AphiaID)
# class(vernaculars$AphiaID)
# class(synonyms$AphiaID)

##### left join the summary from above with all of the other API tables #####
by <- join_by(valid_AphiaID == AphiaID)
joined2 <- left_join(species_list, classification, by)

by <- join_by(valid_AphiaID == AphiaID)
joined3 <- left_join(joined2, vernaculars, by)

by <- join_by(valid_AphiaID == AphiaID)
joined4 <- left_join(joined3, synonyms, by)

##### check #####
# names(joined4)
# setdiff(joined4$AphiaID, species_list_original$valid_AphiaID_complete)
# setdiff(species_list_original$valid_AphiaID_complete, joined4$AphiaID)

##### join original table with the new table #####
joined4$AphiaID2 <- joined4$AphiaID
by <- join_by(valid_AphiaID_complete == AphiaID2)
taxonomy_table <- left_join(species_list_original, joined4, by)
# View(taxonomy_table)
# names(taxonomy_table)

##### add taxonomy to sub #####
sub$AphiaID <- as.numeric(sub$AphiaID)
by <- join_by(AphiaID == AphiaID.x)
sub_enhanced <- left_join(sub, taxonomy_table, by)

##### check #####
# sub_enhanced %>% filter(is.na(phylum.y) == T) %>%
#   pull(ScientificName) %>%
#   unique()
#
# dim(sub)
# dim(sub_enhanced)

##### gather information into proper variables #####
sub_enhanced$VerbatimScientificName <- sub$ScientificName
sub_enhanced$ScientificName <- sub_enhanced$scientificname.y
sub_enhanced$VernacularName <- sub_enhanced$vernaculars_list
sub_enhanced$TaxonRank <- sub_enhanced$rank.y
sub_enhanced$AphiaID <- sub_enhanced$valid_AphiaID_complete
sub_enhanced$Phylum <- sub_enhanced$phylum.y
sub_enhanced$Class <- sub_enhanced$Class.y
sub_enhanced$Subclass <- sub_enhanced$Subclass.y
sub_enhanced$Order <- sub_enhanced$Order.y
sub_enhanced$Suborder <- sub_enhanced$Suborder.y
sub_enhanced$Family <- sub_enhanced$Family.y
sub_enhanced$Subfamily <- sub_enhanced$Subfamily.y
sub_enhanced$Genus <- sub_enhanced$Genus.y
sub_enhanced$Subgenus <- sub_enhanced$Subgenus.y
sub_enhanced$Species <- word(sub_enhanced$Species.y, -1)
sub_enhanced$Subspecies <- sub_enhanced$Subspecies.y
sub_enhanced$ScientificNameAuthorship <- sub_enhanced$authority.y
sub_enhanced$Synonyms <- sub_enhanced$synonyms_list

##### check #####
# table(sub_enhanced$Phylum, useNA = 'always')
#
# sub_enhanced_filter %>% filter(Class == 'Hydrozoa') %>%
#   group_by(Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### apply taxonomic filter #####
sub_enhanced_filter <- sub_enhanced %>%
  filter(Subphylum == 'Vertebrata' |
           Phylum == 'Cnidaria' |
           Phylum == 'Porifera')

`%notin%` <- Negate(`%in%`)
sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Class %notin% c('Scyphozoa', 'Thalicacea', 'Ascidiacea'))

sub_enhanced_filter <- sub_enhanced_filter %>%
  filter(Order == 'Scleractinia' |
           Order == 'Antipatharia' |
           Genus == 'Savalia' |
           Genus == 'Kulamanamana' |
           Genus == 'Gerardia' |
           Family == 'Stylasteridae' |
           Order  == 'Alcyonacea' |
           Order ==  'Gorgonacea' |
           Order ==  'Helioporacea' |
           Order == 'Pennatulacea' |
           Order == 'Scleralcyonacea' |
           Family == 'Stylasteridae' |
           Genus == 'Solanderia' |
           Genus == 'Janaria' |
           Genus == 'Hydrocorella' |
           Genus == 'Hydrodendron' |
           Phylum == 'Chordata' |
           Phylum == 'Porifera' |
           Order == 'Malacalcyonacea'
  )


##### check #####
# table(sub_enhanced_filter$Phylum, useNA = 'always')
# table(sub_enhanced_filter$Subphylum, useNA = 'always')
# table(sub_enhanced_filter$Class, useNA = 'always')
#
# sub_enhanced_filter %>%
#   group_by(AphiaID, Phylum, Subphylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### assign VernacularNameCategory #####
## define not in
`%notin%` <- Negate(`%in%`)

gorgfamilies <- c("Paramuriceidae","Chrysogorgiidae","Dendrobrachiidae",
                  "Ellisellidae", "Isididae",
                  "Pleurogorgiidae", "Primnoidae",
                  "Acanthogorgiidae", "Gorgoniidae","Keroeididae",
                  "Plexauridae", "Anthothelidae",
                  "Coralliidae", "Melithaeidae",
                  "Paragorgiidae", "Parisididae","Spongiodermidae", "Subergorgiidae",
                  "Victorgorgiidae", "Keratoisididae", "Malacalcyonacea incertae sedis")

softfamilies <- c("Alcyoniidae","Aquaumbridae", "Ifalukellidae",
                  "Nephtheidae","Nidaliidae", "Paralcyoniidae",
                  "Xeniidae", "Taiaroidae")

othercorallikehydrozoanfamilies <- c("Solanderiidae", "Haleciidae")

stonycoralbranching <- tax %>%
  filter(VernacularNameCategory == 'stony coral (branching)') %>%
  pull(ScientificName)

stonycoralcupcoral <- tax %>%
  filter(VernacularNameCategory == 'stony coral (cup coral)') %>%
  pull(ScientificName)

sub_enhanced2 <- sub_enhanced_filter %>%
  mutate(VernacularNameCategory = case_when(
    Phylum %in% c('Chordata') ~ 'fish',
    TaxonRank %in% c('Order') &
      Order %in% c('Alcyonacea') ~ 'alcyonacean (unspecified)',
    Order %in% c('Antipatharia') ~ 'black coral',
    Class %in% c('Calcarea')~ 'calcareous sponge',
    Class %in% c('Demospongiae') ~ 'demosponge',
    Class %in% c('Hexactinellida') ~ 'glass sponge',
    Class %in% c('Homoscleromorpha') ~ 'homoscleromorph sponge',
    Family %in% c('Parazoanthidae') ~ 'gold coral',
    Family %in% gorgfamilies ~ 'gorgonian coral',
    Family %in% softfamilies ~ 'soft coral',
    Order %in% c('Malacalcyonacea') ~ 'soft coral)',
    Order %in% c('Anthoathecata') &
      Family %notin%  c('Solanderiidae') ~ 'lace coral',
    Family %in% c('Lithotelestidae') ~ 'lithotelestid coral',
    Family %in% othercorallikehydrozoanfamilies ~ 'other coral-like hydrozoan',
    Superfamily %in% c('Pennatuloidea') ~ 'sea pen',
    ScientificName %in% c('Porifera') ~ 'sponge',
    Suborder %in% c('Stolonifera') ~ 'stoloniferan coral',
    Family %in% c('Clavulariidae') ~ 'stoloniferan coral',
    Genus %in% c('Clavularia') ~ 'stoloniferan coral',
    Order %in% c('Scleractinia') &
      TaxonRank %in% c('Order')  ~ 'stony coral (unspecified)',
    ScientificName %in% stonycoralbranching ~ 'stony coral (branching)',
    ScientificName %in% stonycoralcupcoral ~ 'stony coral (cup coral)',
    Genus %in% c('Acanthogorgia') ~ 'gorgonian coral',
    Genus %in% c('Hydrodendron') ~ 'other coral-like hydrozoan',
    Genus %in% c('Caryophyllia') ~ 'stony coral (cup coral)',
    TRUE ~ ''))

##### check #####
# table(sub_enhanced2$VernacularNameCategory, useNA = 'always')
# filt %>% filter(Order == 'Malacalcyonacea') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# filt %>% filter(Genus == 'Clavularia') %>% pull(VernacularNameCategory) %>%
#   table(useNA = 'always')
#
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == '') %>%
#   pull(ScientificName) %>% unique()
#
# sub_enhanced2 %>%
#   filter(VernacularNameCategory == 'stony coral (cup)') %>%
#   pull(VernacularNameCategory) %>% unique()

##### get rid of unneeded column names #####
names_list <- names(sub)
sub_enhanced2 <- sub_enhanced2 %>%
  dplyr::select(all_of(names_list))

##### select just the taxonomic variables #####
sub_enhanced3<- sub_enhanced2 %>%
  select(CatalogNumber,
         VerbatimScientificName,
         ScientificName,
         VernacularName,
         VernacularNameCategory,
         TaxonRank,
         AphiaID,
         Phylum,
         Class,
         Subclass,
         Order,
         Suborder,
         Family,
         Subfamily,
         Genus,
         Subgenus,
         Species,
         Subspecies,
         ScientificNameAuthorship,
         Synonyms)

##### check #####
# View(sub_enhanced3)
# dim(sub_enhanced3)
# dim(sub)
# length(sub$CatalogNumber) - length(sub_enhanced3$CatalogNumber)
#
# ## look at the lost taxa from the scope filter
# x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
# sub_enhanced %>% filter(CatalogNumber %in% x) %>%
#   group_by(AphiaID, Phylum, Class, Order, Suborder, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()
#
# table(is.na(sub$CatalogNumber))
# table(is.na(sub_enhanced3$CatalogNumber))
# sub %>% filter(ScientificName == 'Dichotella gemmacea') %>% pull(AphiaID)
# 'Dichotella gemmacea'

## look at where name changes occurred
# x <- setdiff(sub_enhanced3$VerbatimScientificName, sub_enhanced3$ScientificName)
# sub_enhanced3 %>% filter(VerbatimScientificName %in% x) %>%
#   group_by(VerbatimScientificName, ScientificName, VernacularNameCategory) %>%
#   summarize(n=n()) %>% View()
#
# x <- setdiff(sub$CatalogNumber, sub_enhanced3$CatalogNumber)
# sub %>% filter(CatalogNumber %in% x) %>% pull(AphiaID)
#
# table(sub_enhanced3$VernacularNameCategory, useNA = 'always')
#
# sub_enhanced3 %>% filter(VernacularNameCategory == '') %>% pull(Order) %>% unique()
#
# sub_enhanced3 %>% filter(VernacularNameCategory == '') %>%
#   group_by(AphiaID, Phylum, Class, Order, Family, Genus, Species) %>%
#   summarize(n=n()) %>% View()

##### export result to csv (export to CSV) #####
filename <- "20231122-2_National_Oceanography_Centre_James_Cook_JC073_2012_122876_taxonomy_patch.csv"
write.csv(sub_enhanced3,
          paste("c:/rworking/deepseatools/indata/",
                filename, sep=''),
          fileEncoding = "latin9",
          row.names = F,
          quote = T)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))


































##### *** NEW VERSION (post-checker) *** #####
setwd('c:/rworking/deepseatools/indata')
filename <- '20231218-1_National_Oceanography_Centre_James_Cook_JC073_2012_2012_122876'
sub <- read.csv(paste(filename, '.csv', sep = ''))

##### packages #####
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)
library(googlesheets4)

##### authorizations #####
## Set authentication token to be stored in a folder called \.secrets``
options(gargle_oauth_cache = ".secrets")

## Authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:

list.files(".secrets/")

## Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

## Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### render the QA dashboard #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'
knitr::opts_knit$set(resource.path = "c:/rworking/deepseatools/code")
setwd('c:/rworking/deepseatools/code')

## render
## manual change version of dashboard version number is required
rmarkdown::render("C:/rworking/deepseatools/code/20230731-0_rmd_accession_qa_dashboard.Rmd",
                  output_file =  paste(filename,".docx", sep=''),
                  output_dir = 'C:/rworking/deepseatools/reports')
##### check #####
# sub %>% filter(grepl('Invalid latitude', FlagReason)) %>% pull(Longitude)
#
# sub %>%
#   # filter(ScientificName == "Keratoisis magnifica") %>%
#   group_by(Flag,
#            FlagReason,
#            ScientificName,
#            Phylum,
#            Class,
#            Order,
#            Family,
#            Genus,
#            Species,
#            VernacularNameCategory
#   ) %>% summarize (n=n()) %>%
#   View()
#
# sub %>% pull(DepthInMeters) %>% median()
# sub %>% filter(DepthInMeters == "68523") %>% pull(SampleID)
# sub %>% pull(SurveyID) %>% table()
# sub %>% pull(RecordType) %>% table()
# s %>% filter(FieldName == "DataContact") %>% pull(FieldDescription)
# sub %>% filter(grepl('>100', VerbatimSize)) %>% pull(MaximumSize)
# sub %>% filter(is.na(LocationAccuracy) == T) %>% rownames()
# sub %>% filter(grepl('>100', VerbatimSize)) %>% rownames()
# rownames(sub %>% filter(grepl('>100', VerbatimSize)))
# sub$row <- rownames(sub)
# sub %>% filter(is.na(LocationAccuracy) == T) %>% pull(row)
# sub %>% filter(row == "246") %>% pull(LocationAccuracy)
# sub %>% filter(row == "247") %>% pull(LocationAccuracy)
# sub %>% filter(row == "248") %>% pull(LocationAccuracy)
# sub %>% slice(246:248) %>% pull(SampleID)
# sub %>% filter(DepthInMeters > 30000) %>% pull(SampleID)
#
# x <- paste(sub$SampleID, sub$ScientificName, sub$VerbatimScientificName)
# table(duplicated(x))
#
# filt %>% filter(grepl("NOAA", DataProvider)) %>% pull(DatasetID) %>% unique()
#
# sub %>% filter(DepthInMeters>500) %>% select(CatalogNumber, DepthInMeters)
#
# yo <- read.delim('c:/rworking/deepseatools/indata/20221031-0_NOAA_EX1304_Northeast_US_SBingo_2013.txt', sep = '\t')
#
# yo %>% filter(DepthInMeters > 7000) %>% select(TrackingID, DepthInMeters)
# sub %>% filter(DepthInMeters > 7000) %>% select(TrackingID, DepthInMeters)
#
# filt %>% filter(grepl("HB-12", DatasetID)) %>% pull(DatasetID) %>% table()
# sub %>% filter(grepl("HB-17", DatasetID)) %>% pull(ImageFilePath) %>% table()
#
# filt %>%
#   filter(grepl("Smithsonian", Repository)) %>%
#   pull(Repository) %>%
#   table()


# Acanthogorgia spissa (N=4)
# Anthomastus gyratus (N=1)
# Anthoptilum gowlettholmesae (N=4)
# Anthothela vickersi (N=5)
# Aphanostichopathes paucispina (N=1)
# Calibelemnon francei (N=1)
# Cladarisis nouvianae (N=2)
# Cornulariidae (N=1)
# Distichopathes hickersonae (N=2)

## manual change: make sure your target RMD in the render function step is correct.
##### MANUAL inspection of QA report in Word #####
## manual: then Develop Redmine Checklist
## manual: then SAVE to PDF
##### upload PDF report to specific folder on Google Drive #####
## MANUAL CHANGE: folderurl to the current drive folder ID for the accession at hand
folderurl <- "https://drive.google.com/drive/folders/1vQaw36d8MCIWDfS11fCwdVLWmj-XWA2H"
setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)
##### checking #####
sub %>% filter(Flag == 1) %>% pull(ScientificName) %>% table(useNA = 'always')

x <- sub %>% filter(DepthInMeters != '-999', Oxygen != '-999', DepthInMeters > 40)
plot(x$DepthInMeters, x$Oxygen)

x <- sub %>% filter(DepthInMeters != '-999', Oxygen != '-999', DepthInMeters > 40, Temperature < 6)
plot(x$DepthInMeters, x$Temperature)

sub %>% filter(Temperature > 10) %>% pull(ScientificName)

sub %>% filter(DepthInMeters < 30, Latitude != -999) %>% pull(ScientificName)

sub %>% filter(Flag == 1, grepl('Invalid', FlagReason)) %>% pull(Longitude)

unique(sub$DatasetID)
filt %>% filter(grepl("HB", DatasetID)) %>% pull(DatasetID) %>% table()
unique(sub$SurveyID)
filt %>% filter(grepl("HB", SurveyID)) %>% pull(SurveyID) %>% table()
unique(sub$EventID)
unique(sub$Citation)
unique(sub$Repository)

x <- filt %>%
  filter(DatasetID == 'MBARI') %>%
  group_by(DataContact, DataProvider, ImageURL) %>%
  summarize(n=n())
View(x)

x <- filt %>%
  filter(FishCouncilRegion == "Caribbean" |
           FishCouncilRegion == "South Atlantic" |
           FishCouncilRegion == "Caribbean") %>%
  group_by(FishCouncilRegion, Vessel, EntryDate, ObservationYear) %>%
  summarize(n=n())
View(x)

x <- filt %>%
  filter(grepl("NOAA", DatasetID)) %>%
  group_by(DatasetID) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  group_by(Locality) %>%
  summarize(n=n())
View(x)

setwd("C:/rworking/deepseatools/indata")
sub <- read.csv("dsc_natdb.csv", header = TRUE)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

x <- filt %>%
  filter(FishCouncilRegion == "Caribbean" |
           FishCouncilRegion == "South Atlantic" |
           FishCouncilRegion == "Gulf of Mexico",
         as.Date(EntryDate) > "2019-10-01") %>%
  group_by(FishCouncilRegion, Vessel, VehicleName, EntryDate, ObservationYear) %>%
  summarize(n=n())
View(x)
length(x$CatalogNumber)

x <- filt %>%
  filter(grepl("NA", DatasetID)) %>%
  group_by(Vessel, DatasetID, DataProvider) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter(CategoricalAbundance == 'minimum count') %>%
  group_by(ScientificName, FlagReason, IndividualCount) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter() %>%
  group_by() %>%
  summarize(n=n())
View(x)

filt %>%
  filter(grepl("Deep Sea Coral", Repository)) %>%
  group_by(Repository, DatasetID) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(grepl("YOGI", VehicleName)) %>%
  group_by(DataProvider, VehicleName) %>%
  summarize(n=n()) %>% View()

x <- sub %>%
  filter(DepthInMeters < 50) %>%
  group_by(ScientificName, FlagReason, DepthInMeters, DepthMethod, ShallowFlag) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  group_by(CategoricalAbundance, IndividualCount) %>%
  summarize(n=n())
View(x)

x <- sub %>%
  filter(VernacularNameCategory == "nipple foliose sponge (yellow)") %>%
  group_by(Flag, FlagReason, ScientificName) %>%
  summarize(n=n())
View(x)

s %>% filter(FieldName == "Modified") %>% pull(ValidValues)
s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(ValidValues)
s %>% filter(FieldName == "IdentificationVerificationStatus") %>% pull(FieldDescription)
s %>% filter(FieldName == "TaxonRank") %>% pull(ValidValues)

##### checking #####
sub %>%
  filter(FlagReason == "Insufficient taxonomic information") %>%
  group_by(ScientificName, VernacularNameCategory, AphiaID, Phylum, Class, Order, Family, Genus, TaxonRank) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(IndividualCount, CategoricalAbundance) %>%
  summarize(n=n()) %>%
  View()

filt$DatasetURL <- paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/", filt$DatasetID, ".html", sep = "")
#head(filt$DatasetURL)

x <- "Yoklavich"
yo <- filt %>%
  filter(grepl(x, DataContact)|
           grepl(x, Reporter)|
           grepl(x, PI)|
           grepl(x, DatasetID) |
           grepl(x, Repository))

unique(yo$DatasetURL)


filt %>%
  filter(grepl("Southwest Fisheries", DataProvider)) %>%
  group_by(DatasetID, SurveyID, SamplingEquipment, DataContact, PI, Reporter) %>%
  summarize(n=n()) %>%
  View()

x <- filt %>%
  filter(grepl("Southwest Fisheries", DataProvider) |
           grepl("Northwest Fisheries", DataProvider))

sub %>%
  # filter(FlagReason == "Insufficient taxonomic resolution") %>%
  group_by(ObservationDate) %>%
  summarize(n=n()) %>%
  View()

sub %>%
  filter(grepl("Desmophyllum", ScientificName)) %>%
  group_by(ScientificName) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(grepl("140", SurveyID)) %>%
  group_by(Vessel, SurveyID, AccessionID, DatasetID, DataProvider, SamplingEquipment ) %>%
  summarize(n=n()) %>%
  View()

filt %>%
  filter(grepl("ftp:", WebSite)) %>%
  group_by(WebSite, Citation, DatasetID, SurveyID, ObservationDate) %>%
  summarize(n=n()) %>%
  View()

x <- s %>%
  filter(FieldName == 'LocationAccuracy') %>%
  pull(ValidValues) %>%
  View()

x <- s %>%
  filter(FieldName == 'OtherData') %>%
  pull(FieldDescription) %>%
  View()

##### mapit using leaflet #####
## optional create new 'sub' ##
sub2 <- sub
# sub2$Longitude <- as.numeric(sub$Longitude)-6
sub2 <- sub2 %>% filter(Latitude != -999 |
                          Longitude != -999)

# filter(CatalogNumber == "1178074")
m <- leaflet()
m <- addProviderTiles(m, "Esri.NatGeoWorldMap")
m <- addCircleMarkers(m, data=sub2,
                      radius=2,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=.5,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", sub2$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", sub2$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", sub2$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", sub2$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", sub2$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", sub2$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", sub2$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", sub2$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", sub2$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", sub2$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", sub2$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", sub2$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", sub2$EventID, "<br>",
                        "<b><em>","Latitude:","</b></em>", sub2$Latitude, "<br>",
                        "<b><em>","Longitude:","</b></em>", sub2$Longitude, "<br>",
                        "<b><em>","Image:","</b></em>",sub2$ImageURL))

m

##### export points and dive centers to GIS #####
# install.packages("arcgisbinding")
library(arcgisbinding)
arc.check_product()

## create x from sub
sub2 <- sub
sub2$Longitude <- as.numeric(sub$Longitude)
x <- sub2 # %>% filter(Temperature > 13)

## filter data
# get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 |
                    Longitude != -999)

# make copy to turn into spatial points data frame.
x_geo <- x

## create spatial points data frame
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class
fgdb_path <- 'C:/rworking/deepseatools/gis/gis.gdb'
arc.write(file.path(fgdb_path, 'x_geo2'), data=x_geo, overwrite = TRUE)

##### export dive points #####
## create summary by EventID
x <- sub %>% filter(Flag == 0,
                    Latitude != -999 |
                      Longitude != -999) %>%
  group_by(EventID) %>%
  summarise(Latitude = mean(Latitude),
            Longitude = mean(Longitude)
  )

## get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 |
                    Longitude != -999)

## make copy to turn into spatial points data frame.
x_geo <- x

## create spatial points data frame
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

## create feature-class

fgdb_path <- 'C:/rworking/gis/gis.gdb'
arc.write(file.path(fgdb_path, 'x_geo_dives'), data=x_geo, overwrite = TRUE)

##### checking #####f

filt %>% filter(grepl("AUV", VehicleName) | VehicleName == 'AUV') %>% pull(VehicleName) %>% unique()

filt %>% filter(grepl("SH1812", SurveyID)) %>% pull(DatasetID) %>% unique()
filt %>% filter(grepl("RL1905", SurveyID)) %>% pull(DatasetID) %>% unique()

yo <- filt %>% filter(grepl("Henry", Vessel)) %>%
  group_by(DatasetID, DataProvider, SamplingEquipment, Vessel, VehicleName) %>%
  summarize(n=n())
View(yo)

x <- filt %>%
  filter(grepl("NOAA_SWFSC_AST", DatasetID)) %>%
  #  ObservationYear == 2019) %>%
  group_by(DatasetID,
           PI,
           PIAffiliation,
           NavType,
           Vessel,
           ObservationYear,
           SurveyID,
           VehicleName,
           SamplingEquipment,
           DataContact,
           Reporter,
           ReporterEmail) %>%
  summarize(n=n())
View(x)

write.csv(x, "c:/rworking/deepseatools/indata/20221027_pacific_cruises_post_2016_RPMcGuinn.csv")

sub %>%
  group_by(SampleID, EventID) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(grepl("West Coast", FishCouncilRegion)) %>%
  group_by(Vessel,IdentificationQualifier, IdentificationVerificationStatus, DataProvider, RecordType, VehicleName, SamplingEquipment, Repository, PI, Reporter, ReporterEmail) %>%
  summarize(n=n()) %>% View()

filt %>%
  filter(grepl("Alaska", DataProvider)) %>%
  group_by(DatasetID, Vessel, SurveyID, ObservationYear) %>%  summarize(n=n()) %>% View()

sub %>%
  #filter(grepl("Alaska", DataProvider)) %>%
  group_by(SurveyID, EventID, Station, Locality, ObservationYear) %>%
  summarize(n=n()) %>% View()

sub %>%
  #filter(FlagReason == "Invalid latitude | Invalid longitude") %>%
  group_by(Flag, Latitude, Longitude, EndLatitude, StartLatitude, EndLongitude, StartLongitude) %>%
  summarize(n=n()) %>% View()


sub %>%
  filter(FlagReason == "Possible intersection with land") %>%
  group_by(Flag, Latitude, Longitude) %>%
  summarize(n=n()) %>% View()

##### check side by side #####
x <- filt %>%

  filter(grepl("Pacific", FishCouncilRegion)) %>%
  group_by(DatasetID,
           PIAffiliation,
           NavType,
           LocationAccuracy,
           Vessel,
           ObservationYear,
           EventID,
           SurveyID,
           IdentificationQualifier,
           IdentificationVerificationStatus,
           DataProvider,
           RecordType,
           VehicleName,
           SamplingEquipment,
           Repository,
           PI,
           Reporter,
           ReporterEmail) %>%
  summarize(n=n())

View(x)

y <- sub %>%
  # filter(grepl("AFSC", DatasetID)) %>%
  group_by(DatasetID,
           PIAffiliation,
           NavType,
           LocationAccuracy,
           Vessel,
           EventID,
           SurveyID,
           IdentificationQualifier,
           IdentificationVerificationStatus,
           DataProvider,
           RecordType,
           VehicleName,
           SamplingEquipment,
           Repository,
           PI,
           Reporter,
           ReporterEmail) %>%
  summarize(n=n())
View(x)
View(y)

##### check #####
filt %>%
  filter(grepl('1202', DatasetID)) %>%
  group_by(DatasetID, SurveyID) %>%
  summarize(n=n())

filt %>%








