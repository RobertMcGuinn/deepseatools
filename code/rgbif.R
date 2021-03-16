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

