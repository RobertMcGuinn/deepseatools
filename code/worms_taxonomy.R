##### Header #####
# purpose: Pulling WoRMS taxonomic information into NOAA's National Database for Deep Sea Corals
# author: Robert P. McGuinn, rpm@alumni.duke.edu
# date started:
#

##### resources and references #####
# https://cran.r-project.org/web/packages/worms/worms.pdf

##### load the most current taxonomy from Google Sheets #####

taxfl <- gs_title('20190909-0_taxonomy_to_flag')
taxfl <- gs_read(taxfl)

taxch <- gs_title('20190909-0_taxonomy_to_change')
taxch <- gs_read(taxch)

tax <- gs_title('20190909-0_taxonomy')
tax <- gs_read(tax)

##### create the taxon names list #####
taxa <- as.character(tax$ScientificName[1:10])
#taxa <- c("Adelogorgia cf. phyllosclera", "Putamayo", "Adelogorgia", "Lophelia pertusa")

taxa <- as.character(setdiff(unique(sub1$ScientificName), tax$ScientificName))
taxa1 <- taxa[1:50]
taxa2 <- taxa[51:66]
##### match taxa #####

x <- wm_records_taxamatch(name = taxa1,
                          ids = TRUE,
                          verbose = TRUE,
                          marine_only = TRUE
                          #sleep_btw_chunks_in_sec = 0.2
                          )
z <- bind_rows(x, .id = "column_label")
View(z)

##### write taxon names #####
setwd("C:/rworking/digs/outdata")
write.csv(taxon_names1,"20181204_0_Taxonomy_Table_names1_version_20181130-0.csv", row.names = F, quote = T)













##### use taxize instead #####


uids <- get_uid(taxa)
