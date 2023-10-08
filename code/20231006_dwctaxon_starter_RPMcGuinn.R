##### Header #####
## author: Robert P. McGuinn
## date_started: 20231006
## forked from: https://docs.ropensci.org/dwctaxon/index.html
## purpose: to start figuring out the 'dwctaxon' package

##### packages #####
# install.packages("dwctaxon")
#
# options(repos = c(
#   ropensci = "https://ropensci.r-universe.dev/",
#   CRAN = "https://cran.rstudio.com/"
# ))
# install.packages("dwctaxon", dep = TRUE)

library(dwctaxon)

##### extract top of a taxonomic dataset #####
filmies_small <- head(dct_filmies, 5)

##### dct_add_row #####
filmies_small |>
  dct_add_row(
    scientificName = "Hymenophyllum dwctaxonense Nitta",
    taxonomicStatus = "accepted"
  )

##### dct_modify_row #####
filmies_small %>%
  dct_modify_row(
    scientificName = "Cephalomanes densinervium (Copel.) Copel.",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Cephalomanes crassum (Copel.) M. G. Price"
  )

##### dct_fill_col #####
filmies_small %>%
  dct_fill_col(
    fill_to = "acceptedNameUsage",
    fill_from = "scientificName",
    match_to = "taxonID",
    match_from = "acceptedNameUsageID"
  )


