library(rmarkdown)
filename <- "mod_ndb_aoi"
v <- '20250521-0'

render(paste0('code/',filename,'.rmd'),
       output_file = paste0(v,'_',filename,'.doc'),
       output_dir = "reports/")

