library(rmarkdown)
filename <- "Aleutian_aoi_AWeinnig"
v <- '20250530'

render(paste0('code/', v,'_',filename,'.rmd'),
       output_file = paste0(v,'_',filename,'.doc'),
       output_dir = "reports/")

