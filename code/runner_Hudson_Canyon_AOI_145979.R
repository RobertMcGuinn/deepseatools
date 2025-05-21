library(rmarkdown)
filename <- "Hudson_Canyon_AOI_145979"
v <- '20250512-0'

render(paste0('code/',v,'_',filename,'.rmd'),
       output_file = paste0(v,'_',filename,'.doc'),
       output_dir = "reports/")
