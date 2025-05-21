library(rmarkdown)
filename <- "NE_Canyons_Monument_143822"
v <- '20250502-0'

render(paste0('code/',v,'_',filename,'.rmd'),
       output_file = paste0(v,'_',filename,'.doc'),
       output_dir = "reports/")
