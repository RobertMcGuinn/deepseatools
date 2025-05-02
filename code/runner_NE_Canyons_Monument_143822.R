library(rmarkdown)

render('c:/rworking/deepseatools/code/20250502-0_NE_Canyons_Monument_143822.Rmd',
       output_file = paste0("report_", Sys.Date(), ".doc"),
       output_dir = "c:/rworking/deepseatools/reports")
