##### Header #####
##### Author: Robert McGuinn #####
##### Started: 20200921

##### packages #####
library(rmarkdown)

##### render the QA dashboard #####
# add the prefix of the dataset you want to report on
x <- "20200701-0_NOAA_NEFSC_Connecticut_ISIS2_Towcam_Packer_2015_2015"

render("C:/rworking/deepseatools/code/rmd_accession_qa_dashboard.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')
