##### Header #####
##### Author: Robert McGuinn #####
##### Started: 20200921

##### packages #####
library(rmarkdown)

##### render the QA dashboard #####
# add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20200923-0_NOAA_OER_EX1502L3_Caribbean_Kennedy_resubmission_2015_2015"

rmarkdown::render("C:/rworking/deepseatools/code/20210303_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')
