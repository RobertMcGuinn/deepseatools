##### Header #####
##### Author: Robert McGuinn #####
##### Started: 20200921

##### packages #####
library(rmarkdown)

##### render the QA dashboard #####
# add the 'AccessionID' of the data set you want to report on as 'x'
x <- "20200923-0_NOAA_OER_EX1502L3_Caribbean_Kennedy_resubmission_2015_2015"

render("C:/rworking/deepseatools/code/rmd_accession_qa_dashboard.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')
