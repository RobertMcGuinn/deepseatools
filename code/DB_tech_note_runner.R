##### Header #####
# author: Robert McGuinn
# date started: 20191201
# purpose: run the RMarkdown for the database technical memo (status update)

##### install packages #####
source('C:/rworking/deepseatools/install_packages.R')

##### input: latest version of NDB #####
# setwd("C:/rworking/deepseatools/indata")
# indata<-read.csv("DSCRTP_NatDB_20191217-0.csv", header = T)
# filt <- indata %>%
#   filter(Flag == "0")

##### input: latest version of the NDB schema #####
# Register and download Google Sheet

# s <- gs_title('2019_DSCRTP_National_Database_Schema')# gs_browse(s)
#
# # s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
#
# s <- gs_read(s)
# names(s)

##### set version #####

version <- '20191217-0'

##### all #####

x <- "rmd_NDB_status_update"
render("C:/rworking/deepseatools/code/rmd_NDB_status_update.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report/',
       clean = FALSE)

##### pieces #####

x <- "yo"
render("C:/rworking/deepseatools/code/yo.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report/',
       clean = FALSE)



