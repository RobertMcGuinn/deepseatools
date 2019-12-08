##### Header #####
# author: Robert McGuinn
# date started: 20191201
# purpose: run the RMarkdown for the database technical memo (status update)

##### input: latest version of NDB #####
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20190920-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

##### input: latest version of the NDB schmea #####
# Register and download Google Sheet
s <- gs_title('2019_DSCRTP_National_Database_Schema')
# gs_browse(s)
# s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
s <- gs_read(s)
names(s)

##### all #####

x <- "rmd_NDB_status_update"
render("C:/rworking/deepseatools/code/rmd_NDB_status_update.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report/')

##### figs_tables_only #####

# x <- "rmd_figs_tables_only"
# render("C:/rworking/deepseatools/code/rmd_figs_tables_only.rmd",
#        output_file =  paste(x,".doc", sep=''),
#        output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report/figures')

##### (inline) #####

# x <- "rmd_status_update_inline"
# render("C:/rworking/deepseatools/code/rmd_status_update_inline.rmd",
#        output_file =  paste(x,".doc", sep=''),
#        output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report/inline')
#




