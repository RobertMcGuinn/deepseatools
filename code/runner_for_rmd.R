##### Author: Robert McGuinn #####
##### Run a bunch of RMD reports on factor based groups of data #####

# see this reference: http://www.reed.edu/data-at-reed/software/R/markdown_multiple_reports.html
##### first subset the data that you want from the larger dataset #####
#install.packages("prettydoc")
table(factor(indata$DatasetID), useNA = "always")

library(prettydoc)
d <- indata %>%
  filter(Flag == "0", DatasetID == "NOAA_SH-10-11" |
           DatasetID == "Thoma_J_2013" |
           DatasetID == "SBMNH" |
           DatasetID == "OET_NA072" |
           DatasetID == "NOAA_EX-14-02-L3")
table(factor(d$DatasetID), useNA = "always")

d <- indata %>%
  filter(Flag == "0", is.na(DatasetID) == F)
table(factor(d$DatasetID), useNA = "always")

d <- indata

##### run RMD on each unique DatasetID group #####
# for HTML
library("rmarkdown")
for (id in unique(d$DatasetID)){
  sub <- d[d$DatasetID == id,]
  render("C:/rworking/digs/code/20171117-0_DatasetID_Dash_RPMcGuinn.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}

# for DOC
library("rmarkdown")
for (id in unique(d$DatasetID)){
  sub <- d[d$DatasetID == id,]
  render("C:/rworking/digs/code/20180524_0_accession_qa_dashboard_RPMcGuinn.rmd" ,
         output_file =  paste(id,".doc", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}

##### run RMD on each unique AccessionID group (for HTML) #####
d <- indata %>%
  filter(Flag == "0", DatasetID == "NOAA_SH-10-11" |
           DatasetID == "Thoma_J_2013" |
           DatasetID == "SBMNH" |
           DatasetID == "OET_NA072" |
           DatasetID == "NOAA_EX-14-02-L3")
table(factor(d$DatasetID), useNA = "always")


library("rmarkdown")

for (id in unique(d$AccessionID)){
  sub <- d[d$AccessionID == id,]
  render("C:/rworking/digs/code/20171104-0_accession_qa_dashboard_RPMcGuinn.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}

##### run RMD on each unique AccessionID group (for DOC) #####
# filter most recent data

d <- indata %>%
  filter(
    grepl('Battista', AccessionID)
  ) %>%
  group_by(AccessionID) %>%
  summarize(#DataProvider = toString(unique(Repository)),
    DatasetID = toString(unique(DatasetID)),
    Repository = toString(unique(Repository)),
    Vessel = toString(unique(Vessel)),
    PI = toString(unique(PI)),
    SampleID = toString(unique(SampleID)),
    ObservationDate = toString(unique(ObservationDate)),
    ScientificName = toString(unique(ScientificName)),
    ImageFilePath = toString(unique(ImageFilePath)),
    Flag = toString(unique(Flag)),
    Records = n())

View(d)

table(d$EntryDate, useNA = 'always')
table(factor(d$AccessionID), useNA = "always")

library(rmarkdown)
for (id in unique(d$AccessionID)){
  sub <- d[d$AccessionID == id,]
  render("C:/rworking/digs/code/20180209_0_accession_qa_dashboard_RPMcGuinn.rmd" ,
         output_file =  paste(id,".doc", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}

##### Status Update _figs_tables_only #####

x <- "rmd_figs_tables_only"
render("C:/rworking/deepseatools/code/rmd_figs_tables_only.rmd",
         output_file =  paste(x,".doc", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report')

##### Status Update (all) #####

x <- "rmd_status_update_figs_tables"
render("C:/rworking/deepseatools/code/rmd_status_update_figs_tables.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report')

##### Status Update (inline) #####

x <- "rmd_status_update_inline"
render("C:/rworking/deepseatools/code/rmd_status_update_inline.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports/2019_status_update_report/inline')


##### checker #####
x <- indata %>%
  filter(grepl("468173", CatalogNumber)) %>%
  group_by(DataProvider, DatasetID, ScientificName, Phylum, AphiaID, Flag, FlagReason, Purpose) %>%
  summarise(n=n())
View(x)

table(x$FlagReason)
table(unique(x$ScientificName))
table(factor(d$DataProvider))

setwd("C:/rworking/digs/outdata")
write.csv(x,"x.csv", row.names = F, quote = T)

length(unique(d2$EventID))
length(unique(d$EventID))
setdiff(unique(d$EventID),unique(d2$EventID))

##### render to google drive #####
library(gdoc)
setwd("C:/rworking/deepseatools/code")
rmarkdown::render('2019_ISDSC7_high_density_THourigan_RPMcGuinn.Rmd', output_format=gdoc())

##### render to html #####

x <- "golden_crab"
render("C:/rworking/digs/code/golden_crab.rmd",
       output_file =  paste(x,".html", sep=''),
       output_dir = 'C:/rworking/digs/reports')

##### render to doc #####

x <- "2019_ISDSC7_high_density_THourigan_RPMcGuinn"
setwd("C:/rworking/deepseatools/code")
render("2019_ISDSC7_high_density_THourigan_RPMcGuinn.Rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

##### render the QA dashboard (non-museum) #####
# install.packages("rmarkdown")
library(rmarkdown)
# add the prefix of the dataset you want to report on
x <- "20191111-0_University_of_Hawaii_Smith_Durden_Kilo_Moana_KM1808_2018_2018"

render("C:/rworking/deepseatools/code/rmd_accession_qa_dashboard.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

##### load in subset alone without running QA dash #####

x <- "20191112-1_HBOI_Walton_Smith_Cuba_Reed_Farrington_2017_2017"
setwd("C:/rworking/deepseatools/indata")
sub <- read.csv(paste(x,'.csv', sep = ''), header = T)

##### render the QA dashboard (museum)  #####
library(rmarkdown)
x <- "20180705-1_NOAA_OER_EX1606_Kelley_2016_2016"

render("C:/rworking/digs/code/20180524_0_accession_qa_dashboard_RPMcGuinn.rmd",
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/digs/reports')

##### species pages #####
d <- indata
rm(d)

sub <- indata %>%
  filter(Flag == "0", FishCouncilRegion == 'Caribbean')

library(rmarkdown)
for (id in unique(d$ScientificName)){
  sub <- d[d$ScientificName == id,]
  render("C:/rworking/digs/code/20171214_Status_Update" ,
         output_file =  paste(x,".doc", sep=''),
         output_dir = 'C:/rworking/digs/reports')
}

##### MBARI report #####
library(rmarkdown)
library(knitr)

setwd("C:\\rworking\\digs\\code")
x <- "20181008_0_west_coast_MBARI_stats_db_version_20181005_0_RPMcGuinn"
render("C:/rworking/digs/code/20181008_0_west_coast_MBARI_stats_db_version_20181005_0_RPMcGuinn.Rmd" ,
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/digs/reports')

##### Golden Crab Report #####
library(rmarkdown)
library(knitr)

setwd("C:\\rworking\\digs\\code")
x <- "20181019_0_golden_crab_report_RPMcGuinn"
render("C:/rworking/digs/code/golden_crab.Rmd" ,
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/digs/reports')

##### render the quarterly database update report #####
library(rmarkdown)
library(knitr)

setwd("C:\\rworking\\deepseatools\\code")
x <- "20190920-0_Quarterly_Report_for_Database_Update_RPMcGuinn"
render("C:/rworking/deepseatools/code/rmd_quarterly_report_for_database_update.Rmd" ,
       output_file =  paste(x,".doc", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')
