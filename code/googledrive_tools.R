##### Header #####
# Author: Robert McGuinn
# Email: rpm@alumni.duke.edu
# Purpose: Interacting with Google Drive using R
##### installation/loading of packages #####
#install.packages('pandoc')
library(pandoc)
#install.packages("pacman")
library(pacman)
#pacman::p_load(captioner, bundesligR)
library(captioner, bundesligR)
#install.packages("beanplot")
library(beanplot)
#install.packages("stringr")
library(stringr)
#install.packages("knitr")
library(knitr)
#install.packages("tidyr")
library(tidyr)
#install.packages("sp")
library(sp)
#install.packages("maptools")
library(maptools)
#install.packages("maps")
library(maps)
#install.packages("reshape")
library(reshape)
#install.packages("reshape2")
library(reshape2)
#install.packages("psych")
library(psych)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("data.table")
library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("car")
library(car)
#install.packages("gdata")
library(gdata)
#install.packages("digest")
library(digest)
#install.packages("rgdal")
library(rgdal)
#install.packages("ggmap")
library(ggmap)
#install.packages("rerddap")
library(rerddap)
#install.packages("raster")
library(raster)
#install.packages("rworldxtra")
library(rworldxtra)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("xtable")
library(xtable)
library(taxize)
library(rgdal)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("extrafont")
library(extrafont)
library(RColorBrewer)
library(googlesheets)
library(googledrive)
library(googlesheets4)

##### download Google Sheet version of schema for use in R  documents #####
# Register and download Google Sheet
s <- gs_key('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')
schema<- gs_read(s)
#gs_browse(s)

# write out to CSV
setwd("C:/rworking/digs/indata")
write.csv(schema, "2018_DSCRTP_Schema.csv")

##### reading in sheet googlesheets4 #####
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

##### help and documentation for googlesheets. #####
##### connecting and listing
# list files in drive
x <- drive_find(n_max = 50)
View(x)

# list files in drive and browse to them
drive_find(n_max = 2) %>% drive_browse()

# as.id function just extracts that ID of the file(s)
x <- drive_find(n_max = 5)
y <- as_id(x)
class(y)
y

# getting an object from Google Drive by name
x <- drive_get("2020_DSCRTP_National_Database_Schema")
1
as_id(x)
x

# this folder on Google Drive also has a drive ID
# big folder
x <- drive_get("photos")
as_id(x)
#[1] "0B6wAjbIPoNo3Q2c5Y2F4eW1XQ2M"

# get the same ID from URL
as_id("https://docs.google.com/spreadsheets/d/1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI/edit#gid=450007262")
#[1] "0B6wAjbIPoNo3Q2c5Y2F4eW1XQ2M"]

# create the dribble object
x <- as_dribble(as_id("https://docs.google.com/spreadsheets/d/1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI/edit#gid=450007262"))
View(x)
as_id(x)

# create dribble by using name
x <- as_dribble("photos")
View(x)
as_id(x)

# specify dribble using by path
x <- as_dribble("photos/Portugal2015/")
View(x)
as_id(x)

# using google sheets
install.packages("googlesheets")
library(googlesheets)

#list your google sheets
(my_sheets <- gs_ls())

# copy a google sheets to your Google Drive
?gs_gap()
gs_gap() %>%
  gs_copy(to = "Gapminder")

# register your google sheet
gap <- gs_title("Gapminder")
#> Sheet successfully identified: "Gapminder"

# Need to access a sheet you do not own?
# Access it by key if you know it!
(GAP_KEY <- gs_gap_key())
#> [1] "1BzfL0kZUz1TsI5zxJF1WNF01IxvC67FbOJUiiGMZ_mQ"
third_party_gap <- GAP_KEY %>%
  gs_key()
#> Sheet successfully identified: "test-gs-gapminder"

# Need to access a sheet you do not own but you have a sharing link?
# Access it by URL!
(GAP_URL <- gs_gap_url())
#> [1] "https://docs.google.com/spreadsheets/d/1BzfL0kZUz1TsI5zxJF1WNF01IxvC67FbOJUiiGMZ_mQ/"
third_party_gap <- GAP_URL %>%
  gs_url()

# Worried that a spreadsheet's registration is out-of-date?
# Re-register it!
gap <- gap %>% gs_gs()
#> Sheet successfully identified: "Gapminder"

#gs_title(), gs_key(), gs_url(), and gs_gs() return a registered sheet as a googlesheet
#The utility function, extract_key_from_url(), helps you dig the key out of a browser URL. Registering via browser URL is fine, but registering by key is a better idea in the long-run.
#Use gs_browse() to visit the Sheet corresponding to a registered googlesheet in your browser. Optionally, you can specify the worksheet of interest.

gap %>% gs_browse()
gap %>% gs_browse(ws = 2)
gap %>% gs_browse(ws = "Europe")

# list the registration information
gap

# list the worksheet tabs
gs_ws_ls(gap)


#returns the worksheet as an R object
gs_read() returns the contents of a worksheet as a data frame.

oceania <- gap %>%
  gs_read(ws = "Oceania")

##### working with the package markdrive #####
#devtools::install_github("milesmcbain/markdrive")
library(markdrive)
markdrive::gdoc_checkout(filename = "markdrive")
getwd()

##### markdrive help ######
# gdoc_checkout(filename = "GOT") Will search your Google drive for Google docs with "GOT" in the name and prompt to download one. After download it will be converted to .md for editing. Let's say the file that was downloaded was my_GOT_theory.docx, my_GOT_theory.md will be created in the working dir.
#
# gdoc_push(filname = "GOT") Will push a markdown file matching the name pattern back to Google drive and update the source document. You could also supply a dribble output from gdoc_checkout. It updates the google doc via a html conversion along the way.
#
# gdoc_render(filename = "./test.Rmd") will render an .md or .Rmd file to html and push it to Google Drive as a google doc. This package includes an Rstudio addin that will do this for the currently active source tab.
#
# Checkout to get the markdown file. Push to "save" your edits to Google drive.

##### using gdoc to push to google drive #####
#devtools::install_github("ropenscilabs/gdoc")
library(gdoc)
setwd("C:/rworking/digs/code")
rmarkdown::render('20180806_0_Status_Update_RPMcGuinn.rmd', output_format=gdoc())

##### render to HTML and DOC to a direct location #####
#or just render to local (html and doc are controlled in YAML header)
setwd("C:/rworking/digs/code")
rmarkdown::render('20180806_0_Status_Update_RPMcGuinn.rmd')


