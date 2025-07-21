##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250717
## purpose: load the most current schema from Google Drive
## google_drive: https://drive.google.com/drive/folders/0B9c2c_XdhpFBNkk4a1Q0VXItNWc?resourcekey=0-zV4mC0G55OLIyMMKa53PGw

##### linkage #####
filename <- 'mod_load_schema' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)

# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)

##### authorizations #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### load schema from google drive (manual: change sheet ID) #####
sheetid <- '1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw'
s <- read_sheet(sheetid)

##### optional: browse the schema ####
# url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id)
# browseURL(url)

##### check #####
s %>% filter(FieldName == 'VernacularNameCategory') %>%
  pull(ValidValues)









