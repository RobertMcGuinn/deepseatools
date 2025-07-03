##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250703
## purpose: data QA
## issuename: 145952

##### linkage #####
filename <- '145952' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(openxlsx)
library(googlesheets4)
library(googledrive)
library(fuzzyjoin)
library(lubridate)
library(worrms)
library(googlesheets4)
library(taxize)

##### authorizations #####
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### ***** #####

##### ***** ORIGINAL ***** #####
##### load database #####
source('C:/rworking/deepseatools/code/mod_load_current_ndb.R')

##### load schema from google drive #####
sheetid <- '1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw'
s <- read_sheet(sheetid)

##### check #####
# s %>%
#   group_by(DSCRTPGroup, FieldName) %>%
#   summarize(n=n()) %>% View()

##### ***** ORIGINAL '2025-Q3_New_Records_5-10-2025' *****  #####
fname <- '2025-Q3_New_Records_5-10-2025.xlsx'
sub <- read.xlsx(paste('c:/rworking/deepseatools/indata/', fname, sep = ''),
                  sheet = 'Sheet1')

