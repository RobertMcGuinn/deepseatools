##### Header #####
## Author: Robert McgGuinn, rpm@alumni.duke.edu
## Date started: 20210519
##### load packages #####
library(tidyverse)
library(knitr)
library(flextable)
library(sp)
library(rgdal)
library(maps)
library(extrafont)
library(googlesheets4)
library(leaflet)
library(RColorBrewer)
library(sf)

##### build url to query to Google sheet (DOES NOT WORK) #####
## see: https://webapps.stackexchange.com/questions/87729/dynamic-filter-view-by-uri-in-a-google-sheet
## need to work out oauth issues
word <- "Frost"
sheetid <- "1C1z3pMmMfWpxHH-nuRc9dv4r2WrAH-EWGzG4rSf_LYQ/edit#gid=1234896357"

paste("https://docs.google.com/spreadsheets/d/",
             sheetid,"/gviz/tq?tq=SELECT%20*%20where%20B%20contains%20%22",
             word,"%22", sep = '')


# /copy
# /export
# /edit
# /preview
# /create
# /pub?
#   /fm?id=
#   /tq?tqx=out:html
# &key=[ID]
# &gid=[#]
#   &single=[true|false]
#   &range=[CellAddress|CellAddress1:CellAddress2]
#   &embedded=[true|false]
#   &widget=[true|false]
#   &output=[html|txt|csv|pdf]
#   &gridlines=[false]
#   &rm=[minimal|embedded|full|demo|?]
#   &ui=2 (interface version)
#   &chrome=[false] (full screen mode)
#   &width=[width]
#   &height=[height]
#   &frameborder=[size of border]
#   &q=[Search Query]
#   &viewer?
#     &start=
#     &channel=
#     &ibd=
#     &client=
#     &fmcmd=12
#   &size=0
#   &fzr=[true]
#   &portrait=[false]
#   &fitw=[true]
#   &printtitle=[true]
#   &sheetnames=[true]
#   &pagenum=[CENTER]
#   &attachment=[true]
#   &alt=[rss]
#   &tq=[query params here]

## example: https://docs.google.com/spreadsheets/d/1yapaaaFn0mtJF0CMiIil4Y1uYCqS97jIWL4iBZMPAKo/gviz/tq?tq=SELECT%20*%20where%20B%20contains%20%22ettoday%22

##### merging with names #####
master <- read.csv("./indata/institutional_crosswalk_master_names.csv", header = T)
yo <- read.csv("./indata/contacts.csv", header = T)
yo2 <- yo %>% filter(Organization.Name != "")
yo2$join <- paste(yo2$Organization.Name, yo2$Department.or.Office.Name, sep = ': ')
merge <- merge(yo2, master, by.x = "join", by.y = "nonconforming", all.x = T)
select <- merge %>%
  dplyr::select(Contact_ID,join, conforming, link, Acronym) %>%
  arrange(Contact_ID)
View(select)

##### write #####
write.csv(select, "./indata/select.csv")

##### master #####
names(master)
master <- master %>% select(2:4)
names(master) <- c("Organization_Name", "Organization_Link", "Organization_Abbreviation")
names(master)
write.csv(master, "./indata/MDBC_org_names.csv")

