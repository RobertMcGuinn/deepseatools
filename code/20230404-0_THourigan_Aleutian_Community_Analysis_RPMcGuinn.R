##### Header #####
## filename: 20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn. R
## author: Robert McGuinn
## date started: 20230404

##### packages #####
library(tidyverse)
library(openxlsx)

##### load data #####
path <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianRecords-ForMap_THourigan.xlsx"
mapdata <- read.xlsx(path)

path2 <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianSurveysTaxonCategories_THourigan.xlsx"
com <- read.xlsx(path2)

##### check #####
dim(mapdata)
names(mapdata)
dim(com)
names(com)
