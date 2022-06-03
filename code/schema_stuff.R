library(rmarkdown)
library(xfun)
library(tidyverse)
library(knitr)
library(flextable)
library(sp)
library(rgdal)
library(extrafont)
library(googlesheets4)
library(googledrive)
library(leaflet)
library(RColorBrewer)
library(terra)
library(sf)

library(googlesheets4)
s <- read_sheet('1YDskzxY8OF-34Q8aI04tZvlRbhGZqBSysuie39kYHoI')

setwd("C:/rworking/deepseatools/indata")
x <- read.csv("dsc_variables.csv", header = TRUE)
x$FieldName <- x$VariableName

x <- x %>%
  mutate(FieldName = str_replace(FieldName, "latitude", "Latitude")) %>%
  mutate(FieldName = str_replace(FieldName, "longitude", "Longitude"))

## checking
names(s)
names(x)
class(s$FieldName)
class(x$VariableName)
setdiff(x$FieldName, s$FieldName)

## merging
sx <- left_join(s,x, by = "FieldName")
#View(sx)

## write
setwd("C:/rworking/deepseatools/indata")
write_csv(sx, "dscrtp_schema_join_raw.csv")





