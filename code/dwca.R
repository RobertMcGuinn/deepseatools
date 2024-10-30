##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20241010
## purpose: looking at Darwin Core Archives

##### linkage #####
filename <- 'dwca' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)

##### ***** looking into DwC-A ***** #####
##### Read the tab-delimited file into R #####
file_path <- "C:/rworking/deepseatools/indata/dwca-noaa_dsc_rtp-v1.19/occurrence.txt"
# Use read.delim() to read the file
data <- read.delim(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

##### Read in the metadata and xml files #####
## Install xml2 if you haven't already (uncomment if needed)
## install.packages("xml2")

## Load the xml2 library
library(xml2)

## Define the file paths
meta_file_path <- "C:/rworking/deepseatools/indata/dwca-noaa_dsc_rtp-v1.19/meta.xml"
eml_file_path <- "C:/rworking/deepseatools/indata/dwca-noaa_dsc_rtp-v1.19/eml.xml"

## Read the XML files
meta_xml <- read_xml(meta_file_path)
eml_xml <- read_xml(eml_file_path)

## Inspect the loaded XML data
print(meta_xml)
print(eml_xml)

## To navigate or extract specific data, you can use functions like xml_find_all() or xml_find_first()
## Example: Find all nodes in the meta XML
meta_nodes <- xml_find_all(meta_xml, "//*")
eml_nodes <- xml_find_all(eml_xml, "//*")

## Inspect the first few nodes
head(meta_nodes)
head(eml_nodes)

##### check #####
# head(data)
# setdiff(names(obis), names(data))
# setdiff(names(data), names(obis))







##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### read in the data inventory sheet from google drive #####




