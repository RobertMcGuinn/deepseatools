##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: YYYYMMDD
## purpose:

##### parameters #####
##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(jsonlite)
library(dplyr)
library(tidyr)

##### Read the JSON file from FathomNet into R #####
raw_data <-  fromJSON("indata/download-worms.json", simplifyDataFrame = TRUE)

##### unnest bounding boxes into individual rows #####
localizations_df <- raw_data %>%
  unnest(boundingBoxes, names_sep = "_")

##### extract and list the ground truth taxa for each image from the 'tags' array #####
ground_truth_df <- raw_data %>%
  select(image_url = url, image_uuid = uuid, tags) %>%
  unnest(tags, names_sep = "_") %>%
  filter(grepl("ground_truth_taxon", tags_key)) %>%
  group_by(image_uuid, image_url) %>%
  summarise(ground_truth_taxa = paste(tags_value, collapse = " | "), .groups = 'drop')

## Extract and list the unique bounding box concepts for each image
concepts_df <- raw_data %>%
  select(image_uuid = uuid, boundingBoxes) %>%
  unnest(boundingBoxes, names_sep = "_") %>%
  filter(!is.na(boundingBoxes_concept)) %>%
  group_by(image_uuid) %>%
  ## Use unique() to avoid listing the same concept multiple times per image
  summarise(image_concepts = paste(unique(boundingBoxes_concept), collapse = " | "), .groups = 'drop')

## Join the concepts onto the ground truth dataframe
combined_df <- ground_truth_df %>%
  left_join(concepts_df, by = "image_uuid")

print(head(combined_df))


