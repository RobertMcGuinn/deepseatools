##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: YYYYMMDD
## purpose:

##### parameters #####
##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(file_name)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- filename
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)


##### packages #####
library(tidyverse)

##### section #####







# Install packages if not already installed:
# install.packages(c("jsonlite", "dplyr", "tidyr"))

library(jsonlite)
library(dplyr)
library(tidyr)

# 1. Read the JSON file into R
raw_data <-  fromJSON("indata/download-worms.json", simplifyDataFrame = TRUE)

# 2. unnest bounding boxes into individual rows
localizations_df <- raw_data %>%
  unnest(boundingBoxes, names_sep = "_")

# 3. Review State Filtering & Quality Control
# Check how many annotations are VERIFIED vs REJECTED or UNKNOWN
review_summary <- localizations_df %>%
  count(reviewState, name = "annotation_count")

print(review_summary)

# Filter dataset to only include high-quality annotations for training
verified_df <- localizations_df %>%
  filter(reviewState == "VERIFIED")

# 4. Class Imbalance Analysis
# Identify the frequency of each fauna concept in the verified data
class_imbalance <- verified_df %>%
  count(concept, sort = TRUE, name = "frequency")

print(class_imbalance, n=100)

# 5. Bounding Box Spatial and Size Analytics
# Calculate aspect ratios and area to configure anchor boxes
bbox_analytics <- verified_df %>%
  mutate(
    bbox_area = width * height,
    aspect_ratio = width / height
  ) %>%
  summarise(
    mean_width = mean(width, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    median_aspect_ratio = median(aspect_ratio, na.rm = TRUE),
    min_area = min(bbox_area, na.rm = TRUE),
    max_area = max(bbox_area, na.rm = TRUE)
  )

print(bbox_analytics)
