##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20260203-0
## purpose:look at wwhen records were added to the database.

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_analysis_west_coast_initiative_summary_20260203-0'

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
# redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
# issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
# redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(robis)

##### load NDB #####
source("C:/rworking/deepseatools/code/dst_tool_load_current_ndb.R")

##### summary #####
library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)

##### create histogram #####
pacific_data <- filt %>%
  filter(FishCouncilRegion == "Pacific") %>%
  mutate(
    EntryDate = as.Date(EntryDate),
    # Drop levels to ensure the legend only shows what's in the Pacific
    SamplingEquipment = fct_drop(as.factor(SamplingEquipment))
  ) %>%
  filter(!is.na(EntryDate), !is.na(SamplingEquipment))

ggplot(pacific_data, aes(x = EntryDate, fill = SamplingEquipment)) +
  geom_histogram(
    color = "white",
    linewidth = 0.1,
    bins = 45 # Fine-grained bars for better temporal detail
  ) +
  # Qualitative Scale: Using 'Set3' or 'Paired' for distinct categories
  # This avoids the "gradient" look and gives each gear its own 'personality'
  scale_fill_brewer(
    palette = "Paired",
    name = "Sampling Method",
    drop = TRUE
  ) +
  # Force X-axis to Year only
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Dates of entry in the Pacific Fishery Management Council",
    subtitle = "Database Version: 20260121-0",
    x = "Year of Entry",
    y = "Frequency of Records"

  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Cleaner look for year-only axis
    plot.title = element_text(face = "bold", color = "#1a1a1a"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  guides(fill = guide_legend(ncol = 3)) # Wraps legend for better fit








