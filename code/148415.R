##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250826
## purpose: creating a crosswalk to get back the original

##### linkage #####
filename <- '148415' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.Rmd', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

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

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")









##### extract the SampleIDs from filt #####
x <- filt %>% filter(grepl('NOAA_PC2202L1_MDBC', DatasetID)) %>% pull(SampleID)

##### ***** NEW ORIGINAL from Tator (2025-06-22) ***** #####
## Tator report: 'PC2202L1_Forward Videos_2025-06-22_report_KZOWtUnqfT.xlsx'
##### load exports from tator #####
tatorexport <- 'PC2202L1_Forward Videos_2025-06-22_report_KZOWtUnqfT.xlsx'

corals <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'corals_inverts')

fish <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                  sheet = 'fish')

events <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                    sheet = 'events')

geology <- read.xlsx(paste('c:/rworking/deepseatools/indata/', tatorexport, sep = ''),
                     sheet = 'geology')

##### check #####
# names(corals)
#
# intersect(corals$TatorId, corals_test$TatorId)
##### load data from SQL files #####
## version 20250516

filename <- 'SAMPLEMETADATA.xlsx'
samplemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                            sheet = 'BiologicalSamples')

filename <- 'DIVEVEHICLEMETADATA.xlsx'
divevehiclemetadata <- read.xlsx(paste('c:/rworking/deepseatools/indata/mdbc/Season1_2021_2022/', filename, sep = ''),
                                 sheet = 'DiveMetadata')

##### check #####
# names(events)
# names(corals)
# names(geology)
# unique(divevehiclemetadata$CruiseID)
# intersect(names(corals), names(fish))
# setdiff(names(corals), names(fish))
# setdiff(names(fish), names(corals))
#
# x <- intersect(names(corals), names(events))
# summary(corals[,x])
#
# table(events$EventType, useNA = 'always')
# names(events)
# corals$MediaId
#
# corals %>% select(MediaName, MediaId) %>% View()
#
# corals %>% group_by(MediaName, MediaId) %>% summarize(n=n()) %>% View()
# names(corals)
# names(fish)
# names(events)
# table(events$EventType)

##### get VehicleName from divevehiclemetadata table #####
vehiclename <- divevehiclemetadata %>% filter(CruiseID == unique(corals$CruiseId)) %>% pull(VehicleName) %>% unique()
vessel <- divevehiclemetadata %>% filter(CruiseID == unique(corals$CruiseId)) %>% pull(CruiseName) %>% unique()

##### create a single fish and coral data frame #####
## fix the mis-matching classes between vectors so I can use bind_rows function
corals$OccurrenceComments <- as.character(corals$OccurrenceComments)
fish$IdentificationComments <- as.character(fish$IdentificationComments)
fish$IdentifiedBy <-  as.character(fish$IdentifiedBy)
fish$IdentificationDate <- as.character(fish$IdentificationDate)

## bind rows
coralsfish <- bind_rows(corals, fish)

##### filter: take out the unknown and null within ScientificName #####
coralsfish_cl <- coralsfish %>% filter(is.na(ScientificName) == F)
coralsfish_cl <- coralsfish %>% filter(!(grepl('Unknown', ScientificName) | grepl('unknown', ScientificName)))

##### check #####
# table(coralsfish_cl$ScientificName, useNA = 'always')
# coralsfish %>% filter(is.na(ScientificName) == T) %>% pull(ScientificName) %>% length()
# coralsfish %>% filter(grepl('Unknown', ScientificName) |
#                       grepl('unknown', ScientificName)) %>%
#   pull(ScientificName) %>% table()

# names(coralsfish_cl)

##### join the geology information by timestamp #####
## get the timestamps in a joinable format
coralsfish_cl$Timestamp_pos <- ymd_hms(coralsfish_cl$Timestamp)
geology$Timestamp_pos <- ymd_hms(geology$Timestamp)

## Specify the tolerance window in seconds (e.g., 600 seconds for 10 minutes)
tolerance_window <- 5

## Perform the fuzzy left join
result <- difference_left_join(coralsfish_cl, geology,
                               by = "Timestamp_pos",
                               max_dist = tolerance_window,
                               distance_col = "time_diff") %>%
  group_by(Timestamp_pos.x) %>%
  slice_min(order_by = time_diff, n = 1, with_ties = FALSE) %>% # `with_ties = FALSE` ensures only one row per Timestamp_pos.x
  ungroup()

coralsfish_cl$joiner_Timestamp_pos <- paste(coralsfish_cl$ScientificName, coralsfish_cl$Timestamp, sep = '_')
result$joiner_Timestamp_pos.x <- paste(result$ScientificName, result$Timestamp.x, sep = '_' )

result <-
  left_join(coralsfish_cl, result, by = c("joiner_Timestamp_pos" = "joiner_Timestamp_pos.x"))
##### join the General Locality information #####
locality_table <- divevehiclemetadata %>%
  filter(CruiseID == unique(corals$CruiseId)) %>%
  select(DiveID, GeneralLocation)
result <- merge(result, locality_table, by.x= 'DiveId', by.y = 'DiveID')

##### create the new ImageFilePath crosswalk #####
library(tools)
result$ImageFilePath <- paste(file_path_sans_ext(result$MediaName),
                              "_frame", result$Frame,'.png', sep = '')

crosswalk <- result %>% filter(TatorElementalId %in% x) %>%
  select(TatorElementalId, ImageFilePath) %>%
  rename(
    SampleID = TatorElementalId
  )

##### write crosswalk #####
write.csv(crosswalk, 'indata/20250826-1_crosswalk_for_ImageFilePath_RPMcGuinn.csv')

##### write crosswalk with annotation frames #####
crosswalk_frames <- result %>% filter(TatorElementalId %in% x) %>%
  select(TatorElementalId, ImageFilePath, BboxX.x, BboxY.x, BboxWidth.x, BboxHeight.x, Polygon.x) %>%
  rename(
    SampleID = TatorElementalId
  )

##### experiment: add all annotations to same image #####
# Load libraries
library(png)      # for PNG images
library(ggplot2)

# Path to your image
frame <- "PC2202L1_20220630T071218Z_FWD_ROV01_HD_frame120405.png"
img_path <- paste0("indata/annotation/",frame)

# Read the image
img <- readPNG(img_path)

# Get dimensions
img_height <- dim(img)[1]
img_width  <- dim(img)[2]

# Example bbox (replace with the row from your dataset)

bbox_x <- crosswalk_frames %>% filter(ImageFilePath == frame) %>% pull(BboxX.x)
bbox_y <- crosswalk_frames %>% filter(ImageFilePath == frame) %>% pull(BboxY.x)
bbox_w <- crosswalk_frames %>% filter(ImageFilePath == frame) %>% pull(BboxWidth.x)
bbox_h <- crosswalk_frames %>% filter(ImageFilePath == frame) %>% pull(BboxHeight.x)

# Convert to pixel coordinates
x_min <- bbox_x * img_width
y_min <- bbox_y * img_height
x_max <- (bbox_x + bbox_w) * img_width
y_max <- (bbox_y + bbox_h) * img_height

# Plot
g <- ggplot() +
  annotation_raster(img, xmin=0, xmax=img_width, ymin=0, ymax=img_height) +
  geom_rect(aes(xmin=x_min, xmax=x_max, ymin=img_height-y_max, ymax=img_height-y_min),
            color="red", fill=NA, size=1) +
  coord_fixed(ratio = 1, xlim=c(0,img_width), ylim=c(0,img_height)) +
  theme_void()

print(g)

##### experiment: add one annotation at a time and save the images #####
library(png)
library(ggplot2)
library(dplyr)
library(purrr)

plot_and_save_sample_bbox <- function(frame, crosswalk_frames, outdir = "plots") {
  # Create output directory if it doesn't exist
  if (!dir.exists(outdir)) dir.create(outdir)

  # Full image path
  img_path <- paste0("indata/annotation/", frame)
  img <- readPNG(img_path)
  img_height <- dim(img)[1]
  img_width  <- dim(img)[2]

  # Get all annotations for this frame
  annots <- crosswalk_frames %>%
    filter(ImageFilePath == frame)

  # Generate one plot per SampleID
  annots %>%
    split(.$SampleID) %>%
    imap(function(df, sid) {
      bbox_x <- df$BboxX.x
      bbox_y <- df$BboxY.x
      bbox_w <- df$BboxWidth.x
      bbox_h <- df$BboxHeight.x

      # Convert normalized to pixel coordinates
      x_min <- bbox_x * img_width
      y_min <- bbox_y * img_height
      x_max <- (bbox_x + bbox_w) * img_width
      y_max <- (bbox_y + bbox_h) * img_height

      # Build plot without title
      g <- ggplot() +
        annotation_raster(img, xmin = 0, xmax = img_width, ymin = 0, ymax = img_height) +
        geom_rect(aes(xmin = x_min, xmax = x_max,
                      ymin = img_height - y_max, ymax = img_height - y_min),
                  color = "red", fill = NA, size = 1) +
        coord_fixed(ratio = 1, xlim = c(0, img_width), ylim = c(0, img_height)) +
        theme_void()

      # Save the plot
      ggsave(
        filename = paste0(outdir, "/", sid, ".png"),
        plot = g,
        width = img_width/100,  # scale to reasonable size
        height = img_height/100,
        dpi = 100
      )

      return(NULL)
    })
}

# Example usage:
plot_and_save_sample_bbox(
  "PC2202L1_20220630T071218Z_FWD_ROV01_HD_frame120765.png",
  crosswalk_frames,
  outdir = "sample_plots"
)


##### check #####
id <- "fa886526-f086-42f9-a883-9fa647d57438"
filt %>% filter(SampleID == id) %>% pull(ScientificName)


