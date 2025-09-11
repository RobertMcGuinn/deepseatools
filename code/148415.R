##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250826
## purpose: creating a crosswalk to get back the original

##### linkage #####
filename <- '148415' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
## browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
## browseURL(redmine_link)

##### packages #####
library(fuzzyjoin)
library(tidyverse)
library(sf)
library(remotes)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(robis)
library(openxlsx)
library(googledrive)

##### source ndb #####
## source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")
##### extract the relevant SamplingIDs from the NDB #####
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
##### load data from SQLfiles #####
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

##### create crosswalk with annotation frames #####
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
  outdir = "indata/sample_plots"
)

##### check #####
id <- "d7e06c23-d15f-441b-bad7-6afc78cb700d"
filt %>% filter(SampleID == id) %>% select(ScientificName, VerbatimScientificName, AphiaID, IdentificationComments)

##### download images at a specific location #####
## drive_auth()
## Ensure local folder exists
dir.create("images/annotation", recursive = TRUE, showWarnings = FALSE)

# Get the folder
folder <- drive_get(as_id("1-WDCcUa2aSR8tgoxW6Ga3F0ld2cP2lss"))

# List contents and pull IDs
files <- drive_ls(folder)

for (i in seq_len(nrow(files))) {
  drive_download(
    as_id(files$id[i]),
    path = file.path("images/annotation", files$name[i]),
    overwrite = TRUE
  )
}

##### check: for transfer of all files #####
# Files listed in Google Drive
files <- drive_ls(folder)

n_drive <- nrow(files)

# Files actually downloaded locally
n_local <- length(list.files("images/annotation", full.names = TRUE))

# Compare
cat("Files on Drive:", n_drive, "\n")
cat("Files downloaded:", n_local, "\n")

if (n_drive == n_local) {
  message("✅ Counts match!")
} else {
  warning("⚠️ Counts do NOT match. Some files may be missing.")
}

##### create a function to plot annotations and save #####
plot_and_save_sample_bbox <- function(frame, crosswalk_frames, indir = "images/annotation", outdir = "plots") {
  # Create output directory if it doesn't exist
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  # Full image path
  img_path <- file.path(indir, frame)
  img <- png::readPNG(img_path)
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

      # Build plot
      g <- ggplot() +
        annotation_raster(img, xmin = 0, xmax = img_width, ymin = 0, ymax = img_height) +
        geom_rect(aes(xmin = x_min, xmax = x_max,
                      ymin = img_height - y_max, ymax = img_height - y_min),
                  color = "red", fill = NA, size = 1) +
        coord_fixed(ratio = 1, xlim = c(0, img_width), ylim = c(0, img_height)) +
        theme_void()

      # Save the plot
      ggsave(
        filename = file.path(outdir, paste0(sid, ".png")),
        plot = g,
        width = img_width/100,
        height = img_height/100,
        dpi = 100
      )

      return(NULL)
    })
}

##### apply to all files in the list #####
library(purrr)

# List downloaded files
all_files <- list.files("images/annotation", full.names = FALSE)
all_files <- all_files[1:(length(all_files) - 1)]

# Run the function for each file
walk(all_files, ~ plot_and_save_sample_bbox(.x, crosswalk_frames,
                                            indir = "images/annotation",
                                            outdir = "images/annotation/with_frames"))

##### stats about what was created #####
# Helper to summarize a folder
summarize_folder <- function(path) {
  files <- list.files(path, full.names = TRUE, recursive = FALSE)
  # Keep only files (drop directories)
  files <- files[!dir.exists(files)]

  n_files <- length(files)
  total_size_mb <- sum(file.info(files)$size, na.rm = TRUE) / (1024^2)

  list(n_files = n_files, total_size_mb = total_size_mb)
}

# Compare
annotation_stats <- summarize_folder("images/annotation")
with_frames_stats <- summarize_folder("images/annotation/with_frames")

cat("images/annotation:\n")
cat("  Number of files:", annotation_stats$n_files, "\n")
cat("  Total size (MB):", round(annotation_stats$total_size_mb, 2), "\n\n")

cat("images/annotation/with_frames:\n")
cat("  Number of files:", with_frames_stats$n_files, "\n")
cat("  Total size (MB):", round(with_frames_stats$total_size_mb, 2), "\n")

##### check #####
sampleid <- '01e98be2-16bf-4986-a18c-cc257b1d4163'
filt %>% filter(SampleID == sampleid) %>% pull(ScientificName)
