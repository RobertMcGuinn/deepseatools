##### Header #####
## author: robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## date_started: 20250507
## purpose: download metadata from WAF and put it in a folder

###### ***** download all metadata and put it in a folder ***** #####
##### packages #####
library(utils)  # for URLencode

###### create a list of datasetID's
z <- unique(filt$DatasetID)

# Create output directory if it doesn't exist
dir.create('C:/rworking/deepseatools/indata/metadata', recursive = TRUE, showWarnings = FALSE)
setwd("C:/rworking/deepseatools/indata/metadata")

# Function to sanitize filenames
sanitize_filename <- function(name) {
  gsub("[^A-Za-z0-9_\\-]", "_", name)  # replace spaces, commas, and special characters with underscores
}

# Loop to download XML files
for(i in z){
  encoded_id <- URLencode(i, reserved = TRUE)        # URL-safe DatasetID
  safe_filename <- sanitize_filename(i)              # File-safe name

  url <- paste0('https://www.ncei.noaa.gov/waf/dsc-data/metadata/', encoded_id, '.xml')
  dest <- paste0(safe_filename, '.xml')

  # Try downloading and handle failures gracefully
  tryCatch({
    download.file(url, destfile = dest, mode = "wb", method = "curl", extra = "-L -A 'Mozilla/5.0'")
    message("Downloaded: ", dest)
  }, error = function(e) {
    message("Failed to download: ", i, " (", e$message, ")")
  })
}

##### check #####
length(list.files("C:/rworking/deepseatools/indata/metadata"))
length(z)
