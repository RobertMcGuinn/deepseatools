##### parameters #####
date_received <- 20260730
received_from <- 'Ashley Marranzino <ashley.marranzino@noaa.gov>'
current_file_path <- rstudioapi::getSourceEditorContext()$path
code_dir <- dirname(current_file_path)
extracted_name <- sub(".*dst_data_(.*)\\.R$", "\\1", current_file_path)
issue_title <- extracted_name

redmine_project_id <- 'qa'
redmine_tracker_id <- 28
redmine_target_version_id <- 423

google_parent_url <- "https://drive.google.com/drive/folders/0B9c2c_XdhpFBfkFiQkxxZGlTeEEya3g3QkpsVTVqeGRPSjZqVEZDVkI0M1ZteU43eC1tN1E?resourcekey=0-KsNjeYbGwZNUg3L2_-wKIg"
redmine_api_key <- Sys.getenv("REDMINE_KEY")
redmine_url <- "https://vlab.noaa.gov/redmine/issues.json"

# Define the local file you want to upload and load
local_filename <- "NA135_ROV_Animal_Data_20260318.tsv"

# Automatically find the 'indata' folder one level up from your 'code' folder
indata_dir <- file.path(dirname(code_dir), "indata")
local_filepath <- file.path(indata_dir, local_filename)

##### packages #####
library(googledrive)
library(httr)
library(jsonlite)
library(readr)
library(readxl)

##### authenticate #####
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### create google drive folder & subfolders #####
# 1. Create the main parent folder
new_folder <- drive_mkdir(
  name = extracted_name,
  path = as_id(google_parent_url)
)
print(paste("Successfully created Google Drive folder. ID:", new_folder$id))

# 2. Create the 'original' and 'current' subfolders inside the new folder
folder_original <- drive_mkdir(
  name = "original",
  path = new_folder
)

folder_current <- drive_mkdir(
  name = "current",
  path = new_folder
)
print("Successfully created 'original' and 'current' subfolders.")
##### create redmine issue #####
##### create redmine issue #####
# 1. Construct the direct Google Drive URL using the new folder's ID
drive_url <- paste0("https://drive.google.com/drive/folders/", new_folder$id)

# 2. Format the description using Markdown: [Text](URL)
markdown_description <- paste0(
  "Date received: ", date_received, "\n",
  "Submitted by: ", received_from, "\n",
  "Drive Folder: ", drive_url)

# 3. Create the payload
issue_payload <- list(
  issue = list(
    project_id = redmine_project_id,
    tracker_id = redmine_tracker_id,
    fixed_version_id = redmine_target_version_id,
    subject = issue_title,
    description = markdown_description
  )
)

# POST = Create
response <- POST(
  url = redmine_url,
  add_headers("X-Redmine-API-Key" = redmine_api_key),
  body = issue_payload,
  encode = "json"
)

##### check redmine response #####
if (status_code(response) == 201) {
  new_issue <- content(response, "parsed")

  # Save the ID to its own variable for easier access HERE (on success)
  current_issue_id <- new_issue$issue$id

  cat("\nSUCCESS! Fully automated Redmine Task #", current_issue_id, "created in Stage 1.\n")
} else {
  print(paste("Failed to create Redmine issue. HTTP Status Code:", status_code(response)))

  # Suppress the encoding warning and print the raw text response
  print("Raw response body:")
  raw_error <- content(response, as = "text", encoding = "UTF-8")
  print(raw_error)
}

##### upload original data to google drive #####
# Check if the local file actually exists before trying to upload
if (!file.exists(local_filepath)) {
  stop(paste("Could not find the local file at:", local_filepath))
}

# Upload the file directly into the 'original' subfolder we created earlier
uploaded_file <- drive_upload(
  media = local_filepath,
  path = folder_original,
  name = local_filename
)
print(paste("Successfully uploaded", uploaded_file$name, "to Google Drive."))


##### load data for inspection #####
# Detect the file extension and load it into the 'sub' dataframe
if (grepl("\\.tsv$|\\.txt$", local_filename, ignore.case = TRUE)) {

  sub <- read_tsv(local_filepath)

} else if (grepl("\\.csv$", local_filename, ignore.case = TRUE)) {

  sub <- read_csv(local_filepath)

} else if (grepl("\\.xlsx?$|\\.xls$", local_filename, ignore.case = TRUE)) {

  sub <- read_excel(local_filepath)

} else {
  stop("Unsupported file extension. Please ensure the local file is a TSV, CSV, or Excel file.")
}

print(paste("Successfully loaded 'sub' into R with", nrow(sub), "rows and", ncol(sub), "columns."))

##### rename google drive folder & redmine issue #####
# manual inspection
table(sub$ObservationDate)
table(sub$Vessel)
table(sub$SurveyID)
filt %>% filter(grepl('OET', DatasetID)) %>% pull(DatasetID) %>% unique()

# Define the new name based on your manual inspection (NOTE: this must be monitored)
updated_name <- paste0("OET_",
                      unique(sub$SurveyID),
                      '_' ,
                      current_issue_id)

# 1. Rename the main Google Drive Folder
# We pass the 'new_folder' object we created earlier directly into drive_rename
renamed_folder <- drive_rename(
  file = new_folder,
  name = updated_name
)
message(sprintf("Successfully renamed Google Drive folder to: %s", renamed_folder$name))

# 2. Rename the Redmine Issue
# Construct the URL for the specific issue we just created
issue_update_url <- paste0("https://vlab.noaa.gov/redmine/issues/", new_issue$issue$id, ".json")

# Create the payload with just the field(s) we want to change
update_payload <- list(
  issue = list(
    subject = updated_name
  )
)

# Use PUT to update an existing resource in Redmine
update_response <- PUT(
  url = issue_update_url,
  add_headers("X-Redmine-API-Key" = redmine_api_key),
  body = update_payload,
  encode = "json"
)

##### add standard checklist items #####
# Define the checklist items you want to add
checklist_items <- c(
  "mcguinn: log dataset",
  "sallis: initial conformation",
  "sallis: download and host images if available",
  paste('sallis: set AccessionID and file name to: ', updated_name),
  paste('sallis: set DatasetID to: ', sub("_[^_]+$", "", updated_name))
)

# Construct the Checklists plugin endpoint URL
checklist_url <- paste0("https://vlab.noaa.gov/redmine/issues/", current_issue_id, "/checklists.json")

# Loop through and POST each checklist item to the issue
for (item_text in checklist_items) {

  item_payload <- list(
    checklist = list(
      subject = item_text,
      is_done = FALSE
    )
  )

  response <- POST(
    url = checklist_url,
    add_headers("X-Redmine-API-Key" = redmine_api_key),
    body = item_payload,
    encode = "json"
  )

  if (status_code(response) %in% c(200, 201)) {
    message(sprintf("Successfully added checklist item: '%s'", item_text))
  } else {
    warning(sprintf("Failed to add checklist item: '%s' (Status Code %s)", item_text, status_code(response)))
  }
}
##### open browser tabs #####
# 1. Construct the human-readable Redmine URL (no .json extension)
redmine_browser_url <- paste0("https://vlab.noaa.gov/redmine/issues/", current_issue_id)

# 2. Open the Redmine issue
browseURL(redmine_browser_url)

# 3. Pause for a second to let the browser catch up (prevents tab overwriting on some OSs)
Sys.sleep(1)

# 4. Open the Google Drive folder using the URL you created earlier
browseURL(drive_url)

message("Browser tabs opened for Redmine and Google Drive.")
