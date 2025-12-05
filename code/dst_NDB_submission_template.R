##### Header #####
## Author: Robert McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## Start date: 20220718
## purpose: generate a submission template from the data dictionary
## warning: BE CAREFUL, this will replace the exising template.
##          You will need to manually redo the formatting in Google Sheet.

##### packages #####
library(tidyverse)
library(googledrive)
library(googlesheets4)

##### authenticate #####
## authenticate using token.
gs4_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")
drive_auth(cache = ".secrets", email = "robert.mcguinn@noaa.gov")

##### check: take a look at schema: browse to schema (manual: must put in correct ID) #####
## Google Drive location: https://drive.google.com/drive/folders/0B8lqJ4X0l6pTajJsa2t6bWRNRWc?resourcekey=0-QVD6rdiSnKeGQ4r7MSvwjQ&usp=drive_link
## see 'current' folder and 'archive' folder within that for earlier versions.
## manual: now is the time to edit the sehema as needed and give the file a new version number
# id <- '1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw'
# gs4_browse(id)

##### load schema from Google Drive(manual: must put in correct ID) #####
id <- '1jZa-b18cWxCVwnKsQcREPdaRQXTzLszBrtWEciViDFw'
s <- read_sheet(id)

##### load current submission template from Google Drive #####
## taking just the instructions and datatypes tab. We build the other 2 tabs from the schema
## manual: if you need to edit the "instructions" or "data types" tab then do it now in Google Drive
## Your Google Sheet ID
sheet_id <- "1xSfnbTRT4Lcrbbx-1_TPuvCqS4AVQEowVbLYYvoFM_s"

# Read instructions tab
instructions_df <- read_sheet(
  ss = sheet_id,
  sheet = "instructions"
)

# colnames(instructions_df) <- rep("", ncol(instructions_df))


# Read observations tab
data_types_df <- read_sheet(
  ss = sheet_id,
  sheet = "data types"
)

##### check #####
# View(data_types_df)
# View(instructions_df)
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(FieldDescription)
# s %>% filter(FieldName == 'IdentificationVerificationStatus') %>% pull(ValidValues)
# s$DataSubmissionTemplate
# names(s)
# names(x)
# class(s$FieldName)
# class(x$VariableName)
# setdiff(x$FieldName, s$FieldName)
#
# x <- s %>%
#   mutate(FieldName = str_replace(FieldName, "latitude", "Latitude")) %>%
#   mutate(FieldName = str_replace(FieldName, "longitude", "Longitude"))

##### create observations data frame #####
## pick out variables needed for the observations tab
x <- s %>%
  dplyr::select(PointHist,
                PointNew,
                PointProgram,
                TransHist,
                TransNew,
                TransProgram,
                TrawlHist,
                TrawlNew,
                TrawlProgram,
                FieldOrder,
                FieldDescription,
                ValidValues,
                FieldName
  )

y <- x %>% filter(PointHist == "R" |
                    PointNew == "R" |
                    PointProgram == "R" |
                    TransHist == "R" |
                    TransNew == "R" |
                    TransProgram== "R" |
                    TrawlHist== "R" |
                    TrawlNew == "R" |
                    TrawlProgram == "R" |
                    PointHist == "D" |
                    PointNew == "D" |
                    PointProgram == "D" |
                    TransHist == "D" |
                    TransNew == "D" |
                    TransProgram== "D" |
                    TrawlHist== "D" |
                    TrawlNew == "D" |
                    TrawlProgram == "D"
)

##### transpose the observations data frame #####
z <- y %>% t()
z <- as.data.frame(z, row.names = T)

## Set the row names to the original column names of y
rownames(z) <- colnames(y)
z <- cbind(Variable = rownames(z), z)
colnames(z) <- NULL

##### check #####
# View(z)

##### create metadata tab for submissions template #####
## pick out variables needed for the metadata tab
x <- s %>%
  dplyr::select(FieldName,
                FieldDescription,
                ValidValues
  )

## make list of FieldNames for metadata tab
fieldlist <- c(
  "DataProvider",
  "DataContact",
  "Citation",
  "Repository",
  "Modified",
  "Reporter",
  "ReporterComments",
  "SurveyID",
  "Vessel",
  "VehicleName",
  "PI",
  "PIAffiliation",
  "SamplingEquipment",
  "DepthMethod",
  "NavType",
  "LocationAccuracy",
  "Purpose",
  "SurveyComments",
  "RecordType",
  "IdentifiedBy",
  "IdentificationQualifier",
  "IdentificationDate",
  "IdentificationComments")

## filter to the fieldlist above
y <- x %>% filter(FieldName %in% fieldlist)
## View(y)

## add space for data provider entry
y$Data_Provider_Add_Entries_Below <- ""
## View(y)

## fix the order the
y2 <- left_join(data.frame(FieldName = fieldlist),
                y,
                by = "FieldName")
## View(y2)

##### -OR- write instructions, data types, metadata, observations to a workbook #####
library(openxlsx)

# Ensure observations z is properly formatted
# Transpose y and set row names as the first column
# Ensure all columns are character
z[] <- lapply(z, as.character)

# Create workbook
wb <- createWorkbook()

# Add worksheets
addWorksheet(wb, "instructions")
addWorksheet(wb, "data types")
addWorksheet(wb, "metadata")
addWorksheet(wb, "observations")

# Write data (no styling, no row names)
writeData(wb, sheet = "instructions", x = instructions_df)
writeData(wb, sheet = "data types", x = data_types_df)
writeData(wb, sheet = "metadata", x = y2)
writeData(wb, sheet = "observations", x = z)

# Save workbook
saveWorkbook(wb, "indata/20251203-0_dscrtp_submission_template.xlsx", overwrite = TRUE)


##### -OR- write to google sheets #####
write_submission_template_to_gsheet <- function(
    sheet_id,
    instructions_df,
    data_types_df,
    metadata_df,
    observations_df
){
  library(googlesheets4)
  library(dplyr)

  # --- Ensure observations is a data frame ---
  if (!is.data.frame(observations_df)) {
    observations_df <- as.data.frame(observations_df, stringsAsFactors = FALSE)
  }

  # --- Names of output tabs ---
  tabs <- list(
    "instructions" = instructions_df,
    "data types"   = data_types_df,
    "metadata"     = metadata_df,
    "observations" = observations_df
  )

  # --- Loop through tabs and write each ---
  for (tab in names(tabs)) {

    message(paste("Writing tab:", tab))

    # create sheet if it does not exist
    existing_sheets <- sheet_properties(ss = sheet_id)$name

    if (!(tab %in% existing_sheets)) {
      sheet_add(ss = sheet_id, sheet = tab)
    }

    # clear existing data
    range_clear(ss = sheet_id, sheet = tab)

    # write new data
    sheet_write(
      data = tabs[[tab]],
      ss = sheet_id,
      sheet = tab
    )
  }

  message("✔ All tabs written to Google Sheet successfully.")
}

write_submission_template_to_gsheet(sheet_id,
                                    instructions_df,
                                    data_types_df,
                                    y2,
                                    z)


##### check #####
drive_browse(as_id(sheet_id))
