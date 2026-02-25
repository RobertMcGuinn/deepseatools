##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260216-0
## purpose: Matching NCCOS/DCEL things.

##### packages #####
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(purrr)

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_analysis_Ren_Salgado_NCCOS_data_20260216-0'
project_folder <- 'https://drive.google.com/drive/folders/1rpcWKPSvWaL8FG6kPfVv_kQQS13Ictoi?usp=drive_link'
sheetid <- '1wUevbqTyYudL5g3TC0sXJn1QiIMRLfFjDA6OhIv3Xbk'
sheetid <- '17rOm3PBC5gETwiWygxYS5ju05V4F6wOholoDnBvx7HY'

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')

##### authorization #####
drive_auth(email = 'robert.mcguinn@noaa.gov')

##### upload sheet #####
rens_file <- read_sheet(sheetid, sheet = 1)

##### perform the fuzzy matching match #####
## tunable parameters
method      <- "jw"   # "jw" (Jaro-Winkler) often good for names; also try "osa", "lv", "cosine"
max_dist_ds <- 0.15   # threshold for DatasetID matching (smaller = stricter)
max_dist_sv <- 0.15   # threshold for SurveyID matching
top_n       <- NA_integer_  # set (e.g., 5) to keep only the top-N closest per source; NA keeps all

## normalization
clean <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

## data prep
rens2 <- rens_file |>
  mutate(
    .rowid = dplyr::row_number(),
    CruiseName_clean = clean(CruiseName)
  )

filt2 <- filt |>
  mutate(
    DatasetID_clean = clean(DatasetID),
    SurveyID_clean  = clean(SurveyID)
  )

## fuzzy match DatasetID
m_ds_all <- stringdist_inner_join(
  x = rens2,
  y = filt2 |> select(DatasetID, DatasetID_clean),
  by = c("CruiseName_clean" = "DatasetID_clean"),
  method = method,
  max_dist = max_dist_ds,
  distance_col = "dist_ds"
)

if (!is.na(top_n)) {
  m_ds_all <- m_ds_all |>
    group_by(.rowid) |>
    slice_min(order_by = dist_ds, n = top_n, with_ties = FALSE) |>
    ungroup()
}

m_ds <- m_ds_all |>
  group_by(.rowid) |>
  summarise(
    datasetid_matches = list(unique(DatasetID)),
    datasetid_details = list(
      distinct(select(cur_data_all(), DatasetID, dist_ds)) |>
        arrange(dist_ds, DatasetID)
    ),
    .groups = "drop"
  )

## fuzzy match SurveyID
m_sv_all <- stringdist_inner_join(
  x = rens2,
  y = filt2 |> select(SurveyID, SurveyID_clean),
  by = c("CruiseName_clean" = "SurveyID_clean"),
  method = method,
  max_dist = max_dist_sv,
  distance_col = "dist_sv"
)

if (!is.na(top_n)) {
  m_sv_all <- m_sv_all |>
    group_by(.rowid) |>
    slice_min(order_by = dist_sv, n = top_n, with_ties = FALSE) |>
    ungroup()
}

m_sv <- m_sv_all |>
  group_by(.rowid) |>
  summarise(
    surveyid_matches = list(unique(SurveyID)),
    surveyid_details = list(
      distinct(select(cur_data_all(), SurveyID, dist_sv)) |>
        arrange(dist_sv, SurveyID)
    ),
    .groups = "drop"
  )

## combine basck to rens file
rens_with_two_lists <- rens2 |>
  left_join(m_ds, by = ".rowid") |>
  left_join(m_sv, by = ".rowid") |>
  tidyr::replace_na(list(
    datasetid_matches = list(character()),
    surveyid_matches  = list(character()),
    datasetid_details = list(tibble(DatasetID = character(), dist_ds = double())),
    surveyid_details  = list(tibble(SurveyID  = character(), dist_sv = double()))
  )) |>
  select(-CruiseName_clean)

# ---------------------------
# (Optional) A single labeled list-column combining both sources
# ---------------------------
# rens_with_two_lists <- rens_with_two_lists |>
#   mutate(
#     all_matches_labeled = purrr::map2(datasetid_details, surveyid_details, ~{
#       d1 <- tibble(source = "DatasetID", key = .x$DatasetID, distance = .x$dist_ds)
#       d2 <- tibble(source = "SurveyID",  key = .y$SurveyID,  distance = .y$dist_sv)
#       bind_rows(d1, d2) |>
#         filter(!is.na(key)) |>
#         arrange(distance, source, key)
#     })
#   )

# Final object:
# rens_with_two_lists


##### check #####







