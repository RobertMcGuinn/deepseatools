# Code Guide
This guide describes the R scripts within the [`code/`](code/) directory of this repo. 

# Style notes for scripts
- Comments describing code function are marked by '## '
- Commented-out code is marked by '# '
- Sections are marked by '##### section #####'
- Sometimes you will find hard coded local paths like 'c:/rworking/...'. You should change these manually. These will be corrected to relative paths in future iterations of this code-base.  
- The text 'manual' anywhere in the code indicates a place where specific text needs input.  These should be checked over using 'crtl-F' before using the code.
- Sections marked '##### check #####' are for testing and checking script progress and interim outputs. 
- File version notes: You will see file names that have a prefix with date-based version in the format `YYYYMMDD-X_` where the first part is the version/date and the 'X' is the iteration on that day. 
- Scripts with simple integer names (for example: 148433.R) are linked to a VLab Redmine issue in the [Redmine data QA pipeline](https://vlab.noaa.gov/redmine/projects/qa/issues?query_id=685) that further details the purpose of the script and other actions taken on the data source. The file name is the actual Redmine issue number in the pipeline.


## Usage
The R code stored and described here is not fully modular and is (unfortunately) meant to be ran interactively. You should not blindly run scripts using `source()`. It is better to run each section of the each script and then check results thoroughly before moving forward. There is an ongoing effort is to re-factor this code-base into a proper modular architecture, but for now it is more interactive.

## Description of each script

- ### [load_current_ndb.R](../code/load_current_ndb.R)  
  - **Purpose:** Load the current National Database for Deep Sea Corals and Sponges (NDB) CSV file and filter out flagged records.  
  - **Input:** 1. Local CSV file of the current NDB. The file should be named with the following format:`DSCRTP_NatDB_YYYYMMDD-X.csv`.  The current National Database is located at within the project's shared Google Drive [at this location](https://drive.google.com/drive/folders/1KPK1YI-n7EHNuOIKfZJM_EsaDCAUQOl8?usp=drive_link).  
  - **Output:** `filt` — filtered dataset containing only valid, unflagged records.
<br><br>
- ### [release_to_obis.R](../code/release_to_obis.R)  
  - **Purpose:** Prepare and transform the National Database into a minimally viable Darwin Core compliant dataset for submission to the Ocean Biogeographic Information System (OBIS) and the Global Biodiversity Information Facility (GBIF) through the [GBIF Integrated Publishing Toolkit](https://ipt-obis.gbif.us/) (IPT).  
  - **Input:** 1. A filtered version of the National Database. Use `load_current_ndb.R`
  - **Output:** Darwin Core compliant occurrence dataset.
<br><br>
- ### [runner_datasetID_dashboard.R](../code/runner_datasetID_dashboard.R)  
  - **Purpose:** Runner file for generating public facing dashboards for each 'DatasetID'. This script orchestrates the running of several RMarkdown files for dashboard generation
  - **Input:** 1. A filtered version of the national database. Use `load_current_ndb.R`. 2. The current DatasetID Key that can be [found at this location in the Google Shared Drive](https://drive.google.com/drive/folders/1e851ZIEpDgYNmnnYwHQQZ9RyuNyz1aWf?usp=drive_link)
  - **Output:** A single dashboard for each DatasetID for publishing in the WAF.  
<br><br>
- ### [dst_report_for_database_update.Rmd](../code/dst_report_for_database_update.Rmd)  
  - **Purpose:** Creating RMD report for announcing the update to the National database. This file is rendered and parameterized via a 'runner' file called `202501001-0_runner_dst_report_for_database_update.R`. The date prefix of this runner file will change
  - **Input:** 1.A filtered National Database. Use `load_current_ndb.R` 2. A filtered version of the previous version of the National Database. These can be found at [this location](https://drive.google.com/drive/folders/1KPK1YI-n7EHNuOIKfZJM_EsaDCAUQOl8?usp=drive_link) with the previous version being found in the 'archive' folder there.
  - **Output:** RMD report for announcing the update to the National database as a Word document.
<br><br>
- ### [20251001-0_runner_dst_report_for_database_update.R](../code/202501001-0_runner_dst_report_for_database_update.R)  
  - **Purpose:** The runner file to render `dst_report_for_database_update.RMD` for database version 20251001-0. 
  - **Input:** 1.A filtered National Database. Use `load_current_ndb.R` 2. A filtered version of the previous version of the National Database. These can be found at [this location](https://drive.google.com/drive/folders/1KPK1YI-n7EHNuOIKfZJM_EsaDCAUQOl8?usp=drive_link) with the previous version being found in the 'archive' folder there.
  - **Output:** RMD report for announcing the update to the National database as a Word document.
<br><br>






