##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose:

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_tool_create_project_spreadsheet'

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(googlesheets4)
library(tibble)

##### authenticate #####
gs4_auth(email = 'robert.mcguinn@noaa.gov')

##### define the tables (tabs) #####
tables <- list(
  initiatives = tibble(
    initiative_id = character(), initiative_name = character(), region = character(),
    start_fy = numeric(), end_fy = numeric(), lead_poc = character(), total_allocation = numeric()
  ),
  projects_master = tibble(
    project_id = character(), initiative_id = character(), project_title = character(),
    fmc_region = character(), start_fy = numeric(), end_fy = numeric(),
    status = character(), project_abstract = character()
  ),
  people_index = tibble(
    person_id = character(), first_name = character(), last_name = character(),
    email = character(), affiliation = character(), person_type = character()
  ),
  project_personnel = tibble(
    project_id = character(), person_id = character(), role = character()
  ),
  project_funding = tibble(
    project_id = character(), fiscal_year = numeric(), amount = numeric(),
    funding_type = character()
  ),
  project_outcomes = tibble(
    project_id = character(), has_discovery = logical(), mapped_area_km2 = numeric(),
    records_added = numeric(), modeling_type = character(), has_tech_dev = logical(),
    report_status = character(), pub_count = numeric(), primary_pub_link = character(),
    data_doi = character(), action_type = character(), management_impact = character()
  ),
  expeditions_surveys = tibble(
    survey_id = character(), project_id = character(), vessel = character(),
    platform = character(), date_start = character(), date_end = character()
  ),
  data_status_tracker = tibble(
    project_id = character(), survey_id = character(), data_category = character(),
    delivery_status = character(), archive_location = character(), is_flagged = logical()
  )
)

##### create the data dictionary #####
dict_data <- tribble(
  ~table_name,          ~column_name,         ~data_type,   ~key_type,      ~description,
  # initiatives
  "initiatives",        "initiative_id",     "String",     "Primary Key",  "Unique ID for 3-year funding cycle",
  "initiatives",        "initiative_name",   "String",     "None",         "Full name of regional initiative",
  "initiatives",        "region",            "Factor",     "None",         "FMC Region (NE, MA, SA, etc.)",
  "initiatives",        "start_fy",          "Integer",    "None",         "Starting Fiscal Year of cycle",
  "initiatives",        "end_fy",            "Integer",    "None",         "Ending Fiscal Year of cycle",
  "initiatives",        "lead_poc",          "String",     "None",         "Overall Initiative Coordinator",
  "initiatives",        "total_allocation",  "Numeric",    "None",         "Total 3-year budget allocation",

  # projects_master
  "projects_master",    "project_id",        "String",     "Primary Key",  "Unique ID for individual funded project",
  "projects_master",    "initiative_id",     "String",     "Foreign Key",  "Link to 'initiatives' table",
  "projects_master",    "project_title",     "String",     "None",         "Official title of project",
  "projects_master",    "fmc_region",        "Factor",     "None",         "Primary management region",
  "projects_master",    "start_fy",          "Integer",    "None",         "Project start year",
  "projects_master",    "end_fy",            "Integer",    "None",         "Project end year",
  "projects_master",    "status",            "Factor",     "None",         "Active, Completed, or Legacy",
  "projects_master",    "project_abstract",  "String",     "None",         "Summary of research goals",

  # people_index
  "people_index",       "person_id",         "String",     "Primary Key",  "Unique handle (e.g., jsmith_noaa)",
  "people_index",       "first_name",        "String",     "None",         "First Name",
  "people_index",       "last_name",         "String",     "None",         "Last Name / Surname",
  "people_index",       "email",             "String",     "None",         "Contact email address",
  "people_index",       "affiliation",       "String",     "None",         "NOAA Office or University",
  "people_index",       "person_type",       "Factor",     "None",         "Federal, Academic, Contractor, etc.",

  # project_personnel
  "project_personnel",  "project_id",        "String",     "Foreign Key",  "Link to 'projects_master'",
  "project_personnel",  "person_id",         "String",     "Foreign Key",  "Link to 'people_index'",
  "project_personnel",  "role",              "Factor",     "None",         "PI, coPI, NOAA POC, Science Lead",

  # project_funding
  "project_funding",    "project_id",        "String",     "Foreign Key",  "Link to 'projects_master'",
  "project_funding",    "fiscal_year",       "Integer",    "None",         "Year of funding allocation",
  "project_funding",    "amount",            "Numeric",    "None",         "Dollar amount ($)",
  "project_funding",    "funding_type",      "Factor",     "None",         "Initiative, Small Project, Co-fund",

  # project_outcomes
  "project_outcomes",   "project_id",        "String",     "Foreign Key",  "Link to 'projects_master'",
  "project_outcomes",   "has_discovery",     "Logical",    "None",         "T/F: Species or range discovery",
  "project_outcomes",   "mapped_area_km2",   "Numeric",    "None",         "Total area mapped (0 for desktop projects)",
  "project_outcomes",   "records_added",     "Integer",    "None",         "Records added to National Database",
  "project_outcomes",   "modeling_type",     "Factor",     "None",         "SDM, Abundance, Risk, N/A",
  "project_outcomes",   "has_tech_dev",      "Logical",    "None",         "T/F: New technology/method created",
  "project_outcomes",   "report_status",     "Factor",     "None",         "Pending, Submitted, Published",
  "project_outcomes",   "pub_count",         "Integer",    "None",         "Number of peer-reviewed pubs",
  "project_outcomes",   "primary_pub_link",  "String",     "None",         "Link to main report or publication",
  "project_outcomes",   "data_doi",          "String",     "None",         "Persistent identifier for dataset",
  "project_outcomes",   "action_type",       "Factor",     "None",         "Management action (EFH, HAPC, etc.)",
  "project_outcomes",   "management_impact", "String",     "None",         "Notes on how data influenced management",

  # expeditions_surveys
  "expeditions_surveys","survey_id",         "String",     "Primary Key",  "Unique cruise/event ID (e.g., EX2401)",
  "expeditions_surveys","project_id",        "String",     "Foreign Key",  "Link to 'projects_master'",
  "expeditions_surveys","vessel",            "String",     "None",         "Ship name",
  "expeditions_surveys","platform",          "Factor",     "None",         "ROV, AUV, DropCam, TowCam",
  "expeditions_surveys","date_start",        "Date",       "None",         "Start of field work",
  "expeditions_surveys","date_end",          "Date",       "None",         "End of field work",

  # data_status_tracker (Option 2)
  "data_status_tracker","project_id",        "String",     "Foreign Key",  "MANDATORY: Link to 'projects_master'",
  "data_status_tracker","survey_id",         "String",     "Foreign Key",  "OPTIONAL: Link to 'expeditions_surveys'",
  "data_status_tracker","data_category",     "Factor",     "None",         "Video, Models, Bathymetry, etc.",
  "data_status_tracker","delivery_status",   "Factor",     "None",         "Expected, Received, Archived, N/A",
  "data_status_tracker","archive_location",  "String",     "None",         "Repository link or path",
  "data_status_tracker","is_flagged",        "Logical",    "None",         "T/F: Issue with data delivery/quality"
)

##### add the dictionary to the sheet list #####
tables$data_dictionary <- dict_data

# 5. Create the Google Sheet
ss <- gs4_create(
  name = "DSCRTP Project Inventory (Master)",
  sheets = tables
)

# 6. Browse to the new sheet
gs4_browse(ss)
