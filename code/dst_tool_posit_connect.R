##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20260806
## purpose: publish bookdown on PositConnect

##### parameters #####
##### linkage #####
current_file <- rstudioapi::getSourceEditorContext()$path
filename <- basename(current_file)
print(filename)
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)


##### packages #####
library(tidyverse)
library(connectapi)
library(bookdown)
library(rsconnect)

##### render book and publish to Posit Connect#####
# 1. Render your book locally (outputs to docs/)
bookdown::render_book("index.Rmd")

# 2. Connect to server
client <- connect()

# 3. Target the "docs" folder and declare index.html as the entry point
rsconnect::writeManifest(
  appDir = "docs",
  contentCategory = "site",
  appPrimaryDoc = "index.html"
)

# 4. Bundle ONLY the docs folder
bundle <- bundle_dir("docs")

# 5. Deploy to Posit Connect (updating your existing GUID)
deploy_obj <- deploy(
  client,
  bundle,
  guid = "82b540a6-3534-4520-a5e0-c8e218443a22",
  name = "deep-sea-code-book"
)

# 6. Stream build logs until finished
poll_task(deploy_obj)

# Get the URL directly (no $get_content() needed!)
live_url <- deploy_obj$get_url()

# Print it to your console
print(live_url)

# Open your live site in your browser
browseURL(live_url)
