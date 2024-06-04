##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250521
## purpose: overlaying annotations from BIIGLE export onto original images

##### linkage #####
filename <- '130435' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
gitlab_path <- 'https://git.ncei.noaa.gov/robert.mcguinn/deepseatools/-/blob/master/code/'
gitlab_link <- paste(gitlab_path, filename, '.R', sep = '')
browseURL(gitlab_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(magick)
library(jsonlite)
library(openxlsx)
library(ggplot2)
library(grid)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### bring in the json file #####
setwd('C:/rworking/deepseatools/indata')
json_file <- '16235-pacific-islands_SGoode.json'
json_data <- fromJSON(json_file)

##### bring in the XLS file #####
setwd('C:/rworking/deepseatools/indata')
xlsx_file <- '16235_full_image_annotation_report.xlsx'
xlsx_data <- read.xlsx(xlsx_file, startRow = 2)

##### download the images of interest from the NCEI WAF #####
path <- 'C:/rworking/deepseatools/images/annotation_test'
dir.create(path)
setwd(path)

for(i in seq_along(images)){
  download.file(images[i],
                destfile = basename(images[i]),
                mode = "wb")
}

##### pick the image that you would like to look at #####
filename <- '535944.JPG'
image_path <- paste("C:/rworking/deepseatools/images/annotation_test/", filename, sep='')
image <- image_read(image_path)

info <- image_info(image)
image_width <- info$width
image_height <- info$height

##### fill in original dataframe in prep for grouping #####
data <- xlsx_data %>%
  mutate(image.filename = ifelse(image.filename == "", NA, image.filename)) %>%
  fill(image.filename)

data <- data %>%
  mutate(annotation.id = ifelse(annotation.id == "", NA, annotation.id)) %>%
  fill(annotation.id)


##### rename the column with the '/' in it #####
data <- data %>%
  rename(
    "x.radius" = "x/radius",
  )

##### group by 'image.filename' and 'annotation.id' and summarize min and max for x and y#####
result <- data %>%
  group_by(image.filename, annotation.id) %>%
  summarise(
    x_min = min(x.radius),
    x_max = max(x.radius),
    y_min = min(y),
    y_max = max(y)
  ) %>%
  ungroup()

##### filter out the image of interest #####
z <- result %>% filter(grepl(filename, image.filename))

rects <- data.frame(
  xmin = z$x_min,# Lower-left x
  ymin = z$y_min,# Lower-left y
  xmax = z$x_max,# Upper-right x
  ymax = z$y_max # Upper-right y
)

##### plot the image with the annotations #####
p <- ggplot() +
  annotation_raster(image, xmin = 0, xmax = image_width, ymin = 0, ymax = image_height) +
  geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "red", fill = NA, size = 1) +
  theme_void() +  # Remove axes for better image display
  coord_fixed(ratio = 1, xlim = c(0, image_width), ylim = c(0, image_height))  # Adjust limits based on image size

##### Display the plot
print(p)



