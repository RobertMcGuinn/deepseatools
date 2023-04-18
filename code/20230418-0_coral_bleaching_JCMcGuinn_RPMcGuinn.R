##### header #####
## author: Robert McGuinn
## started on: 20230418
## purpose: coral bleaching map layouts for JCM.
## google drive folder: https://drive.google.com/drive/folders/1E0Nnd6kYpmPHdA_eyn7DxURIm_y4aWse?usp=share_link
## ArcGIS project path: C:\Users\Robert.Mcguinn\Documents\ArcGIS\Projects\20230418-0_coral_bleaching_JCMcGuinn_RPMcGuinn\

##### packages #####
library(tidyverse)
library(sf)

##### load data #####
filename <-  '20230418-0_bleaching_data_subset_RMcGuinn.csv'
path <- 'c:/rworking/deepseatools/indata/'
sub <- read_csv(paste(path,filename,sep=''),
                na = c("", "NA", "no data"))

##### **check #####
hist(sub$Latitude)
hist(sub$Longitude)

sub %>% filter(Location == '1 mile east of Looe Key  SPA') %>% pull(Longitude)
sub %>% filter(grepl("Looe Key", Location))

##### change all values within Longitude to negative
sub <- sub %>%
  mutate(Longitude = ifelse(Longitude >= 0, -Longitude, Longitude))

##### make a correction to longitue #####
sub<-sub %>%
  mutate(Longitude =
  case_when(Location %in% c('1 mile east of Looe Key  SPA') ~ -81.38954,
            TRUE ~ Longitude))

##### create sf object #####
points <- st_as_sf(sub, coords = c("Longitude", "Latitude"), crs = 4326)

##### plot points #####
plot(points)

##### export shapefile #####
st_write(points, "C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230418-0_coral_bleaching_JCMcGuinn_RPMcGuinn/shapefiles/sub_geo.shp", delete_dsn = T)



