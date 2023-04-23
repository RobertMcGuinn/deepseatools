##### Header #####
## filename: 20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn. R
## author: Robert McGuinn
## date started: 20230404
## project log: https://docs.google.com/document/d/1dT_DYHmX2hfFfxQLCT8zUv0umIfd_MPTQX5iJ96whiE/edit?usp=sharing

##### packages #####
library(tidyverse)
library(openxlsx)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(vegan)
library(maditr)

##### load data #####
path <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianRecords-ForMap_THourigan.xlsx"
mapdata <- read.xlsx(path)

path2 <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianSurveysTaxonCategories_THourigan.xlsx"
com <- read.xlsx(path2)

##### check #####
dim(mapdata)
names(mapdata)
dim(com)
names(com)

##### filter lat and longs ######
## filter out -999
sub2 <- mapdata %>%
  filter(Latitude != '-999' , Longitude != '-999')

##### set projection of incoming data #####
projcrs <- 4326

##### set target CRS #####
domainCRS <- 6393

##### create sf object #####
geosub <- st_as_sf(x = sub2,
                   coords = c("Longitude", "Latitude"),
                   crs = projcrs)

geosub <- st_transform(geosub, crs = domainCRS)

##### create a polygon of the bounding box of the file #####
polybbox <- geosub  %>%
  st_bbox() %>%
  st_as_sfc()

polybbox_buffer <- st_buffer(polybbox, dist=200000) # map units are in meters
bbox <- st_bbox(polybbox_buffer)

##### get naturalearth basemap #####
world <- ne_countries(scale = "medium", returnclass = "sf") #, country = "United States of America"
world <- st_transform(world, crs = domainCRS)
world <- st_crop(world, bbox)

##### plot the quick map #####
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = geosub,
          color = "red",
          size = 1,
          shape = 15)

##### write shapefile #####
st_write(geosub,
         "C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/shapefiles/mapdata.shp",
         delete_dsn = T)

###### import protected area shapefile using sf #####
pa <- sf::st_read("C:/data/gis_data/protected_areas/shapefiles/20221104_protected_areas.shp")



##### check #####
com %>%
  group_by(EventID) %>%
  summarize(n=n()) %>% View()

com %>% pull(DensityCategory) %>% table(useNA = 'always')
com %>% pull(AnalysisCategory) %>% table(useNA = 'always')


dim(com)

com %>%
  group_by(DensityCategory, Temperature) %>%
  summarize(n=n()) %>% View()

com %>%
  group_by(EventID, Temperature) %>%
  summarize(n=n()) %>% View()

com %>% filter(is.na(Temperature) == T) %>% pull(EventID) %>% unique()

com %>% pull(Habitat) %>% unique()

##### create copy of com #####
x <- com

##### enhance with categories #####
x$DepthCat[as.numeric(x$DepthInMeters) <= 200] <- "shallow"
x$DepthCat[as.numeric(x$DepthInMeters) > 200] <- "deep"
x$DepthCat <- factor(x$DepthCat, levels = c("shallow", "deep"))

##### setting location categories #####
x$loc[as.numeric(x$Latitude) >= 52] <- "north"
x$loc[as.numeric(x$Latitude) < 52] <- "south"

##### setting temperature categories #####
x$temp_cat[as.numeric(x$Temperature) > 4.5] <- "warmest"
x$temp_cat[as.numeric(x$Temperature) >= 4.0 & as.numeric(x$Temperature) <= 4.5] <- "cold"
x$temp_cat[as.numeric(x$Temperature) < 4.0] <- "coldest"
x$temp_cat <- factor(x$temp_cat, levels = c("warmest", "cold", "coldest"))

##### filter something out #####
x <- x %>% filter(EventID != 'HAPC_43')
x <- x %>% filter(EventID != 'TG16_21')
x <- x %>% filter(EventID != 'TG16_22')
x <- x %>% filter(EventID != 'transect 2012-39')
x <- x %>% filter(EventID != 'transect 2014-56')
x <- x %>% filter(EventID != 'transect 2012-104')
x <- x %>% filter(AnalysisCategory != 'Ptilosarcus gurneyi')

##### add IndividualCount variables #####
x$IndividualCount <- 1

##### create site X species matrix #####
site.sp <- dcast(x, EventID ~ AnalysisCategory, value.var = "IndividualCount", fun.aggregate = sum)

##### Assign EventID to the row names #####
row.names(site.sp) <- site.sp$EventID
names <- site.sp$EventID

##### Remove the EventID variable #####
site.sp <- site.sp %>%
  dplyr::select(-EventID)

##### running NMDS starting with site X species matrix #####
NMDS <- metaMDS(site.sp, distance = "bray", binary = T, k=3, trymax = 30)

##### extracting the site and species scores for use with ggplot2 #####
site.scores <- as.data.frame(scores(NMDS, "site"))
site.scores$EventID <- names # create a column of site names, from the rownames of data.scores

##### joining information from original able
yo <- x %>% group_by(EventID, DepthCat, temp_cat, loc, DensityCategory) %>% summarize(n=n())
site.scores <- left_join(site.scores, yo)

##### extracting species scores #####
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species
species.scores$spec_code <- 1:(length(species.scores$species))

##### plotting the NMDS scores #####
ggplot() +
  geom_point(data=site.scores,aes(x=NMDS2,y=NMDS1, color= loc), size=5) + # add the point markers
  geom_text(data=species.scores,
            aes(x=NMDS2,y=NMDS1,
                label = spec_code),
            size=4,
            position = position_nudge(y = .0001)) +
  geom_point(data=species.scores,
             aes(x=NMDS2,y=NMDS1),
             shape = 3, size=4)

##### legend to species #####
species.scores %>%  dplyr::select(spec_code)


