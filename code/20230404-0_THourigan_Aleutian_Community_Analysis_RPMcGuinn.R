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
install.packages("ggord")

##### load data #####
path <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianRecords-ForMap_THourigan.xlsx"
mapdata <- read.xlsx(path)

path2 <- "C:/rworking/deepseatools/indata/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/20230404-0_AleutianSurveysTaxonCategories_THourigan.xlsx"
com <- read.xlsx(path2)

##### ** check #####
# dim(mapdata)
# names(mapdata)
#
# mapdata %>% pull(Habitat) %>% unique()
#
# mapdata %>% pull(Locality) %>% unique()
#
# dim(com)
# names(com)
#
# com %>% group_by(EventID) %>%
#   summarize(n=n()) %>% View()
#
# com %>%
#   group_by(EventID) %>%
#   summarize(n=n()) %>% View()
#
# com %>% pull(DensityCategory) %>% table(useNA = 'always')
#
# com %>% pull(AnalysisCategory) %>% table(useNA = 'always')
#
# dim(com)
#
# com %>%
#   group_by(DensityCategory, Temperature) %>%
#   summarize(n=n()) %>% View()
#
# com %>%
#   group_by(EventID, Temperature) %>%
#   summarize(n=n()) %>% View()
#
# com %>% filter(is.na(Temperature) == T) %>% pull(EventID) %>% unique()
#
# com %>% pull(Habitat) %>% unique()

##### create copy of com call it 'x' #####
x <- com

##### setting depth categories #####
x$DepthCat[as.numeric(x$DepthInMeters) <= 200] <- "shallow"
x$DepthCat[as.numeric(x$DepthInMeters) > 200] <- "deep"
x$DepthCat <- factor(x$DepthCat, levels = c("shallow", "deep"))

##### setting latitude categories #####
x$loc[as.numeric(x$Latitude) >= 52] <- "north"
x$loc[as.numeric(x$Latitude) < 52] <- "south"

##### setting temperature categories #####
x$temp_cat[as.numeric(x$Temperature) > 4.5] <- "warmest"
x$temp_cat[as.numeric(x$Temperature) >= 4.0 & as.numeric(x$Temperature) <= 4.5] <- "cold"
x$temp_cat[as.numeric(x$Temperature) < 4.0] <- "coldest"
x$temp_cat <- factor(x$temp_cat, levels = c("warmest", "cold", "coldest"))
## note: look at trawl survey data

##### set place categories #####
x$place[grepl("North Pacific", x$Locality)] <- 'North Pacific'
x$place[grepl("Bering Sea", x$Locality)] <- 'Bering Sea'
x$place[grepl("Umnak", x$Locality)] <- 'Umnak'
x$place[grepl("Unalaska", x$Locality)] <- 'Unalaska'
x$place[grepl("Attu", x$Locality)] <- 'Attu'
x$place[grepl("Semichi", x$Locality)] <- 'Semichi'
# table(x$place, useNA = 'always')

##### set habitat categories #####
x$habitat[grepl("Rock-", x$Habitat)] <- 'Rock'
x$habitat[grepl("Sand-", x$Habitat)] <- 'Sand'
x$habitat[grepl("Cobble-", x$Habitat)] <- 'Cobble'
x$habitat[grepl("MixedCoarse-", x$Habitat)] <- 'MixedCoarse'
x$habitat[grepl("Gravel-", x$Habitat)] <- 'Gravel'
x$habitat[grepl("Boulder", x$Habitat)] <- 'Boulder'
# table(x$place, useNA = 'always')

##### filter some problem sites #####
x <- x %>% filter(EventID != 'HAPC_43')
x <- x %>% filter(EventID != 'TG16_21')
x <- x %>% filter(EventID != 'TG16_22')
x <- x %>% filter(EventID != 'transect 2012-39')
x <- x %>% filter(EventID != 'transect 2014-56')
x <- x %>% filter(EventID != 'transect 2012-104')
x <- x %>% filter(EventID != 'transect 2012-62')
x <- x %>% filter(AnalysisCategory != 'Ptilosarcus gurneyi')

##### add IndividualCount variable #####
x$IndividualCount <- 1

##### create site X species matrix #####
site.sp <- dcast(x,
                 EventID ~ AnalysisCategory,
                 value.var = "TotalDensity",
                 fun.aggregate = sum)
site.sp <- data.frame(site.sp)

##### assign EventID to the row names #####
row.names(site.sp) <- site.sp$EventID

##### remove the EventID variable #####
site.sp <- site.sp %>%
  dplyr::select(-EventID)

##### create vector of site labels #####
sitenames <- row.names(site.sp)

##### running NMDS on site by species matrix #####
NMDS <- metaMDS(site.sp,
                distance = "bray",
                binary = T,
                k=4,
                trymax = 40)

##### *** plot the NMDS results *** #####
##### make a site centered plot using ordiplot #####
## set zoom
xlow <- -4
xhigh <- 0
ylow <- -.7
yhigh <- 1.5

ordiplot(NMDS,
         display = "sites",
         type = "n",
         # xlim = c(xlow, xhigh),
         # ylim = c(ylow, yhigh)
         )
points(NMDS,
       display = "sites",
       pch = 21, col = "blue",
       bg = "lightblue", cex = .5)
text(NMDS,
     display = "sites",
     col = "black",
     cex = 0.3)

##### make a species centered plot using ordiplot #####
## set zoom
xlow <- -4
xhigh <- 5
ylow <- -5
yhigh <- 5

## set zoom
# xlow <- -2.0
# xhigh <- -2.0
# ylow <- -0.3
# yhigh <- 0.2

ordiplot(NMDS,
         display = "species",
         add = TRUE,
         col = "red",
         pch = 24,
         bg = "orange",
         cex = 0.2,
         # xlim = c(xlow, xhigh),
         # ylim = c(ylow, yhigh)
         )
text(NMDS,
     display = "species",
     col = "black",
     cex = 0.3)

##### extracting the site scores #####
site.scores <- as.data.frame(scores(NMDS, "site"))

##### joining information from original table #####
site.scores$EventID <- row.names(site.scores)
yo <- x %>% group_by(EventID, DepthCat, temp_cat, loc, DensityCategory, Locality, place, habitat) %>% summarize(n=n())
site.scores <- left_join(site.scores, yo)

##### extracting species scores #####
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species
species.scores$spec_code <- 1:(length(species.scores$species))


##### plotting the NMDS scores with habitat #####
hull <- site.scores %>% group_by(habitat) %>%
  slice(chull(NMDS2, NMDS1))

xlow <- min(site.scores$NMDS2)-.5
xhigh <- max(site.scores$NMDS2)+.5
ylow <- min(site.scores$NMDS1)-.25
yhigh <- max(site.scores$NMDS1)+.25

ggplot() +
  geom_point(data=site.scores,
             aes(x=NMDS2,
                 y=NMDS1,
                 color=habitat),
             size=2) +
  geom_polygon(data=hull,
               aes(x=NMDS2,
                   y=NMDS1,
                   fill=habitat,
                   group=habitat),
               alpha=0.1) + # add the convex hulls
  geom_text(data=species.scores,
            aes(x=NMDS2,y=NMDS1,
                label = species),
            check_overlap = TRUE,
            size=3) +
  # geom_text(data=site.scores,
  #           aes(x=NMDS2,y=NMDS1,
  #               label = EventID),
  #           check_overlap = TRUE,
  #           size=3) +
  coord_cartesian(xlim=c(xlow, xhigh),
                  ylim=c(ylow, yhigh))

ggsave('c:/rworking/deepseatools/images/20230426_NMDS_habitat_RPMcGuinn.png')


##### legend to species #####
species.scores %>%  dplyr::select(spec_code)

##### computing distance matrices #####
sp_bray_binary = vegdist(site.sp, method='bray', binary=T)
sp_bray = vegdist(site.sp, method='bray')
sp_bray_transformed = vegdist(wisconsin(sqrt(site.sp)), method='bray')

##### clusterfit (hclust) and make charts #####
## sp_bray_binary
fit <- hclust(sp_bray_binary)
groups <- cutree(fit, k=5)

plot(fit, main = "",
     sub = "",
     xlab="Sites",
     axes = T,
     ylab = "Ecological Distance",
     hang = -1, cex = 1)

rect.hclust(fit,
            k=5,
            border="red",
            cluster = groups
            )
setwd('c:/rworking/deepseatools/images')
dev.copy(png, "20230425_bray_binary_5_tree.png", width =1500, height = 700)
dev.off()

## sp_bray
fit <- hclust(sp_bray)
groups <- cutree(fit, k=5)

plot(fit, main = "",
     sub = "",
     xlab="Sites",
     axes = T,
     ylab = "Ecological Distance",
     hang = -1, cex = 1)

rect.hclust(fit,
            k=5,
            border="red",
            cluster = groups
)
setwd('c:/rworking/deepseatools/images')
dev.copy(png, "20230425_bray_5_tree.png", width =1500, height = 700)
dev.off()

## sp_bray_transformed
fit <- hclust(sp_bray_transformed)
groups <- cutree(fit, k=5)

plot(fit, main = "",
     sub = "",
     xlab="Sites",
     axes = T,
     ylab = "Ecological Distance",
     hang = -1, cex = 1)

rect.hclust(fit,
            k=5,
            border="red",
            cluster = groups
)
setwd('c:/rworking/deepseatools/images')
dev.copy(png, "20230425_bray_transformed_5_tree.png", width =1500, height = 700)
dev.off()

##### indicator species analysis #####
library(indicspecies)

indval = multipatt(site.sp, groups,
                   control = how(nperm=999))

summary(indval, alpha = 1, indvalcomp=TRUE)
indval$sign
View(indval$sign)

##### produce shapefile with groups attached #####
sitegroups <- data.frame(sitenames,groups)
mapdata2 <- left_join(com, sitegroups, by = c("EventID" = "sitenames"))
sub2 <- mapdata2 %>%
  filter(Latitude != '-999' , Longitude != '-999')
projcrs <- 4326
domainCRS <- 6393
geosub <- st_as_sf(x = sub2,
                   coords = c("Longitude", "Latitude"),
                   crs = projcrs)
geosub <- st_transform(geosub, crs = domainCRS)
st_write(geosub,
         "C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230404-0_THourigan_Aleutian_Community_Analysis_RPMcGuinn/shapefiles/mapdata2.shp",
         delete_dsn = T)

##### **check #####
mapdata2 %>% group_by(EventID, groups) %>% summarize(n=n()) %>% View()

##### *** for map layout *** #####
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

##### import protected area shapefile using sf #####
pa <- sf::st_read("C:/data/gis_data/protected_areas/shapefiles/20221104_protected_areas.shp")
