##### header #####
## author: Robert McGuinn | robert.mcguinn@noaa.gov | rpm@alumni.duke.edu
## file_start: 20230927
## purpose: map graphic production work for million record announcement

##### packages ######
library(sf)
library(st)

###### load shapefile of fish council region ######
fcr <- sf::st_read("C:/rworking/deepseatools/indata/fishery_management_council_regions_20210609/20210609_fishery_management_council_regions.shp")

##### subset NDB by bounding box variables #####
filt_geo <- st_as_sf(filt,
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)

##### intersect points #####
filt_geo_fcr <- st_intersection(filt_geo, fcr)

##### fix_data #####
## correction from Tom Hourigan
catlist <- c(1138122,1138471,1138520,1139150, 1139153, 1139157, 1139200, 1139303, 1139348, 1139447, 1142884)
filt <- filt %>% mutate(VernacularNameCategory = ifelse(as.numeric(CatalogNumber) %in% catlist,
                                                        'gorgonian coral',
                                                        VernacularNameCategory))
##### make depth box-plots #####
ylower <- 0
yupper <- 3500


sub <- filt %>% filter(is.na(FishCouncilRegion) == F,
                       as.numeric(DepthInMeters) < yupper,
                       as.numeric(DepthInMeters) > ylower,
                       is.na(VernacularNameCategory) == F,
                       VernacularNameCategory != 'fish')

list <- unique(sub$FishCouncilRegion)
sub$VernacularNameCategory <- as.factor(sub$VernacularNameCategory)

## original color pallette for DSCRTP
# "stony coral (branching)" =  "#FFFFFF",
# "stony coral (cup coral)" =  "#00E6A9",
# "black coral" =  "#000000",
# "gorgonian coral" = "#FF0000",
# "soft coral" =  "#00734C",
# "sea pen" = "#0000FF",
# "stoloniferan coral" = "#7F7F7F",
# "lace coral" = "#A80084",
# "lithotelestid coral" = "#FF00C5",
# "stony coral (unspecified)" = "#D1FF73",
# "gold coral" =  "#FFFF00",
# "sponge (unspecified)" = "#FF0000",
# "demosponge" = "#00FFC5",
# "glass sponge" = "#C500FF",
# "homoscleromorph sponge" = "#FFFF00",
# "calcareous sponge" = "#55FF00",
# "other coral-like hydrozoan" = "#00FFFF",
# "alcyonacean (unspecified)" = "#D7B09E"


my_colors <- c(
  "stony coral (branching)" =  "#FFFFFF",
  "stony coral (cup coral)" =  "#00E6A9",
  "black coral" =  "#000000",
  "gorgonian coral" = "#FF0000",
  "soft coral" =  "#00734C",
  "sea pen" = "#0000FF",
  "stoloniferan coral" = "#7F7F7F",
  "lace coral" = "#A80084",
  "lithotelestid coral" = "#FF00C5",
  "stony coral (unspecified)" = "#D1FF73",
  "gold coral" =  "#FFFF00",
  "sponge (unspecified)" = "#FF0000",
  "demosponge" = "#00FFC5",
  "glass sponge" = "#C500FF",
  "homoscleromorph sponge" = "#FFFF00",
  "calcareous sponge" = "#55FF00",
  "other coral-like hydrozoan" = "#00FFFF",
  "alcyonacean (unspecified)" = "#D7B09E"
)

for(i in list){
  x <- sub %>% filter(FishCouncilRegion == i)
  g <- ggplot(x, aes(reorder(VernacularNameCategory, DepthInMeters, FUN=median),
                     as.numeric(DepthInMeters),
                     fill = VernacularNameCategory)) +
    geom_boxplot() +
    scale_y_reverse() +
    ylab("Depth (meters)") +
    xlab("Vernacular Name Category") +
    theme_bw(base_size = 13, base_family = "Cambria") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = 'italic'),
          axis.title.y = element_text(margin = margin(r = 30))) +
    scale_fill_manual(values = my_colors) +
    labs(fill = "Vernacular Name Category")


  ggsave(paste("c:/rworking/deepseatools/images/ISDSC8_Scotland_RPMcGuinn/",
               "20230927_",
               "NatDB_",
               unique(sub$DatabaseVersion),
               '_',
               i,
               ".png",
               sep = ''),
         width = 10,
         height = 6,
         units = "in")
}


##### create sf object from subset #####
points <- st_as_sf(filt, coords = c("Longitude", "Latitude"), crs = 4326)
points <- points %>% select(VernacularNameCategory, ScientificName)

##### ** check #####
plot(points)
filt %>% filter(SurveyID == 'NA052') %>% pull(DataContact) %>% unique()
filt %>% filter(grepl('1038228', CatalogNumber)) %>% pull(ScientificName) %>% unique()


##### export shapefile #####
st_write(points, "C:/Users/Robert.Mcguinn/Documents/ArcGIS/Projects/20230511-0_ISDSC8_Scotland_RPMcGuinn/shapefiles/sub_geo.shp", delete_dsn = T)

##### top ten tables #####

list <- unique(filt$FishCouncilRegion)

for(i in list){
  x <- filt %>%
    filter(
      FishCouncilRegion == i,
      Phylum != 'Chordata',
    ) %>%
    group_by(ScientificName, Phylum, FishCouncilRegion) %>%
    summarize(n=n()) %>%
    arrange(desc(n))
  x <- x %>% select(ScientificName, Phylum, n)
  print(x[1:20,])
}


"South Atlantic"
"Gulf of Mexico"
NA
"New England"
"Mid-Atlantic"
"Caribbean"
"North Pacific"
"Pacific"
"Western Pacific"



