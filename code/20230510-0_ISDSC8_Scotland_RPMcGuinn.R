##### Header #####
## author: Robert McGuinn, rpm@alumni.duke.edu
## date started: 20230510
## purpose: ISDSC8 conference in Edinburgh, Scotland
## drive: https://drive.google.com/drive/folders/1e5-RzLCIOW4swlwGiemMnOoMgDyi_tal?usp=share_link
## aprx: 20230511-0_ISDSC8_Scotland_RPMcGuinn

##### packages ####
library(tidyverse)
library(ggplot2)
library(sf)
library(RColorBrewer)

##### load_data #####
source("C:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### building big palette #####
## to get the colors from a palette:
palette1 <- brewer.pal(9,"Set1")
palette2 <- brewer.pal(8,"Set2")
palette3 <- brewer.pal(9,"Set3")
palette4 <- brewer.pal(8,"Dark2")
palette5 <- brewer.pal(8,"Accent")

## We can just stick a few color palettes together
big_palette <- c(palette1,palette2,palette3, palette4, palette5)

##### make depth box-plots #####
ylower <- 0
yupper <- 3500


sub <- filt %>% filter(is.na(FishCouncilRegion) == F,
                       as.numeric(DepthInMeters) < yupper,
                       as.numeric(DepthInMeters) > ylower,
                       is.na(VernacularNameCategory) == F)

list <- unique(sub$FishCouncilRegion)
levels <- unique(sub$VernacularNameCategory)

set.seed(9)
color_lookup <- data.frame(VernacularNameCategory = levels,
                           color = sample(big_palette, 18))

sub <- left_join(sub, color_lookup, by = "VernacularNameCategory")
sub$VernacularNameCategory <- as.factor(sub$VernacularNameCategory)

# sub$color<-factor(sub$color)
# levels(sub$color) <- color_lookup$VernacularNameCategory
# levels(sub$color)

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
"sponge (unspecified)" = "#e0fff0",
"demosponge" = "#00FFC5",
"glass sponge" = "#C500FF",
"homoscleromorph sponge" = "#FFFF00",
"calcareous sponge" = "#55FF00",
"other coral-like hydrozoan" = "#00FFFF",
"alcyonacean (unspecified)" = "#D7B09E"
)



for(i in list){
  x <- sub %>% filter(FishCouncilRegion == i)
  g <- ggplot(x, aes(reorder(VernacularNameCategory, DepthInMeters, FUN=median), as.numeric(DepthInMeters), fill = VernacularNameCategory)) +
    geom_boxplot() +
    scale_y_reverse() +
    ylab("Depth (meters)") +
    xlab("Vernacular Name Category") +
    theme_bw(base_size = 13, base_family = "Cambria") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, face = 'italic'),
          axis.title.y = element_text(margin = margin(r = 20))) +
    scale_fill_manual(values = my_colors) +
    labs(fill = "Vernacular Name Category")


  ggsave(paste("c:/rworking/deepseatools/images/ISDSC8_Scotland_RPMcGuinn/",
               "20230511_",
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

##### images #####
