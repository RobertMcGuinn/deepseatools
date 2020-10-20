##### Header #####
# Date Created: 2018-02-06
# Author: Robert P. McGuinn
# Email: rpm@alumni.duke.edu
# Location: Charleston, SC
# Purpose: Protected area analysis. Creating summary of records within
#   within each ecoregion and within depth zone.

##### Installation/Loading of Packages #####
#install.packages("pacman")
library(pacman)
#pacman::p_load(captioner, bundesligR)
library(captioner, bundesligR)
#install.packages("beanplot")
library(beanplot)
#install.packages("stringr")
library(stringr)
#install.packages("knitr")
library(knitr)
#install.packages("tidyr")
library(tidyr)
#install.packages("sp")
library(sp)
#install.packages("maptools")
library(maptools)
#install.packages("maps")
library(maps)
#install.packages("reshape")
library(reshape)
#install.packages("reshape2")
library(reshape2)
#install.packages("psych")
library(psych)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("data.table")
library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("car")
library(car)
#install.packages("gdata")
library(gdata)
#install.packages("digest")
library(digest)
#install.packages("rgdal")
library(rgdal)
#install.packages("ggmap")
library(ggmap)
#install.packages("rerddap")
library(rerddap)
#install.packages("raster")
library(raster)
#install.packages("rworldxtra")
library(rworldxtra)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("xtable")
library(xtable)
library(taxize)
library(rgdal)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("extrafont")
library(extrafont)
library(RColorBrewer)
library(googlesheets)
library(googledrive)

##### bringing in input datasets #####
setwd('C:/rworking/digs/indata')
indata<-read.csv('join_protected_20180206_0.csv', header = T)
filt <- indata

##### building big pallette #####
# to get the colors from a palette:
palette1 <- brewer.pal(9,"Set1")
palette2 <- brewer.pal(8,"Set2")
palette3 <- brewer.pal(9,"Set3")
palette4 <- brewer.pal(8,"Dark2")
palette5 <- brewer.pal(8,"Accent")

# We can just stick a few color palettes together
big_palette <- c(palette1,palette2,palette3, palette4, palette5)
##### setting depth class DepthCat4 - 4 class #####
filt$DepthCat4[filt$DpthInM < 150] <- "< 150 m"
filt$DepthCat4[filt$DpthInM > 150 & filt$DpthInM <= 300] <- "150-300 m"
filt$DepthCat4[filt$DpthInM > 300 & filt$DpthInM <= 600] <- "300-600 m"
filt$DepthCat4[filt$DpthInM > 600] <- "> 600 m"
filt$DepthCat4 <- factor(filt$DepthCat4, levels = c("< 150 m", "150-300 m","300-600 m", "> 600 m" ))
#table(filt$DepthCat4)
#table(filt$DepthCat2)
##### modifying join_count #####d
filt$Ecoregion <- filt$Ecoregn
filt$Join_Count <- as.character(filt$Join_Count)
filt2 <- filt %>% mutate(Join_Count = ifelse(Join_Count == '1' | Join_Count == '2', '1', as.character(Join_Count)))
filt2 <- filt2 %>% mutate(Join_Count = ifelse(Join_Count == '1', 'Inside Managed Area', as.character(Join_Count)))
filt2 <- filt2 %>% mutate(Join_Count = ifelse(Join_Count == '0', 'Outside Managed Area', as.character(Join_Count)))
filt2 <- filt2 %>% mutate(Ecoregion = ifelse(Ecoregion == 'N_Carolinean', 'North Carolinian', as.character(Ecoregion)))
filt2 <- filt2 %>% mutate(Ecoregion = ifelse(Ecoregion == 'South Carolinean', 'South Carolinian', as.character(Ecoregion)))

##### making the figure #####
png(file="mygraphic.png",width=1000,height=623)
options(scipen=10000)
filt2$Ecoregion <- factor(filt2$Ecoregion, levels = c('Northern Carolinian', 'Southern Carolinian', 'Floridian'))
g <- ggplot(x, aes(DepthCat4, fill = Ecoregion)) +
  geom_bar() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  facet_wrap(~Join_Count) +
  ylab("Number of Records") +
  xlab("Depth Zone") +
  scale_x_discrete(limits = rev(levels(filt2$DepthCat4))) +
  theme_bw(base_size = 20, base_family = "Cambria")

#display.brewer.all(colorblindFriendly=TRUE)
set.seed(8)
g + scale_fill_manual(values = sample(big_palette))
dev.off()

##### make a shapefile with the OGR driver #####
# geofilt <- filt
#
# coordinates(geofilt) <- c("Longitude", "Latitude")
# proj4string(geofilt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
#
# setwd("C:/rworking/digs/outdata")
# writeOGR(geofilt, dsn="geofilt_2018206_0",
#          layer= "geofilt_2018206_0",
#          driver = "ESRI Shapefile",
#          dataset_options=c("NameField=CatalogNumber"),
#          overwrite_layer = T)


##### table #####
sum_tbl <- filt2 %>%
  group_by(Ecoregion, Join_Count) %>%
  summarize(Records = n()
  ) %>%
  arrange(desc(Records))
View(sum_tbl)

setwd('C:/rworking/digs/outdata')
write.csv(sum_tbl, 'join_protected_20180206_0.csv')

