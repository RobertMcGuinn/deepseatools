##### Header #####
## author: Robert McGuinn, rpm@alumni.duke.edu
## date started: 20230510
## purpose: ISDSC8 conference in Edinburgh, Scotland
## drive: https://drive.google.com/drive/folders/1e5-RzLCIOW4swlwGiemMnOoMgDyi_tal?usp=share_link

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
sub <- filt %>% filter(is.na(FishCouncilRegion) == F,
                       as.numeric(DepthInMeters) < 3000,
                       as.numeric(DepthInMeters) > 2000,
                       TaxonRank == 'species',
                       Phylum == 'Cnidaria')
list <- unique(sub$FishCouncilRegion)

for(i in list){
x <- sub %>% filter(FishCouncilRegion == i)
g <- ggplot(x, aes(reorder(ScientificName, DepthInMeters, FUN=median), as.numeric(DepthInMeters),fill=Order)) +
  geom_boxplot() +
  scale_y_reverse() +
  ylab("Depth (meters)") +
  xlab("Genus") +
  theme_bw(base_size = 22, base_family = "Cambria") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, face = 'italic'))  +
  #  geom_hline(yintercept = 150, col = 'grey') +
  geom_hline(yintercept = 300, col = 'grey') #+
#  geom_hline(yintercept = 600, col = 'grey')

set.seed(8)
g + scale_fill_manual(values = sample(big_palette))
#g + scale_color_manual(values = brewer.pal(12, "Paired")[c(10,9,8,7,6,5,4,3,2,1)])
ggsave(paste("c:/rworking/deepseatools/images/ISDSC8_Scotland_RPMcGuinn/",
             "20230510_",
             "NatDB_",
             unique(sub$DatabaseVersion),
             '_',
             i,
             ".png",
             sep = ''),
       width = 20,
       height = 15,
       units = "in")
}


