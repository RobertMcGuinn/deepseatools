##### Header #####
# Date created: 2018_02_06
# Author: Robert P. McGuinn, rpm@alumni.duke.edu
# Purpose: Creating depth classes and drawing figure from specific list of genera.  
##### load data and transform #####
setwd("C:/rworking/digs/indata")
#indata<-read.csv("DSCRTP_NatDB_20171214-0.csv", header = T)
indata<-read.csv('20180130_0_Genus_NoFlag_TH.csv')

filt <- indata %>%
  filter(Flag == '0' | Flag == '2', 
         ObservationYear != "-999",
         DepthInMeters != "-999", 
         is.na(Phylum)== F)
options(digits = 1)

# changing levels of 'Ecoregion'
filt2 <- filt %>% mutate(Ecoregion = ifelse(Ecoregion == 'N_Carolinean', 'North Carolinian', as.character(Ecoregion)))
filt2 <- filt2 %>% mutate(Ecoregion = ifelse(Ecoregion == 'South Carolinean', 'South Carolinian', as.character(Ecoregion)))
table(filt2$Ecoregion)

minLon <- -84
maxLon <- -73 
minLat <- 22
maxLat <- 37

geofilt <- 
  filt2 %>% filter(as.numeric(Latitude) > minLat, 
                   as.numeric(Latitude) < maxLat, 
                   as.numeric(Longitude) < maxLon,
                   as.numeric(Longitude) > minLon)

coordinates(geofilt) <- c("Longitude", "Latitude")
proj4string(geofilt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### creating list of specific genera from Tom #####

genera_1 <- c('Phyllangia',
              'Renilla',
              'Leptogorgia',
              'Titanideum',
              'Balanophyllia',
              'Stichopathes',
              'Ellisella',
              'Tanacetipathes',
              'Diodogorgia', 
              'Nidalia',
              'Telesto',
              'Oculina',
              'Cladocora',
              'Thesea',
              'Paracyathus',
              'Dasmosmilia',
              'Polymyces')

genera_2 <- c('Bathypathes',
              'Stylaster',
              'Paramuricea',
              'Plumarella',
              'Thecopsammia',
              'Leiopathes',
              'Deltocyathus',
              'Madrepora',
              'Lophelia',
              'Enallopsammia',
              'Bathypsammia',
              'Swiftia',
              'Javania',
              'Pseudodrifa',
              'Clavularia',
              'Anthomastus',
              'Acanella',
              'Keratoisis',
              'Eunicella'
)

genera <- c('Phyllangia',
            'Renilla',
            'Leptogorgia',
            'Titanideum',
            'Balanophyllia',
            'Stichopathes',
            'Ellisella',
            'Tanacetipathes',
            'Diodogorgia', 
            'Nidalia',
            'Telesto',
            'Oculina',
            'Cladocora',
            'Thesea',
            'Paracyathus',
            'Dasmosmilia',
            'Polymyces',
            'Bathypathes',
            'Stylaster',
            'Paramuricea',
            'Plumarella',
            'Thecopsammia',
            'Leiopathes',
            'Deltocyathus',
            'Madrepora',
            'Lophelia',
            'Enallopsammia',
            'Bathypsammia',
            'Swiftia',
            'Javania',
            'Pseudodrifa',
            'Clavularia',
            'Anthomastus',
            'Acanella',
            'Keratoisis',
            'Eunicella'
)
##### building big palette #####
# to get the colors from a palette:
palette1 <- brewer.pal(9,"Set1")
palette2 <- brewer.pal(8,"Set2")
palette3 <- brewer.pal(9,"Set3")
palette4 <- brewer.pal(8,"Dark2")
palette5 <- brewer.pal(8,"Accent")

# We can just stick a few color palettes together
big_palette <- c(palette1,palette2,palette3, palette4, palette5)
##### setting depth class DepthCat2 - 2 class #####
filt2$DepthCat2[filt2$DepthInMeters > 0 & filt2$DepthInMeters <= 300] <- "< 300 m"
filt2$DepthCat2[filt2$DepthInMeters > 300 & filt2$DepthInMeters <= 2000] <- "300-2000 m"
filt2$DepthCat2 <- factor(filt2$DepthCat2, levels = c("0-300 m", "300-2000 m"))
# table(filt2$DepthCat2)

##### setting depth class DepthCat4 - 4 class ##### 
filt2$DepthCat4[filt2$DepthInMeters < 150] <- "< 150 m"
filt2$DepthCat4[filt2$DepthInMeters > 150 & filt2$DepthInMeters <= 300] <- "150-300 m"
filt2$DepthCat4[filt2$DepthInMeters > 300 & filt2$DepthInMeters <= 600] <- "300-600 m"
filt2$DepthCat4[filt2$DepthInMeters > 600] <- "> 600 m"
filt2$DepthCat4 <- factor(filt2$DepthCat4, levels = c("< 150 m", "150-300 m","300-600 m", "> 600 m" ))
# table(geofilt$DepthCat4)
#table(geofilt$DepthCat2)
##### draw figure #####
png(file="mygraphic.png",width=1000,height=687)
options(scipen=10000)
filt2$Ecoregion <- factor(filt2$Ecoregion, levels = c('North Carolinian', 'South Carolinian', 'Floridian'))
geofilt <- as.data.frame(geofilt) %>%
  filter(Ecoregion != 'Virginian' , is.na(Ecoregion) == F)
g <- ggplot(filt2, aes(DepthCat4, fill = Order)) +
  geom_bar() + 
  coord_flip() + 
  theme(text = element_text(size=20)) + 
  facet_wrap(~Ecoregion) +
  ylab("Number of Records") + 
  xlab("Depth Zone") + 
  scale_x_discrete(limits = rev(levels(filt2$DepthCat4))) +
  theme_bw(base_size = 22, base_family = "Cambria")

#display.brewer.all(colorblindFriendly=TRUE)
set.seed(8)
g + scale_fill_manual(values = sample(big_palette))
dev.off()

