##### Header: NMDS for Deep Sea Coral #####
# Author: Robert P. McGuinn
# Date Started: 20180131
# Location: Charleston, SC

##### input datasets #####
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
filt2 <- filt %>% mutate(Ecoregion = ifelse(Ecoregion == 'N_Carolinean', 'North Carolinean', as.character(Ecoregion)))

minLon <- -84
maxLon <- -73 
minLat <- 22
maxLat <- 37

geofilt <- 
  filt2 %>% filter(as.numeric(Latitude) > minLat, 
                   as.numeric(Latitude) < maxLat, 
                   as.numeric(Longitude) < maxLon,
                   as.numeric(Longitude) > minLon)

#coordinates(geofilt) <- c("Longitude", "Latitude")
#proj4string(geofilt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### setting depth class DepthCat2 - 2 class #####
geofilt$DepthCat2[geofilt$DepthInMeters > 0 & geofilt$DepthInMeters <= 300] <- "< 300 m"
geofilt$DepthCat2[geofilt$DepthInMeters > 300 & geofilt$DepthInMeters <= 2000] <- "300-2000 m"
geofilt$DepthCat2 <- factor(geofilt$DepthCat2, levels = c("0-300 m", "300-2000 m"))
# table(geofilt$DepthCat2)

##### setting depth class DepthCat4 - 4 class ##### 
geofilt$DepthCat4[geofilt$DepthInMeters < 150] <- "< 150 m"
geofilt$DepthCat4[geofilt$DepthInMeters > 150 & geofilt$DepthInMeters <= 300] <- "150-300 m"
geofilt$DepthCat4[geofilt$DepthInMeters > 300 & geofilt$DepthInMeters <= 600] <- "300-600 m"
geofilt$DepthCat4[geofilt$DepthInMeters > 600] <- "> 600 m"
geofilt$DepthCat4 <- factor(geofilt$DepthCat4, levels = c("< 150 m", "150-300 m","300-600 m", "> 600 m" ))
# table(geofilt$DepthCat4)
#table(geofilt$DepthCat2)

##### start empty subset varialble #####
geofilt$rep <- NA

##### summarize unique genera and set threshold#####
sum_tbl <-
  as.data.frame(geofilt) %>%
  group_by(Genus) %>%
  summarize(
    Records = n()
  )
sum_tbl <- sum_tbl %>%
  filter(Records > 30)

# select threshold for number of occurrences 
# over the project area of interest.
##### Select only genera to be included as specfied above #####
geofilt2 <- geofilt %>%
  filter(Genus %in% sum_tbl$Genus)

# setdiff(unique(geofilt2$Genus), genera_1)
# setdiff(genera_1, unique(geofilt2$Genus))

##### Populate IndividualCount with 1 #####

geofilt2$IndividualCount = 1

##### create site X species matrix #####
library(vegan)
site.sp <- cast(geofilt2, Ecoregion + DepthCat4 ~ Genus, 
                value='IndividualCount',
                sum)

site.sp <- as.data.frame(site.sp)

# setwd("C:/rworking/digs/outdata")
# write.csv(site.sp, 'site.sp.csv')

# creating a site variable
site.sp$site <- paste(site.sp$Ecoregion, site.sp$DepthCat4, sep = '|')

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-Ecoregion, -DepthCat4)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)

# # limit to sites that actually have more than X observations
# site.sp <- site.sp[apply(site.sp, 1, sum) > 200,]
# dim(site.sp)

# making site numeric
# site.sp$site <- as.numeric(factor(site.sp$site))
# str(site.sp)


##### running NMDS starting with site X species matrix #####
#install.packages("vegan")
library(vegan)
NMDS <- metaMDS(site.sp, distance = "jaccard", binary = T, k=2)

##### creating a group variable for Ecoregion ##### 
site.sp2 <- cast(geofilt2, Ecoregion + DepthCat4 ~ Genus, 
                 value='IndividualCount',
                 sum)

site.sp2 <- as.data.frame(site.sp2)
Ecoregion <- site.sp2$Ecoregion

##### extracting the site and species scores for use with ggplot2 ##### 
# extracting site scores 
site.scores <- as.data.frame(scores(NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)  # create a column of site names, from the rownames of data.scores
site.scores$Ecoregion <- Ecoregion  #  add the Ecoregion variable created earlier
head(site.scores)  #look at the data

# extracting species scores
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

##### create a hull around the grouping variable Ecoregion #####
Ecoregion.a <- site.scores[site.scores$Ecoregion == "Floridian", ][chull(site.scores[site.scores$Ecoregion == 
                                                                           "Floridian", c("NMDS1", "NMDS2")]), ]  # hull values for Ecoregion A
Ecoregion.b <- site.scores[site.scores$Ecoregion == "South Carolinean", ][chull(site.scores[site.scores$Ecoregion == 
                                                                                  "South Carolinean", c("NMDS1", "NMDS2")]), ]  # hull values for Ecoregion B
Ecoregion.c <- site.scores[site.scores$Ecoregion == "North Carolinean", ][chull(site.scores[site.scores$Ecoregion == 
                                                                                  "North Carolinean", c("NMDS1", "NMDS2")]), ]  # hull values for Ecoregion A

hull.data <- rbind(Ecoregion.a, Ecoregion.b, Ecoregion.c)  #combine groups
hull.data


##### cleaned up ggplot when using group = Ecoregion ##### 
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Ecoregion,group=Ecoregion),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),
            alpha=0.5, 
            fontface = "bold"
            #position=position_jitter(width=.4,height=.4)
            ) +  # add the species labels
  geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=site),
            alpha=0.5, 
            fontface = "bold"
            #position=position_jitter(width=.4,height=.4)
  ) +
  
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=Ecoregion,colour=Ecoregion),size=4) + # add the point markers
  scale_colour_manual(values = 
                        c('Floridian' = 'blue', 
                          'North Carolinean' = 'green', 
                          'South Carolinean' = 'red')) +
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2)) + 
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

##### creating a group variable for depthCat##### 
site.sp2 <- cast(geofilt2, Ecoregion + DepthCat4 ~ Genus, 
                 value='IndividualCount',
                 sum)

site.sp2 <- as.data.frame(site.sp2)
Depth_Zone <- site.sp2$DepthCat4

##### extracting the species scores for use with ggplot2 ##### 
# extracting site scores 
site.scores <- as.data.frame(scores(NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)  # create a column of site names, from the rownames of data.scores
site.scores$Depth_Zone <- Depth_Zone  #  add the Depth_Zone variable created earlier
head(site.scores)  #look at the data

# extracting species scores
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

##### create a hull around the grouping variable DepthCat ##### 
Depth_Zone.a <- site.scores[site.scores$Depth_Zone == "< 150 m", ][chull(site.scores[site.scores$Depth_Zone == 
                                                                         "< 150 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone A
Depth_Zone.b <- site.scores[site.scores$Depth_Zone == "150-300 m", ][chull(site.scores[site.scores$Depth_Zone == 
                                                                      "150-300 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone B
Depth_Zone.c <- site.scores[site.scores$Depth_Zone == "300-600 m", ][chull(site.scores[site.scores$Depth_Zone == 
                                                                         "300-600 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone A
Depth_Zone.d <- site.scores[site.scores$Depth_Zone == "> 600 m", ][chull(site.scores[site.scores$Depth_Zone == 
                                                                      "> 600 m", c("NMDS1", "NMDS2")]), ]  # hull values for Depth_Zone B
hull.data <- rbind(Depth_Zone.a, Depth_Zone.b, Depth_Zone.c, Depth_Zone.d)  #combine groups
hull.data

##### cleaned up plot with hulls groups = DepthCat ##### 
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Depth_Zone,group=Depth_Zone),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),
            alpha=0.5, 
            fontface = "bold"#,
            #position=position_jitter(width=.5,height=.5)
            ) +  # add the species labels
  # geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,colour=Depth_Zone),size=4) +
  #   scale_colour_manual(values = c('< 150 m' = 'blue',
  #                                '150-300 m' = 'red', 
  #                                '300-600 m' = 'green', 
  #                                '> 600 m' = 'black')) +
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=Ecoregion),size=4) + # add the point markers
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2), ) + 
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

##### the transformation applied to the data by the metaMDS function #####
wisconsin(sqrt(site.sp))

##### create distance matrix #####
library(vegan)
d <- vegdist(site.sp, method = "chao", binary = T)

##### basic visualizations of metaMDS plots ##### 
# set the cut tree
NMDS
stressplot(NMDS)
plot(NMDS)
ordiplot(NMDS)
ordihull(NMDS, cl, lty=6, label = TRUE)
ordispider(NMDS, cl, col="blue", label=TRUE)
ordiellipse(NMDS, cl, col="blue")
orditorp(NMDS,display="sites",cex=1,air=.09)
orditorp(NMDS,display="species",col="red",air=3)

ordiplot(NMDS)
cl <- cutree(caver, 4)
ordispider(NMDS, cl, col="blue", label=TRUE)
orditorp(NMDS,display="species",col="red",air=3)

caver <- hclust(d, method="aver")
plot(caver, hang = -1)
rect.hclust(caver,4)


# use this if you want to select which things to label
fig <- ordiplot(NMDS)
identify(fig, "species")
identify(fig, "sites")

##### clustering methods #####
# set margins
# bottom, left, top, right
par(5,5,5,5)
csin <- hclust(d, method="single")
plot(csin, labels = NULL, 
     main = "Cluster dendrogram", sub = NULL, ylab = "Height")
rect.hclust(csin,5)

ccom <- hclust(d, method="complete")
plot(ccom, labels = NULL, 
     main = "Cluster dendrogram", sub = NULL, ylab = "Height")
rect.hclust(ccom,5)

caver <- hclust(d, method="aver")
plot(caver, hang = -1)
rect.hclust(caver,5)


##### _____ setting up sites from (3 reps X 2 depth X 3 ecoregions) #####
##### North Carolinean #####
geofilt_a <- as.data.frame(geofilt) %>% 
  filter(DepthCat == 'shallow', 
         Ecoregion == 'North Carolinean')
geofilt_a$rep[1:(length(geofilt_a$CatalogNumber)/3)] <- 'shallow_nc_1'
geofilt_a$rep[((length(geofilt_a$CatalogNumber)/3) + 1):((length(geofilt_a$CatalogNumber)/3)*2)] <- 'shallow_nc_2'
geofilt_a$rep[(((length(geofilt_a$CatalogNumber)/3)*2)+1):(length(geofilt_a$CatalogNumber))] <- 'shallow_nc_3'

geofilt_b <- as.data.frame(geofilt) %>% 
  filter(DepthCat == 'deep', 
         Ecoregion == 'North Carolinean')
geofilt_b$rep[1:(length(geofilt_b$CatalogNumber)/3)] <- 'deep_nc_1'
geofilt_b$rep[((length(geofilt_b$CatalogNumber)/3) + 1):((length(geofilt_b$CatalogNumber)/3)*2)] <- 'deep_nc_2'
geofilt_b$rep[(((length(geofilt_b$CatalogNumber)/3)*2)+1):(length(geofilt_b$CatalogNumber))] <- 'deep_nc_3'

##### South Carolinean #####
geofilt_c <- as.data.frame(geofilt) %>% 
  filter(DepthCat == 'shallow', 
         Ecoregion == 'South Carolinean')
geofilt_c$rep[1:(length(geofilt_c$CatalogNumber)/3)] <- 'shallow_sc_1'
geofilt_c$rep[((length(geofilt_c$CatalogNumber)/3) + 1):((length(geofilt_c$CatalogNumber)/3)*2)] <- 'shallow_sc_2'
geofilt_c$rep[(((length(geofilt_c$CatalogNumber)/3)*2)+1):(length(geofilt_c$CatalogNumber))] <- 'shallow_sc_3'

geofilt_d <- as.data.frame(geofilt) %>% 
  filter(DepthCat == 'deep', 
         Ecoregion == 'South Carolinean')
geofilt_d$rep[1:(length(geofilt_d$CatalogNumber)/3)] <- 'deep_sc_1'
geofilt_d$rep[((length(geofilt_d$CatalogNumber)/3) + 1):((length(geofilt_d$CatalogNumber)/3)*2)] <- 'deep_sc_2'
geofilt_d$rep[(((length(geofilt_d$CatalogNumber)/3)*2)+1):(length(geofilt_d$CatalogNumber))] <- 'deep_sc_3'

##### Floridian #####
geofilt_e <- as.data.frame(geofilt) %>% 
  filter(DepthCat == 'shallow', 
         Ecoregion == 'Floridian')
geofilt_e$rep[1:450] <- 'shallow_fl_1'
geofilt_e$rep[451:901] <- 'shallow_fl_2'
geofilt_e$rep[902:1349] <- 'shallow_fl_3'

geofilt_f <- as.data.frame(geofilt) %>% 
  filter(DepthCat == 'deep', 
         Ecoregion == 'Floridian')
geofilt_f$rep[1:953] <- 'deep_fl_1'
geofilt_f$rep[954:1907] <- 'deep_fl_2'
geofilt_f$rep[1908:2858] <- 'deep_fl_3'

##### checking ##### 

table(geofilt_a$rep, useNA = 'always')
table(geofilt_b$rep, useNA = 'always')
table(geofilt_c$rep, useNA = 'always')
table(geofilt_d$rep, useNA = 'always')
table(geofilt_e$rep, useNA = 'always')
table(geofilt_f$rep, useNA = 'always')

##### bind the subsets back together 
geofilt2 <- rbind(geofilt_a,geofilt_b,geofilt_c,geofilt_d, geofilt_e, geofilt_f)


##### cleaned up ggplot when using group = Ecoregion ##### 
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Ecoregion,group=Ecoregion),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),
            alpha=0.5, 
            fontface = "bold"
            #position=position_jitter(width=.4,height=.4)
  ) +  # add the species labels
  geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=site),
            alpha=0.5, 
            fontface = "bold"
            #position=position_jitter(width=.4,height=.4)
  ) +
  
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=Ecoregion,colour=Ecoregion),size=4) + # add the point markers
  scale_colour_manual(values = 
                        c('Floridian' = 'blue', 
                          'North Carolinean' = 'green', 
                          'South Carolinean' = 'red')) +
  geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2)) + 
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


##### Header: NMDS for Deep Sea Coral #####
# Author: Robert P. McGuinn
# Date Started: 20180131
# Location: Charleston, SC


##### start empty subset varialble #####
geofilt$rep <- NA

##### _____ setting up sites from (3 reps X 2 depth X 3 ecoregions) #####
##### North Carolinean #####
geofilt_a <- as.data.frame(geofilt) %>% 
  filter(DepthCat2 == '0-300', 
         Ecoregion == 'North Carolinean')
geofilt_a$rep[1:(length(geofilt_a$CatalogNumber)/3)] <- 'shallow_nc_1'
geofilt_a$rep[((length(geofilt_a$CatalogNumber)/3) + 1):((length(geofilt_a$CatalogNumber)/3)*2)] <- 'shallow_nc_2'
geofilt_a$rep[(((length(geofilt_a$CatalogNumber)/3)*2)+1):(length(geofilt_a$CatalogNumber))] <- 'shallow_nc_3'

geofilt_b <- as.data.frame(geofilt) %>% 
  filter(DepthCat2 == '300-2000', 
         Ecoregion == 'North Carolinean')
geofilt_b$rep[1:(length(geofilt_b$CatalogNumber)/3)] <- 'deep_nc_1'
geofilt_b$rep[((length(geofilt_b$CatalogNumber)/3) + 1):((length(geofilt_b$CatalogNumber)/3)*2)] <- 'deep_nc_2'
geofilt_b$rep[(((length(geofilt_b$CatalogNumber)/3)*2)+1):(length(geofilt_b$CatalogNumber))] <- 'deep_nc_3'

##### South Carolinean #####
geofilt_c <- as.data.frame(geofilt) %>% 
  filter(DepthCat2 == '0-300', 
         Ecoregion == 'South Carolinean')
geofilt_c$rep[1:(length(geofilt_c$CatalogNumber)/3)] <- 'shallow_sc_1'
geofilt_c$rep[((length(geofilt_c$CatalogNumber)/3) + 1):((length(geofilt_c$CatalogNumber)/3)*2)] <- 'shallow_sc_2'
geofilt_c$rep[(((length(geofilt_c$CatalogNumber)/3)*2)+1):(length(geofilt_c$CatalogNumber))] <- 'shallow_sc_3'

geofilt_d <- as.data.frame(geofilt) %>% 
  filter(DepthCat2 == '300-600', 
         Ecoregion == 'South Carolinean')
geofilt_d$rep[1:(length(geofilt_d$CatalogNumber)/3)] <- 'deep_sc_1'
geofilt_d$rep[((length(geofilt_d$CatalogNumber)/3) + 1):((length(geofilt_d$CatalogNumber)/3)*2)] <- 'deep_sc_2'
geofilt_d$rep[(((length(geofilt_d$CatalogNumber)/3)*2)+1):(length(geofilt_d$CatalogNumber))] <- 'deep_sc_3'

##### Floridian #####
geofilt_e <- as.data.frame(geofilt) %>% 
  filter(DepthCat2 == '0-300', 
         Ecoregion == 'Floridian')
geofilt_e$rep[1:450] <- 'shallow_fl_1'
geofilt_e$rep[451:901] <- 'shallow_fl_2'
geofilt_e$rep[902:1349] <- 'shallow_fl_3'

geofilt_f <- as.data.frame(geofilt) %>% 
  filter(DepthCat2 == '300-600', 
         Ecoregion == 'Floridian')
geofilt_f$rep[1:953] <- 'deep_fl_1'
geofilt_f$rep[954:1907] <- 'deep_fl_2'
geofilt_f$rep[1908:2858] <- 'deep_fl_3'

##### checking ##### 

table(geofilt_a$rep, useNA = 'always')
table(geofilt_b$rep, useNA = 'always')
table(geofilt_c$rep, useNA = 'always')
table(geofilt_d$rep, useNA = 'always')
table(geofilt_e$rep, useNA = 'always')
table(geofilt_f$rep, useNA = 'always')

##### bind the subsets back together 
geofilt2 <- rbind(geofilt_a,geofilt_b,geofilt_c,geofilt_d, geofilt_e, geofilt_f)

##### summarize unique genera and set threshold#####
sum_tbl <-
  as.data.frame(geofilt) %>%
  group_by(Genus) %>%
  summarize(
    Records = n()
  )
sum_tbl <- sum_tbl %>%
  filter(Records > 30)

# select threshold for number of occurrences 
# over the project area of interest.
# Select only genera to be included as specfied above.
geofilt3 <- as.data.frame(geofilt2) %>%
  filter(Genus %in% sum_tbl$Genus)

##### Populate IndividualCount with 1 #####

geofilt3$IndividualCount = 1

##### create site X species matrix #####
site.sp <- cast(geofilt3, rep ~ Genus, 
                value='IndividualCount',
                sum)

site.sp <- as.data.frame(site.sp)

# setwd("C:/rworking/digs/outdata")
# write.csv(site.sp, 'site.sp.csv')

# creating a site variable
site.sp$site <- site.sp$rep

# getting rid of non-needed variables
site.sp <- site.sp %>%
  dplyr::select(-rep)

# # moving the site variable to the beginning
# col_idx <- grep("site", names(site.sp))
# site.sp <- site.sp[, c(col_idx, (1:ncol(site.sp))[-col_idx])]
# # names(site.sp)

# set the site as the row.names
row.names(site.sp) <- site.sp$site

# remove the site variable
site.sp <- site.sp %>%
  dplyr::select(-site)

# making it a matrix
site.sp <- as.matrix(site.sp)

# # limit to sites that actually have more than X observations
# site.sp <- site.sp[apply(site.sp, 1, sum) > 200,]
# dim(site.sp)

# making site numeric
# site.sp$site <- as.numeric(factor(site.sp$site))
# str(site.sp)

##### create a presence only site x species matrix ##### 
site.sp_bin <- ifelse(site.sp > 0, 1, 0)

##### create presence-only distance matrix #####
library(vegan)
d <- vegdist(site.sp, method = 'jaccard', binary = T)

##### running NMDS starting with site X species matrix #####
#install.packages("vegan")
library(vegan)
NMDS <- metaMDS(site.sp, distance = 'jaccard', binary = T, k=2)

##### creating a group variable for Ecoregion ##### 
site.sp2 <- cast(geofilt3, rep + Ecoregion ~ Genus, 
                 value='IndividualCount',
                 sum)

site.sp2 <- as.data.frame(site.sp2)
grp <- site.sp2$Ecoregion

##### extracting the species scores for use with ggplot2 ##### 
# extracting site scores 
site.scores <- as.data.frame(scores(NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)  # create a column of site names, from the rownames of data.scores
site.scores$grp <- grp  #  add the grp variable created earlier
head(site.scores)  #look at the data

# extracting species scores
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

##### create a hull around the grouping variable Ecoregion #####
grp.a <- site.scores[site.scores$grp == "Floridian", ][chull(site.scores[site.scores$grp == 
                                                                           "Floridian", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- site.scores[site.scores$grp == "South Carolinean", ][chull(site.scores[site.scores$grp == 
                                                                                  "South Carolinean", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.c <- site.scores[site.scores$grp == "North Carolinean", ][chull(site.scores[site.scores$grp == 
                                                                                  "North Carolinean", c("NMDS1", "NMDS2")]), ]  # hull values for grp A

hull.data <- rbind(grp.a, grp.b, grp.c)  #combine groups
hull.data


##### cleaned up ggplot when using group = Ecoregion ##### 
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),
            alpha=0.5, 
            fontface = "bold",
            position=position_jitter(width=.4,height=.4)) +  # add the species labels
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=4) + # add the point markers
  scale_colour_manual(values = 
                        c('Floridian' = 'blue', 
                          'North Carolinean' = 'green', 
                          'South Carolinean' = 'red')) +
  
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


```

######Figure 11: NMDS results.  Sites are colored by depth zone..

``` {r NMDS2, echo=FALSE, cache = FALSE, dpi=300, warning=FALSE, message=FALSE, results='hide'}

##### creating a group variable for depthCat##### 
site.sp2 <- cast(geofilt3, rep + DepthCat2 ~ Genus, 
                 value='IndividualCount',
                 sum)

site.sp2 <- as.data.frame(site.sp2)
grp <- site.sp2$DepthCat