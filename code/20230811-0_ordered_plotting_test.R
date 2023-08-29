##### Header #####
## Author: Robert McGuinn, rpm@alumni.duke.edu, robert.mcguinn@noaa.gov
## Started on: 20230811
## Forked from: Arvind Shantharam, arvind.shantharam@noaa.gov

##### packages #####
library(ggplot2)
library(optpart)
library(tidyverse)

##### load data #####
setwd('c:/rworking/deepseatools/indata')
df <- read.csv("20230811-0_OverlapWarrensIDissim.csv", header = T)
dist <- dist(df)

##### prep clusters and silhouette #####
opt.10 <- optpart(16,dist) #use large number to find optimal clustering close to sample size
sil.10 <- optsil(opt.10,dist,10000) # make take a few minutes
silhouettes <- summary(silhouette(sil.10,dist)) #cluster stats
sil.val <- data.frame(silhouette(sil.10,dist)) #Puts silhouette values of taxa into data frame
sil.val <- cbind(sil.val, df[,1])
names(sil.val) <- c('cluster', 'neighbor', 'sil_width', 'species')

##### setting colors #####
my_colors <- c(
  '1' =  "#FFFF00",
  '2' =  "#00E6A9",
  '3' =  "#000000",
  '4' = "#FF0000",
  '5' =  "#00734C",
  '6' = "#0000FF",
  '7' = "#7F7F7F",
  '8' = "#A80084",
  '9' = "#FF00C5",
  '10' = "#D1FF73"
)

##### plotting #####
sil.plot <- sil.val %>%
  ggplot(aes(x = reorder(species, cluster, FUN = max),
             y = sil_width,
             fill = factor(cluster))) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Silhouette width") +
  coord_flip() +
  scale_fill_manual(values = my_colors) +
  theme_minimal()

sil.plot
##### check values #####
# sil.val$sil_width







