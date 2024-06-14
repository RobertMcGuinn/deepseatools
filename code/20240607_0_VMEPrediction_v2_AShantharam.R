###loading packages
library(raster)
library(sp)
library(tidyverse)
library(dendextend)
library(dismo)
library(foreach)
library(doParallel)

###### Setting working directory and temporary file repository##
setwd("E:/source files/rasters/bio rasters/Mean")

##### Set where temporary files and system environment instance are saved.
rasterOptions(tmpdir="E:/rtemp/") # large volume external hard drive
write("R_USER = E:/rtemp/", file=file.path(Sys.getenv('R_USER'), '.Renviron'))

##### Import tiff rasters ######

#first call all files in a single folder as a list
rastlist <- list.files(path = ".", pattern='.tif$',
                       all.files=T, full.names=F)

##### Generating the similarity matrix #####
overlap <- matrix(NA, ncol = length(rastlist), nrow = length(rastlist))

for (i in 1:(length(rastlist) - 1)) {

  rst1 <- raster(rastlist[i])

  for(j in (i + 1):(length(rastlist))){

    print(paste("Overlap between models", rastlist[i], "and", rastlist[j], Sys.time()))

    rst2 <- raster(rastlist[j])

    ov <- nicheOverlap(rst1, rst2, stat = "I", mask = T, checkNegatives = T)
    overlap[i, j] <- ov
    overlap[j, i] <- ov


  }
}

#Dissimilarity computation
overlap = 1-overlap

##### Add taxa names to rows and columns and save dissimilarity matrix #####
rslist1 = rastlist
rslist1 = gsub(".tif", "", rslist1)
colnames(overlap) = rslist1
rownames(overlap) = rslist1

#save the dissimilarity matrix
write.csv(overlap, file.choose(),
          row.names=T)

##### Plotting the dendrogram (cluster tree) #####
library(dendextend)

#dendogram
library(ggplot2)

overlap=read.csv(file.choose(), row.names = 1, header = T)
library(dendextend)
hc <- hclust(as.dist(overlap, diag = T), method = "ward.D2")
par(mar=c(4, 10, 1, 1), cex=0.8)
plot_horiz.dendrogram(hc, side=T)
abline(v=0.80, col="grey", lty=2, lwd=3)

title(xlab="Dissimilarity",
      sub = "Warren's I")

##### Stack SDMs from clusters #####

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)

rs1 = lapply(rastlist, raster)
rs1 = stack(rs1)

stack1 <- subset(rs1, c(37,16,27))

stack2 <- subset(rs1, c(19,15,4))

stack3 <- subset(rs1, c(22,6,32,1,35,17,18,2))

stack4a<- subset(rs1, c(36,33,9,8,28))
stack4b<- subset(rs1, c(39,23))

stack5 <- subset(rs1, c(30,20,14))

stack6a <- subset(rs1, c(29,24,10,26,5,21,25))
stack6b <- subset(rs1, c(40,31))

stack7a <- subset(rs1, c(13,7,11,41,38,12))
stack7b <- subset(rs1, c(34,3))


#create list of cluster stacks
clusterstacklist <- list(stack1, stack2, stack3, stack4a, stack4b, stack5,
                         stack6a, stack6b, stack7a, stack7b, st_allmodels)

##Average over probabilities in Stacked SDMs
predictlist=list()
for (i in 1:length(clusterstacklist)){
    predictlist[[i]] = mean(clusterstacklist[[i]])
}

#add names to list of prediction map rasters
predictions = stack(predictlist)
biotope_names <- c("Biotope 1",
					"Biotope 2",
					"Biotope 3",
					"Biotope 4A",
					"Biotope 4B",
					"Biotope 5",
					"Biotope 6A",
					"Biotope 6B",
					"Biotope 7A",
					"Biotope 7B",
					"Biotope AllModels")

#add names to raster stack of prediction map rasters
names(predictions) <- biotope_names

#write prediction map rasters to file
writeRaster(predictions, file.path(choose.dir(), names(predictions)), bylayer=T, format="GTiff", overwrite=T)

#quick plot of predictions and value distributions
for (i in 1:length(predictlist)) {
  hist(predictlist[[i]][predictlist[[i]] > 0], main = paste("Prediction ", i),
       xlab = "Suitability", ylab = "Frequency")
}


for (i in 1:length(predictlist)){
  plot(predictlist[[i]], main = paste("Prediction ", i))
}


##### Reclassification matrix #####
# 1 = Very low
# 2 = Low
# 3 = Moderate
# 4 = High
# 5 = Very high

classes<-matrix(c(0,0.2,1,
                  0.2,0.4,2,
                  0.4,0.6,3,
                  0.6,0.8,4,
                  0.8,1,5), ncol=3, byrow=TRUE)

###Reclassify rasters###
reclass_list = list()
for (i in 1:length(rs1)){
  reclass_list[i]<-raster::reclassify(x=rs1[[i]], rcl=classes, include.lowest=TRUE)
}

reclass_list <- stack(reclass_list)
writeRaster(reclass_list, filename = file.choose(),
            bylayer=T, format="GTiff", overwrite=T)


##### Summary statistics of prediction rasters####
predictbrick <- brick(predictlist)
mean <- cellStats(predictbrick, mean, na.rm=T)
max <- cellStats(predictbrick, max, na.rm=T)
min <- cellStats(predictbrick, min, na.rm=T)
median <- cellStats(predictbrick, median, na.rm=T)
summarystatsdf <- as.data.frame(list(mean, min, max))
names(summarystatsdf)[1] <- "Mean"
names(summarystatsdf)[2] <- "Max"
names(summarystatsdf)[3] <- "Min"


##### robert testing #####
## Step 1: Read the existing TIFF file and get its CRS
tiff <- raster("C:/rworking/deepseatools/indata/prediction_AllModels.tiff")
x <- crs(tiff)

## Step 2: Read another TIFF file
## This would be the path to one of your classified outputs from the script you sent
another_tiff <- raster("C:/path/to/your/another_file.tiff")

## Step 3: Assign the CRS from the first TIFF file to the second TIFF file
crs(another_tiff) <- x

## Step 4: Optional - Save the newly projected TIFF file
writeRaster(another_tiff, "C:/path/to/your/another_file_with_new_projection.tiff")




