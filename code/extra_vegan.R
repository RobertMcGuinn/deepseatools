##### EXTRA #####

##### basic ggplot when using DepthCat as the group ##### 
ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values = 
                        c('very shallow' = 'blue', 
                          'mesophotic' = 'green', 
                          'deep' = 'red',
                          'very deep' = 'pink')) +
  coord_equal() +
  theme_bw()

##### basic ggplot when using Ecoregion as the group ##### 
ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=site.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values = 
                        c('Floridian' = 'blue', 
                          'North Carolinean' = 'green', 
                          'South Carolinean' = 'red')) +
  coord_equal() +
  theme_bw()


##### basic visualizations of metaMDS plots ##### 
# set the cut tree
cl <- cutree(caver, 9)
stressplot(NMDS)
plot(NMDS)
ordiplot(NMDS)
ordihull(NMDS, cl, lty=6, label = TRUE)
ordispider(NMDS, cl, col="blue", label=TRUE)
ordiellipse(NMDS, cl, col="blue")
orditorp(NMDS,display="sites",cex=1,air=.09)
orditorp(NMDS,display="species",col="red",air=3)

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
rect.hclust(ccom,5)

ccom <- hclust(d, method="complete")
plot(ccom, labels = NULL, 
     main = "Cluster dendrogram", sub = NULL, ylab = "Height")
rect.hclust(ccom,5)

caver <- hclust(d, method="aver")
plot(caver, hang = -1)
rect.hclust(caver,5)

##### calculations of Biodiversity #####
#Shannon’s index for each of the sites
divtable <- diversity(site.sp, index = "shannon")

# Counting the number of species per site
dummyVar <- site.sp > 0 
x<-rowSums(dummyVar)

# binding these two results together
y<-cbind(divtable, x)
View(y)

# output
setwd("C:/rworking/digs/outdata")
write.csv(y,"divtable.csv", row.names = T, quote = T)

##### Species Accumulation Curves #####
library(vegan)
spa <- specaccum(site.sp)
plot(spa) #plots the species accumulation curve and the confidence intervals for sites.
plot(spa, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue") #males a prettier plot

spi<-specaccum(site.sp, method="rarefaction")
plot(spi)

dim(site.sp)

##### Exploring ways of summarizing species #####
spc_pres<-apply(site.sp>0,2,sum)

plot(sort(spc_pres),
     main="Cumulative Distribution of Species Occurrences",
     xlab='Cumulative Count of Species',ylab='Number of Plots')

# log version of plot
plot(sort(spc_pres),log='y',
     main="Cumulative Distribution of Species Occurrences",
     xlab='Cumulative Count of Species',ylab='Number of Plots')


# load the data

##### Generating a summary by summary by site (row) #####
yo <- apply(site.sp, 1, sum)

# bottom, left, top and right
par(mar=c(20,5,5,5))
barplot(yo, las=2, 
        cex.names = 1, 
        main = "Number of Coral and Sponge Observations by Marine Eco-Region")

title(ylab = "Number of Observations", 
      cex = 1.5,
      line = 4)

##### Dune Example #####
data(dune)
data(dune.env)
str(dune)

### Example 2: Constrained ordination (RDA)
## The example uses formula interface to define the model
data(dune)
data(dune.env)
## No constraints: PCA
mod0 <- rda(dune ~ 1, dune.env)
mod0
plot(mod0)
## All environmental variables: Full model
mod1 <- rda(dune ~ ., dune.env)
mod1
plot(mod1)
## Automatic selection of variables by permutation P-values
mod <- ordistep(mod0, scope=formula(mod1))
mod
plot(mod)
## Permutation test for all variables
anova(mod)
## Permutation test of "type III" effects, or significance when a term
## is added to the model after all other terms
anova(mod, by = "margin")
## Plot only sample plots, use different symbols and draw SD ellipses 
## for Managemenet classes
plot(mod, display = "sites", type = "n")
with(dune.env, points(mod, disp = "si", pch = as.numeric(Management)))
with(dune.env, legend("topleft", levels(Management), pch = 1:4,
                      title = "Management"))
with(dune.env, ordiellipse(mod, Management, label = TRUE))
## add fitted surface of diversity to the model
ordisurf(mod, diversity(dune), add = TRUE)
### Example 3: analysis of dissimilarites a.k.a. non-parametric
### permutational anova
adonis(dune ~ ., dune.env)
adonis(dune ~ Management + Moisture, dune.env)

# Function metaMDS would be an easier alternative.
data(dune)
dune.dis <- vegdist(wisconsin(dune))
library(MASS)
dune.mds <- metaMDS(dune.dis)

dune.mds$species <- wascores(dune.mds$points, dune, expand = TRUE)
fig <- ordiplot(dune.mds, type = "none")
points(fig, "sites", pch=21, col="red", bg="yellow")
text(fig, "species", col="blue", cex=0.9)
# Default plot of the previous using identify to label selected points
## Not run: 
fig <- ordiplot(dune.mds)
identify(fig, "spec")
## End(Not run)

##### xy plotting ##### 
library(lattice)
y <- as.numeric(sub$DepthInMeters)
x <- as.numeric(sub$Temperature)

qplot(as.numeric(x), 
      as.numeric(y), 
      color = factor(gisMEOW), 
      data = sub)

xyplot(x ~ y | gisMEOW, sub,
       group = factor(gisMEOW),
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       auto.key = FALSE,
       type = c("p", "smooth"), lwd = 1)

## groups + type "smooth"
xyplot(y ~ x, sub,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       group = factor(gisMEOW), auto.key = TRUE,
       type = c("p", "smooth"), lwd = 1)

## making key more compact
xyplot(lifeExp ~ gdpPercap, jDat,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       group = continent, auto.key = list(columns = nlevels(jDat$continent)),
       type = c("p", "smooth"), lwd = 4)

# Simple Scatterplot
attach(sub)
plot(as.numeric(sub$DepthInMeters), 
     as.numeric(sub$Temperature),
     main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
# Add fit lines
lines(lowess(as.numeric(sub$DepthInMeters),as.numeric(sub$Temperature)), col="blue") # lowess line (x,y)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
#To see it in action:

set.seed(1)

y <- remove_outliers(as.numeric(Temperature))




# Pull map from Google (e.g., location = c('Sunappe, NH') or specified lat/long (as below)
map <- get_map(location = c(lon = as.numeric(sub$Longitude), lat = 35.05), zoom = 14, maptype = c('hybrid'))

# Plot map- 2013 Q
ggmap(map)
+ geom_point(aes(x = lat, y = long, colour=Q, size=Q), data = data)
+ scale_colour_continuous(name='Discharge (L/s)',limits=c(0,1000), low = "yellow", high = "red", space = "Lab", guide = "colorbar")
+ scale_size(guide='none', range= c(2,12), limits=c(0,1000), breaks=c(5, 50, 100, 400, 800))

##### Reading in hexagons to use as regular "sites" #####
setwd("C:/data/BaseLayers")
hex <- readOGR("./Hexagons", "hex2")
hex<-spTransform(hex, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# creating SpatialPointsDataFrame from the subset. 
coords<-sub[,c("Longitude.x", "Latitude.x")]
coords$Latitude<-as.numeric(coords$Latitude.x)
coords$Longitude<-as.numeric(coords$Longitude.x)
spdf<-SpatialPointsDataFrame(coords, sub, 
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                             match.ID = TRUE)

# mapping hexagons and points together
plot(hex %in% list)
points(spdf, pch=20, cex=0.2, col = "red")

# intersection of sub and hex
z<-over(spdf, hex)
sub$hex <- z$GRID_ID
list<-unique(sub$hex)
setwd("C:/rworking/digs/outdata")
write.csv(sub,"sub.csv", row.names = T, quote = T)


##### BCI example #####
# from: BIO 377  Lab Exercise: Vegetation Data and Diversity
data(BCI)	#loads BCI data
dim(BCI)	#gives you the dimensions of the data set, (rows, columns)
BCI[1:10,20:25]	#shows the data for rows 1:10 and columns 20:25
BCI[1:50, 71]	#addresses the row and column of the data set
BCI[,71]	#R takes no value for rows to mean "show them all"
BCI$Faramea.occidentalis	#you can also use the dataset$column address form
sum(BCI$Faramea.occidentalis)	#adds up all the individuals 
plots <- diversity(BCI, index = "shannon") #makes an object 
#that the diversity values #are written to.
summary(plots) #gives summary statistics for the plots
median(plots) #gives the median
mean(plots) #gives the mean
class(plots)
max(plots[1:30])









