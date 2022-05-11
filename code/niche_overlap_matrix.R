##### Header #####
## purpose: exploration of nich overlap stuff
## author: Robert McGuinn
## date started: 20220504

##### packages #####
# install.packages("dismo")
library(dismo)

##### play with rasters #####
## build rasters
r1 <- raster(nr=18, nc=36)
r2 <- raster(nr=18, nc=36)
r3 <- raster(nr=18, nc=36)

## populate rasters with random values
set.seed(0)
r1[] <- runif(ncell(r1))
r2[] <- runif(ncell(r1))
r3[] <- runif(ncell(r1))

## checking: visualize these randomly generated raster files
plot(r1)
plot(r2)
plot(r3)

## checking: explore the values within the raster
class(r1)
summary(r1)
str(r1)
ncells(r1)

## create a vector of the raster names
rasters<-list(r1,r2,r3)

## create an empty vector for use in a for
vec <- c()

for(i in seq_along(rasters)) {
  ov <- nicheOverlap(
    rasters[[1]],
    rasters[[i]],
    stat = "I",
    mask = FALSE,
    checkNegatives = FALSE
  )
  vec <- c(vec, ov)
}

##### different method to avoid the loop #####
## author: Robert McGuinn
## forked from: Nello Blaser: https://stat.ethz.ch/pipermail/r-help/2013-June/355792.html
## forked on date: 20220505

## packages
install.packages("irr")
library (irr)

## data example
c1 <- c(1,1,1,0.25,0,1,1,1,0,1)
c2 <- c(0,0,1,1,0,1,0,1,0.5,1)
c3 <- c(0,1,1,1,0,0.75,1,1,0.5,0)
x <- data.frame(c1,c2,c3)

## checking
View(x)
str(x)

## Here's a possible solution to avoid the loop
k <- as.matrix(expand.grid(1:ncol(x),1:ncol(x)))
a1 <- as.data.frame(matrix(sapply(1:nrow(k), function(n)
agree(x[,k[n,]])$value), nrow=ncol(x)))
colnames(a1) <- colnames(x)
rownames(a1) <- colnames(x)

## checking
class(k)

identical(a, a1)


## [1] TRUE

Or if you want to avoid double calculation,

a2 <- as.data.frame(matrix(0, nrow=ncol(x), ncol=ncol(x)))
colnames(a2) <- colnames(x)
rownames(a2) <- colnames(x)
k <- t(combn(1:ncol(x), 2))
a2[lower.tri(a2)] <- sapply(1:nrow(k), function(n)
agree(x[,k[n,]])$value)
a2 <- a2+diag(100,ncol(x))
a2[upper.tri(a2)] <- t(a2)[upper.tri(a2)]

> identical(a, a2)
[1] TRUE



