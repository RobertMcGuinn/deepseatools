##### find file #####
## manual: edit string for x
x <- 'rmd_datasetid'
path <- 'C:/rworking/deepseatools/code'
files<-list.files(path,
                  pattern=x,
                  full.names=TRUE)

## look at the list
files

##### choose and open #####
## manual input required: pick the number or number you want from the list presented
y <- c(2,3,5,6,8,9,12,13,14)
path <- files[y]
file.edit(path)


###### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))

