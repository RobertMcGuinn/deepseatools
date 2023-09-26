##### find file #####
## manual: edit string for x
x <- 'taxonomy'
path <- 'C:/rworking/deepseatools/code'
files<-list.files(path,
                  pattern=x,
                  full.names=TRUE)

## look at the list
files

##### choose and open #####
## manual input required: pick the number or number you want from the list presented
y <- c(5)
path <- files[y]
file.edit(path)

##### clean up everything except core objects ######
rm(list=setdiff(ls(), c("filt")))



