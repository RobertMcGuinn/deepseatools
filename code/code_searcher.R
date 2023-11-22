##### find file #####
## manual: edit string for x
x <- 'header'
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

##### clean up everything except filt ######
rm(list=setdiff(ls(), c("filt")))
## rm(filt)












