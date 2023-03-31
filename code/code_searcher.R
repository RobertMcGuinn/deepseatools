##### find file #####
## manual: edit string
x <- 'worms'
path <- 'C:/rworking/deepseatools/code'
files<-list.files(path,
                  pattern=x,
                  full.names=TRUE)

## look at the list
files

##### choose and open #####

## manual input required: pick the number or number you want from the list presented
y <- c(2)
path <- files[y]
file.edit(path)
=
