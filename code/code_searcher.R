##### find file #####
x <- 'dataset'
path <- 'C:/rworking/deepseatools/code'
list<-list.files(path,
                 pattern=x,
                 full.names=TRUE)
list

##### choose and open #####
y <- 6 # manual input required: pick the number you want from the list presented
path <- list[y]
file.edit(path)


