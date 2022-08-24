##### find file #####
x <- "obis"
list<-list.files('C:/rworking/deepseatools/code',
                 pattern=x,
                 full.names=TRUE)

list

##### choose and open #####
y <- 1
path <- list[y]
file.edit(path)


