##### find file #####
x <- "runner"
list<-list.files('C:/rworking/deepseatools/code',
                 pattern=x,
                 full.names=TRUE)

list

##### choose and open #####
y <- 5
path <- list[y]
file.edit(path)


