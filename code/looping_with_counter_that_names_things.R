##### Header #####
## Original author: https://stackoverflow.com/users/645206/bdemarest
## Code page: https://stackoverflow.com/questions/26034177/save-multiple-ggplots-using-a-for-loop
## Forked by: Robert P. McGuinn
## Forked on: 20200302

##### Plot separate ggplot figures in a loop. #####
library(ggplot2)

##### Make list of variable names to loop over.#####
var_list = combn(names(iris)[1:3], 2, simplify=FALSE)

##### create an empty list #####
plot_list = list()

##### run the loop and store outputs in list at different list positions #####
for (i in 1:3) {
  p <-  ggplot(iris, aes_string(x=var_list[[i]][1], y=var_list[[i]][2])) +
    geom_point(size=3, aes(colour=Species))
  plot_list[[i]] <-  p
}

##### output: Makes a separate file for each plot(*.png). #####
for (i in 1:3) {
  file_name = paste("iris_plot_", i, ".png", sep="")
  png(file_name)
  print(plot_list[[i]])
  dev.off()
}

# Another option: create pdf where each page is a separate plot.
pdf("plots.pdf")
for (i in 1:3) {
  print(plot_list[[i]])
}
dev.off()
