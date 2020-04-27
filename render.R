##### Header #####
# Author: Robert P. McGuinn
# Started on: 20190516
# Purpose: A runner for the rendering the book

#install.packages('bookdown')
library(bookdown)
setwd('c:/rworking/deepseatools')
#rmarkdown::render_site(encoding = 'UTF-8')
bookdown::render_book("index.Rmd", output_dir = 'docs')
