##### packages #####
library(tidyverse)
##### find file #####
## manual: edit string for x
x <- 'dst_'
path <- 'C:/rworking/deepseatools/code'
files<-list.files(path,
                  pattern=x,
                  full.names=TRUE)

## look at the list
files

##### choose and open #####
## manual input required: pick the number
## or number you want from the list presented
y <- c(15)
this <- files[y]
file.edit(this)

##### source chosen file #####
source(this)

##### delete a file (one at a time) #####
delete_file <- function(pattern, path) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (!length(files)) return(invisible(NULL))

  i <- menu(files, title = "Pick file to delete")
  if (i == 0) return(invisible(NULL))

  file.remove(files[i])
}
delete_file(x, path)

##### find out the time files were edited #####
## Get file information
file_info <- lapply(files, file.info)

## Extract modification time
modification_times <- sapply(file_info, function(info) info$mtime)

## Combine filenames with modification times
file_info_df <- data.frame(File = basename(files), Modification_Time = modification_times)

## arrange
file_info_df <- file_info_df %>% arrange(Modification_Time)

## Print the result
print(file_info_df) ## files having a larger Modification_Time were created later in time.

##### tool: paste to get the file name of the currently open file on the clipboard #####
writeLines(rstudioapi::getSourceEditorContext()$path, "clipboard")

##### clean up everything except filt ######

##### section: clean up selectively  #####
## list of all objects
all_objects <- ls()

## set what you want to keep
to_keep <- "filt"

## remove unwanted objects
rm(list = setdiff(all_objects, to_keep))

## garbage collection
gc()







