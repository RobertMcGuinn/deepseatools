##### packages #####
library(tidyverse)
##### find file #####
## manual: edit string for x
x <- 'dst_tool'
path <- 'C:/rworking/deepseatools/code'
files<-list.files(path,
                  pattern=x,
                  full.names=TRUE)

## look at the list
files

##### choose and open #####
## manual input required: pick the number
## or number you want from the list presented
y <- c(5)
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
# rm(list=setdiff(ls(), c("filt")))

### Function to clear environment except for specific objects
keep_only <- function(keep_list) {
  # 1. Get all objects in the Global Environment
  all_vars <- ls(envir = .GlobalEnv)

  # 2. Identify which ones are NOT in our keep list
  # We also make sure the function itself doesn't try to delete itself while running
  to_delete <- setdiff(all_vars, c(keep_list, "keep_only"))

  # 3. Remove them explicitly from the Global Environment
  rm(list = to_delete, envir = .GlobalEnv)

  message(paste("Environment cleared. Kept:", paste(keep_list, collapse = ", ")))
}

### How to use it:
keep_only("filt")
# keep_only(c("filt", "my_data", "model_results"))








