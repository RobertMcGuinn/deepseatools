##### packages #####
library(tidyverse)
##### find file #####
## manual: edit string for x
x <- 'dst_tool_get_OCIS_H3_from_ArcGIS_service'
path <- 'C:/rworking/deepseatools/code'
files<-list.files(path,
                  pattern=x,
                  full.names=TRUE)

## look at the list
files

##### choose and open #####
## manual input required: pick the number
## or number you want from the list presented
y <- c(1)
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
rm(all_objects)
rm(to_keep)
gc()

##### search within code #####
x <- 'H3'
find_in_files <- function(search_term, dir_path = "code/", file_pattern = "\\.(R|Rmd)$", ignore_case = FALSE) {

  # 1. Get a list of all files in the directory (and subdirectories) matching the pattern
  files <- list.files(path = dir_path, pattern = file_pattern, recursive = TRUE, full.names = TRUE)

  results_list <- list()

  # 2. Loop through each file and search for the string
  for (file in files) {
    # Read the file contents (warn=FALSE suppresses warnings about missing end-of-line characters)
    lines <- readLines(file, warn = FALSE)

    # Find the line numbers that contain the search term
    matches <- grep(search_term, lines, ignore.case = ignore_case)

    # 3. If there's a match, save the details
    if (length(matches) > 0) {
      results_list[[file]] <- data.frame(
        File = file,
        LineNumber = matches,
        CodeSnippet = trimws(lines[matches]), # trimws() removes extra leading/trailing spaces
        stringsAsFactors = FALSE
      )
    }
  }

  # 4. Bind it all into one clean data frame
  if (length(results_list) > 0) {
    final_results <- do.call(rbind, results_list)
    rownames(final_results) <- NULL
    return(final_results)
  } else {
    message("No matches found.")
    return(invisible(NULL))
  }
}
View(find_in_files(x))
