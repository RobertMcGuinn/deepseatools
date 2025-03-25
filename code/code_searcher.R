##### find file #####
## manual: edit string for x
x <- '20240320-0_rmd_accession_qa_dashboard.Rmd'
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

##### find out the time files were edited #####
## Get file information
file_info <- lapply(files, file.info)

## Extract modification time
modification_times <- sapply(file_info, function(info) info$mtime)

## Combine filenames with modification times
file_info_df <- data.frame(File = basename(files), Modification_Time = modification_times)

## Print the result
print(file_info_df) ## files having a larger Modification_Time were created later in time.

##### clean up everything except filt ######
rm(list=setdiff(ls(), c("filt")))











