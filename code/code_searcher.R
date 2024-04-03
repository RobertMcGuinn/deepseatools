##### find file #####
## manual: edit string for x
x <- '20230828-0_rmd_quarterly_report_for_database_update'
path <- 'C:/rworking/deepseatools/code'
files<-list.files(path,
                  pattern=x,
                  full.names=TRUE)

## look at the list
files


##### choose and open #####
## manual input required: pick the number or number you want from the list presented
y <- c(1)
path <- files[y]
file.edit(path)

##### source chosen file #####
source(path)

##### clean up everything except filt ######
rm(list=setdiff(ls(), c("filt")))















