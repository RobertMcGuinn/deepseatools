##### Header #####
# original author: Elizabeth Gugliotti, elizabeth.gugliotti@noaa.gov
# date started: 2020-01-16
# forked by: Robert McGuinn, robert.mcguinn@noaa.gov
# date started: 2020-01-28
# purpose: Build input file for VLC capture image code 'bat.exec.R' in this repository

##### Read in file #####
test.time<-read.csv("test.time.csv", header=TRUE)

##### Because working with DateTimes is always SUPER fun #####
test.time$SDateTime<-paste(test.time$Date, test.time$StartTime, sep=" ")
test.time$SDateTime<-mdy_hms(test.time$SDateTime)

test.time$EDateTime<-paste(test.time$Date, test.time$EndTime, sep=" ")
test.time$EDateTime<-mdy_hms(test.time$EDateTime)


##### Must create a loop to get times #####
# Create empty data frame
test.df <- data.frame(Path = character(),
                      File = character(),
                      DateTime = as.POSIXct(character()),
                      RunTime=numeric(),
                      stringsAsFactors=FALSE)

##### Create the loop #####
for (id in test.time$Name){
  x<- seq.POSIXt(test.time$SDateTime[test.time$Name==id], test.time$EDateTime[test.time$Name==id], units="seconds", by= 1)
  f<-as.numeric(difftime(test.time$EDateTime[test.time$Name==id],test.time$SDateTim[test.time$Name==id],units="secs"))
  g<- rep(test.time$Path[test.time$Name==id],length(x))
  h<- rep(test.time$Name[test.time$Name==id],length(x))
  w<- seq(from = 0, to=f, by=1)
  test.d <- data.frame(Path = g,
                       File = h,
                       DateTime = x,
                       RunTime = w,
                       stringsAsFactors=FALSE)
  test.df <- rbind(test.df, test.d)
}


