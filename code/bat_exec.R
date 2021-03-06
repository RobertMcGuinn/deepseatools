##### Header #####
# author: Robert P. McGuinn, rpm@alumni.duke.edu
# purpose: reading, writing, and creating .bat files from with R.
# * This example here is for executing
#   commands to work with VLC video annotation software.
# date_started: 2020-01-03

##### packages #####
library(openxlsx)

##### load annotation data: get annotation video run time stamps via Excel #####

setwd("C:/rworking/deepseatools/indata")
x <- read.xlsx('annotations.xlsx', sheet = 1)

##### build vlc screen capture commands for video stored on disk #####

x$cmd <- paste('vlc ', x$vidpath,' ',
               '--video-filter=scene --start-time=',
               x$start, ' ', '--stop-time=', x$stop, ' ', '--scene-ratio=', x$fps, ' ',
               '--scene-prefix=',x$annotationID,'_',x$start,'_',x$stop,'_',' ',
               '--scene-path=', x$stillpath, ' ', 'vlc://quit', sep = '')

##### ***OR*** build commands for videos stored on YouTube  #####

# # setwd("C:/rworking/deepseatools/indata")
# x <- read.xlsx('annotations_yt.xlsx', sheet = 1)
#
# ## change annotation data to 'annotations_yt.xlsx')
# setwd("C:/rworking/deepseatools/indata")
# x <- read.xlsx('annotations_yt.xlsx', sheet = 1)
# x$cmd <- paste('vlc ','-vvv',' ', x$vidpath,' ',
#                '--video-filter=scene --start-time=',
#                x$start, ' ', '--stop-time=', x$stop, ' ', '--scene-ratio=', x$fps, ' ',
#                '--scene-prefix=',x$annotationID,'_',x$start,'_',x$stop,'_',' ',
#                '--scene-path=', x$stillpath, ' ', 'vlc://quit', sep = '')

##### build batch file and save #####

bat <- x$cmd
bat <- data.frame(bat)
cd <- "cd C:\\Program Files\\VideoLAN\\VLC"
cd <- data.frame(cd)
names(cd) <- names(bat)
bat <- rbind(cd,bat)
write.table(bat, "C:/rworking/deepseatools/code/vlc_vidcap.bat",
            quote = F,
            row.names = F,
            col.names = F)

##### execute the batch file #####
shell.exec("C:/rworking/deepseatools/code/vlc_vidcap.bat")


