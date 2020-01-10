##### Header #####
# author: Robert P. McGuinn, rpm@alumni.duke.edu
# purpose: reading, writing, and executing batch files from R. This example here is for executing
#   commands to work with VLC.

##### get annotation video run time stamps via Excel #####
setwd("C:/rworking/deepseatools/indata")
x <- read.xlsx('annotations.xlsx', sheet = 1)

##### build batch file for commands #####
x$cmd <- paste('vlc ', x$vidpath,' ',
               '--video-filter=scene --start-time=',
               x$start, ' ', '--stop-time=', x$stop, ' ', '--scene-ratio=', x$fps, ' ',
               '--scene-path=', x$stillpath, ' ', 'vlc://quit', sep = '')

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


