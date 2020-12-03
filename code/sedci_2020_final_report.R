##### Header #####
# author: Robert P. McGuinn
# started on: 20201125
# purpose: Working on the SEDCI final report 2020

##### Getting new corals and sponges #####
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20201021-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

## cleaning
rm(indata)

##### Create a not-in function #####
`%notin%` <- Negate(`%in%`)

##### get all the DatasetIDs in SEDCI region #####
ecogig <- c("NOAA_EX-12-02-L2",
            "BOEM_Lophelia_II",
            "WHOI_AT-18",
            "SOI_FK006B",
            "NOAA_HC-11-10",
            "OET_NA028",
            "OET_NA043",
            "OET_NA057",
            "OET_NA058",
            "ECOGIG_OI-16-10",
            "ECOGIG_O2-17-06")

ecogig <- unique(ecogig)

## DatasetIDs in the SEDCI rescue project (as of 2020-10-19)
sedci_rescue <- c("HBOI_SJ-10-07", "HBOI_SJ-10-07", "NOAA_SJ-09-08", "NOAA_SJ-09-08","HBOI_SJ-08-09","NOAA_SJ-07-05", "NOAA_SJ-07-05", "NOAA_SJ-07-06", "HBOI_SJ-06-02", "HBOI_SJ-06-05-L1", "HBOI_SJ-06-05-L3", "HBOI_SJ-06-02", "NOAA_SJ-05-04", "NOAA_SJ-05-08", "NOAA_SJ-05-10", "NOAA_SJ-05-11", "NOAA_SJ-04-05", "NOAA_SJ-02-08", "HBOI_EL-99-08", "HBOI_SJ-88-11", "USGS_TM-97-10", "NOAA_RB-03-07", "NOAA_SJ-05-11",
                  "NOAA_NF-09-01", "NOAA_NF-08-04", "NOAA_NF-07-06", "NOAA_NF-05-05", "NOAA_SJ-07-06", "HBOI_SJ-08-09", "NOAA_SJ-09-08", "NOAA_PC-10-02",
                  "NOAA_NF-14-01", "NOAA_NF-13-02", "NOAA_NF-12-01", "NOAA_NF-11-09", "NOAA_NF-11-09-L3", "NOAA_PC-11-05-L1", "NOAA_NF-11-09", "NOAA_NF-11-09-L3", "Sharuga_SM_2014", "NOAA_PC-12-03", "NOAA_NF-13-02", "NOAA_PC-13-03", "NOAA_NF-14-08","NOAA_PC-15-02", "NOAA_PC-16-02", "OET_NA058", "OET_NA028", "NOAA_PC-17-02", "HBOI_WS-17-125")

## unique DatasetIDs
sedci_rescue <- unique(sedci_rescue)

## sedci_rescue add ecogig
sedci_rescue <- union(sedci_rescue, ecogig)

## datasetIDs in the SEDCI rescue project (as of 2020-11-25)
sedci_new <- c("NOAA_DFH-30", "NOAA_NF-17-08",
               "NOAA_NF-17-08", "NOAA_DFH-32-33",
               "NOAA_DFH-32-33", "NOAA_EX-17-11",
               "NOAA_EX-18-03", "NOAA_EX-18-06",
               "NOAA_NF-18-04","NOAA_FGBNMS_DFH-35",
               "NOAA_FGBNMS_DFH-37", "NOAA_RESTORE_MT18")

sedci_new <- unique(sedci_new)

##### create geographic filter for just the SEDCI region Fish Coucils. #####
x <- filt %>% filter(FishCouncilRegion == "Gulf of Mexico" |
                       FishCouncilRegion == "South Atlantic" |
                       FishCouncilRegion == "Caribbean")

##### build a variable for new vs. rescue datasets #####
x$sedci <-
  case_when(
    x$DatasetID %in% sedci_rescue ~ "rescue" ,
    x$DatasetID %in% sedci_new ~ "new",
  )

#length(x$Flag)
#table(x$sedci, useNA = 'always')

##### build a variable for pre-SEDCI and post-SEDCI
x$sedci2 <-
  case_when(
    x$DatasetID %in% sedci_rescue ~ "Post SEDCI" ,
    x$DatasetID %in% sedci_new ~ "Post SEDCI",
    x$DatasetID %notin% sedci_rescue ~ "Pre SEDCI",
    x$DatasetID %notin% sedci_new ~ "Pre SEDCI"
  )

##### order the levels properly #####
x$sedci2 <- factor(x$sedci2, levels = c('Post SEDCI', 'Pre SEDCI'))

## check
# x %>% pull(CatalogNumber) %>% length()


# filt %>%
#   filter(grepl("EX", DatasetID),
#          is.na(ImageURL) == F#,
#          #ObservationDate == '-999'
#          ) %>%
#   pull(DatasetID) %>%
#   unique() %>%
#   length()

##### make separate old and new files #####
filt_new_only <- x %>% filter(sedci2 == 'Post SEDCI')
filt_old <- x %>% filter(sedci2 == "Pre SEDCI")

##### creating just the xy's #####
old <- filt_old %>% dplyr::select(CatalogNumber, Latitude, Longitude)
new <- filt_new_only %>% dplyr::select(CatalogNumber, Latitude, Longitude)

##### install packages #####

library(arcgisbinding)
arc.check_product()

##### create spdf #####
old_geo <- old
coordinates(old_geo) <- c("Longitude", "Latitude")
proj4string(old_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

new_geo <- new
coordinates(new_geo) <- c("Longitude", "Latitude")
proj4string(new_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####
fgdb_path <- 'C:/Users/Robert.McGuinn/Documents/ArcGIS/Packages/RTC2020_0d88f3/p20/rtc2020.gdb'
arc.write(file.path(fgdb_path, 'old_geo'), data=old_geo, overwrite = TRUE)
arc.write(file.path(fgdb_path, 'new_geo'), data=new_geo, overwrite = TRUE)
