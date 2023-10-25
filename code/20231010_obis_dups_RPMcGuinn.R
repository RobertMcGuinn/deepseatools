##### checking ######
x <- filt %>% filter(grepl('Smithsonian', DataProvider)) %>% head()
y <- filt %>% filter(grepl('MCZ', DatasetID)) %>% head()
z <- filt %>% filter(grepl('NOAA_SH-22-09', DatasetID))  %>% head()
yo <- rbind(x, y)
yo <- rbind(yo,z)
write.csv(yo, "c:/rworking/deepseatools/indata/20231010-0_export_for_dup_check_RPMcGuinn.csv")

