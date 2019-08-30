##### 20190315-0: RPMcGuinn #####
##### summary query with Google Drive output #####

]x <- filt  %>%
  arrange(ObservationDate)  %>%
  filter(
    #is.na(RecordType) ==  T
    #DatasetID == "NOAA_LS-05-01",
    grepl('Stone, Robert', PI)
  ) %>%
  group_by(ObservationYear, DatasetID) %>% #Flag, FlagReason, CatalogNumber, SampleID
  summarise(
    n=n(),
    ScientificName = paste(unique(ScientificName), collapse=" | "),
    DataContact = paste(unique(DataContact), collapse=" | "),
    Reporter = paste(unique(Reporter), collapse=" | ")
  )

View(x)

##### write to CSV and upload to Google Drive  #####

setwd("C:/rworking/digs/indata")
x %>%
  write.csv("query.csv", row.names = FALSE)
xsheet <- gs_upload("query.csv")
gs_browse(xsheet, ws = 1)

