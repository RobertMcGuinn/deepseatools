#EDAN creds
AppID = "vaY73bqSeEdYLQmZzOgFRLN94pcfX1VrEHItg5MS"
AppKey = "vaY73bqSeEdYLQmZzOgFRLN94pcfX1VrEHItg5MS"

edan <- connectEDAN(AppID, AppKey)

#Required packages
library("EDANr")
library("magick")
library("stringr")

#Create folders to store images
#Horizontal images
dir.create("reports/images_h", showWarnings = FALSE)
#Vertical images
dir.create("reports/images_v", showWarnings = FALSE)


#EDAN query for orchids of Smithsonian Gardens with images
orchids_query <- "orchid smithsonian gardens&fq=online_media_type:\"Images\""
orchids_fqs <- "online_media_type:\"Images\""


#Get number of results in EDAN
results <- EDANr::edan_metadata_search(query = orchids_query,
                                       fqs = orchids_fqs,
                                       AppID = AppID,
                                       AppKey = AppKey,
                                       rows = 1,
                                       start = 0)
results_count <- results$rowCount

#Calculate the number of steps needed to get all the results,
# in steps of 100 rows each, the maximum the API returns
steps <- floor(results_count/100)
