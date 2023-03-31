##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: apply a filter and summarize

##### packages #####
library(tidyverse)

##### variables #####
datasetID <- "KOK 05-11"

##### filter and summarize #####
filt %>%
  filter(grepl("2013", ObservationYear)) %>%
  #filter(CatalogNumber == "538350") %>%
           group_by(DatasetID,
                    ObservationYear,
                    DataContact) %>%
           summarize(n=n()) %>% View()

##### check #####
x <- filt %>%
  filter(DatasetID == "NOAA_CT-13-07",
         EventID == "Tow 5") %>%
  group_by(ImageFilePath, ScientificName) %>%
  summarize(n=n())
View(x)










