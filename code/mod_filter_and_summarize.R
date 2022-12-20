##### header #####
## author: Robert P. McGuinn | rpm@alumni.duke.edu
## purpose: modular: apply a filter and summarize

##### packages #####
library(tidyverse)


filt %>%
  filter(grepl("KOK 05-11", SurveyID)) %>%
  #filter(CatalogNumber == "538350") %>%
           group_by(DatasetID,
                    SurveyID,
                    Locality,
                    ObservationYear,
                    ObservationDate) %>%
           summarize(n=n()) %>%
           View()

