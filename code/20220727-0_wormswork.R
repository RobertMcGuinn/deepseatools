##### Header #####
## Author: Robert McGuinn
## Started on: 20220727
## Purpose: WoRMS API calls.
## Forked from: https://www.marinespecies.org/aphia/webservice/Aphia_webservice_R_elaborate.txt

#### install the required packages (comment if needed) #####
# install.packages("jsonlite", repos="http://cran.r-project.org")
# install.packages("httr")

#### Use the libraries #####
library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
library(httr)
library(magrittr)

##### Create a character vector of names manually #####
namesToMatch <- c('Madracis')

##### Convert the namesToMatch to a valid REST-url #####
urlNamesPart <- ""
for (index in 1:length(namesToMatch)) {
  urlNamesPart <- sprintf("%s&scientificnames[]=%s", urlNamesPart, namesToMatch[index]);
}

##### The url can contain special characters that need to be converted #####
urlNamesPart <- URLencode(urlNamesPart)

##### The dyanmic build of the URL causes an obsolete '&' at the beginning of the string, so remove it #####
urlNamesPart <- substring(urlNamesPart, 2)

##### Build the final REST-url #####
url <- sprintf("http://www.marinespecies.org/rest/AphiaRecordsByMatchNames?%s", urlNamesPart);

##### Get the data from the URL #####
matches <- url %>%
  httr::GET() %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON()

##### Create a dataframe from the matches #####
df <- do.call(rbind,
              lapply(matches,
                     as.data.frame))
View(df)

