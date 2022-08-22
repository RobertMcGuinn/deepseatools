##### Header #####
## Author: Robert McGuinn
## Started on: 20220727
## Purpose: WoRMS API calls.
## Forked from: https://www.marinespecies.org/aphia/webservice/Aphia_webservice_R_elaborate.txt

#install the required packages (comment if needed)
install.packages("jsonlite", repos="http://cran.r-project.org")
install.packages("httr")

#Use the libraries
library(jsonlite) #https://cran.r-project.org/web/packages/jsonlite/
library(httr)

#An vector of names we wan't to match (can be one item only!)
namesToMatch <- c("Solea soleo", "Abra Albo", "sdqkljflkfjqsmlfds")

#Convert the namesToMatch to a valid REST-url
urlNamesPart <- ""
for (index in 1:length(namesToMatch)) {
  urlNamesPart <- sprintf("%s&scientificnames[]=%s", urlNamesPart, namesToMatch[index]);
}

#The url can contain special characters that need to be converted
urlNamesPart <- URLencode(urlNamesPart)

#The dyanmic build of the URL causes an obsolete '&' at the beginning of the string, so remove it
urlNamesPart <- substring(urlNamesPart, 2)

#Build the final REST-url
url <- sprintf("http://www.marinespecies.org/rest/AphiaRecordsByMatchNames?%s", urlNamesPart);

#Get the actual data from the URL
matches <- fromJSON(url)

#Handle the data (each requested name has an list of results)
for (matchesindex in 1:length(namesToMatch)) {
  #Get the results for the current index
  currentResultList = matches[[matchesindex]]

  #Get the number of list entries for the first column
  numberOfResults <- length(currentResultList[[1]])

  #Handle empty data due to no matches found
  if (is.na(currentResultList[[1]][[1]])) {
    numberOfResults <- 0
  }
  print(sprintf("%d Result(s) found for %s", numberOfResults, namesToMatch[matchesindex]))
  if (numberOfResults > 0) {
    for (listentry in 1:numberOfResults) {
      print(sprintf("ID: %d, SCIENTIFICNAME: %s, MATCH_TYPE: %s",
                    currentResultList[["AphiaID"]][listentry],
                    currentResultList[["scientificname"]][listentry],
                    currentResultList[["match_type"]][listentry]
      ));
    }
  }
}
