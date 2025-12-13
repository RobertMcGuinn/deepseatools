#DSC SMITHSONIAN API INTEGRATION HOME SCRIPT

#Install devtools, if needed
#install.packages("devtools")

#Install package from Github, with vignettes
library(devtools)
#install_github("Smithsonian/EDANr", build_vignettes = TRUE)

#install.packages(c("httr", "uuid", "stringr", "jsonlite", "digest", "openssl"))
library("httr")
library( "uuid")
library( "stringr")
library( "jsonlite")
library( "digest")
library("openssl")
library("EDANr")


#' Prepares the query to the EDAN API
#'
#' @return Result of the GET request
#'
#' @param AppID AppID used for authentication
#' @param AppKey Key for the AppID used for authentication
#' @param QueryParameters query params
#' @param api_url URL of the route
#' @param rows Number of rows to return, max is 100.
#' @param start Start number, to use with rows
#'
#' @importFrom httr GET
#' @importFrom uuid UUIDgenerate
#' @importFrom stringr str_replace_all
#' @importFrom jsonlite fromJSON
#' @importFrom digest digest
#' @importFrom openssl base64_encode
#' @importFrom httr add_headers
#' @importFrom httr content

#'
queryAPI <- function(AppID = NA, AppKey = NA, QueryParameters = NA, api_url = NA, rows = 10, start = 0){

  if (rows > 100){
    warning("The number of rows has been set to the maximum allowed of 100")
    rows <- 100
  }

  if (is.na(AppID) || AppID == ""){
    stop("Error: AppID can not be empty.")
  }

  if (is.na(AppKey) || AppKey == ""){
    stop("Error: AppKey can not be empty.")
  }

  #Date of request
  RequestDate <- as.character(Sys.time())

  #Generated uniquely for this request
  Nonce <- substr(stringr::str_replace_all(uuid::UUIDgenerate(FALSE), "-", ""), 0, 24)

  #This will be the value of X-AuthContent, each element is joined by a single newline
  StringToSign <- paste(Nonce, QueryParameters, RequestDate, AppKey, sep = "\n")

  #First hash using SHA1
  HashedString <- digest::digest(StringToSign, algo="sha1", serialize=F)

  #Base64 encode
  EncodedString <- openssl::base64_encode(bin = HashedString)

  API_url <- 'https://edan.si.edu/'
  api_url_q <- paste0(API_url, api_url)

  api_answer <- httr::GET(url = api_url_q,
                          httr::add_headers("X-AppId" = AppID,
                                            "X-Nonce"= Nonce,
                                            "X-RequestDate"= RequestDate,
                                            "X-AuthContent"= EncodedString)
  )

  return(api_answer)
}

api_key="aY73bqSeEdYLQmZzOgFRLN94pcfX1VrEHItg5MSO"

base_url<-"https://api.si.edu/openaccess/api/v1.0/search"

smithsonian_search <- function(q,
                               rows = 100,
                               max_records = 25,
                               verbose = TRUE) {
  if (api_key == "") {
    stop("No API key found. Set SMITHSONIAN_API_KEY in your .Renviron first.")
  }

  start <- 0
  all_batches <- list()
  total_fetched <- 0

  repeat {
    if (verbose) {
      message("Fetching records ", start + 1, " to ", start + rows, " ...")
    }

    resp <- GET(
      url   = base_url,
      query = list(
        q       = q,
        start   = start,
        rows    = rows,
        api_key = api_key
      )
    )

    stop_for_status(resp)

    txt  <- content(resp, as = "text", encoding = "UTF-8")
    json <- fromJSON(txt, flatten = TRUE)

    # >>> IMPORTANT: adjust this line if your JSON structure differs <<<
    batch <- json$response$rows

    if (is.null(batch) || length(batch) == 0) {
      if (verbose) message("No more records returned, stopping.")
      break
    }

    # Make sure this is a data.frame
    batch_df <- as.data.frame(batch)

    all_batches[[length(all_batches) + 1]] <- batch_df
    n_this <- nrow(batch_df)
    total_fetched <- total_fetched + n_this

    if (verbose) message("Retrieved ", n_this, " records (total so far: ", total_fetched, ").")

    if (n_this < rows) {
      if (verbose) message("Last page was partial, assuming end of results.")
      break
    }

    if (total_fetched >= max_records) {
      warning("Reached max_records limit (", max_records, "), stopping early.")
      break
    }

    start <- start + rows
  }

  if (length(all_batches) == 0) {
    if (verbose) message("No records found for query: ", q)
    return(dplyr::tibble())
  }

  dplyr::bind_rows(all_batches)
}

query <- paste(
  'unit_code:NMNHINV',
  "AND (coral OR corals OR sponge OR sponges OR cnidaria OR porifera)",
  sep=" "
)

query <- paste(
  'unit_code:NMNHINV',
  "AND (content.indexedStructured.tax_phylum:Porifera)",
  sep=" "
)

coralnspongesdb<-smithsonian_search(
  q=query,
  rows=100
)
