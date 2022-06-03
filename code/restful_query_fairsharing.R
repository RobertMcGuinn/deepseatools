##### load packages #####
library(RCurl)
library(jsonlite)
library(tidyverse)
library(data.table)
library(httr)

##### get jwt #####
url<-'https://api.fairsharing.org/users/sign_in'
request<-POST(url,
              add_headers(
                "Content-Type"="application/json",
                "Accept"="application/json"),
              body="{\"user\": {\"login\":\"positivebob\",\"password\":\"1Asheville7!1970\"} }")
con<-jsonlite::fromJSON(rawToChar(request$content))
auth<-con$jwt

##### set query #####
query_url<-"https://api.fairsharing.org/search/fairsharing_records?fairsharing_registry=standards&countries=hungary"

get_res<-POST(
  query_url,
  add_headers(
    "Content-Type"="application/json",
    "Accept"="application/json",
    "Authorization"=paste0("Bearer ",auth,sep="")
  )
)

query_con<-fromJSON(rawToChar(get_res$content))

##### see results #####
data<-query_con$data
View(data)
