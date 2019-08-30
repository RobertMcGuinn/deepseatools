##### 20190313-0: RPMcGuinn ####

##### first filter the data as you want it #####

x <- d %>%
  filter(
    campaign == "CAPSTONE", 
    missionCat == "ROV"
    )

##### do the render for-loop step with just filtered data ##### 
# the output would be one report per cruiseID within the filtered data  

for (id in unique(x$cruiseID)){
  sub <- x[x$cruiseID == id,]
  render("C:/rworking/digs/code/your.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/digs/reports/')
}



