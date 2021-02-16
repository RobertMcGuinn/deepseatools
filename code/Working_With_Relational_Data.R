#set my working directory to location of netcdf file
setwd('d:/NCEI_DataVis/RDB1')

#Website for WODB Worldwide temperature climatology
#https://www.nodc.noaa.gov/cgi-bin/OC5/woa13/woa13.pl
#Select Availabe formats as NetCDF 
#Select Availabe grids as 1/4 degree
#Click "Update Data Links"
#Under "All fields data links (1/4 grid)" click on THREDDS Catalog
#Download 664.9 MB woa13_decay_t0004v2.nc for annual average of all time periods.

# Read in Data 
##############################################################################################
starec <- read.csv("starec.csv",stringsAsFactors = F)
envrec <- read.csv("envrec.csv",stringsAsFactors = F)
invrec <- read.csv("invrec.csv",stringsAsFactors = F)
bgsrec <- read.csv("bgsrec.csv",stringsAsFactors = F)
##############################################################################################

# Keep only selected variables for clariey
##############################################################################################
starec <- subset(starec,
                 select=c(CRUISEID, STATIONID,VESSEL, CRUISE_NO, P_STA_NO, S_STA_NO, TIME_ZN,
                          TIME_MIL, MO_DAY_YR, DECSLAT, DECSLON, DEPTH_SSTA,
                          STAT_ZONE, TOW_NO, NET_NO)
                 )

envrec <- subset(envrec,
                 select=c(STATIONID, ENVRECID, TEMPMAX,
                          SALMAX, CHLORMAX, OXYMAX)
                 )

invrec <- subset(invrec,
                 select=c(INVRECID, STATIONID, CRUISEID, VESSEL, CRUISE_NO, P_STA_NO,
                          GEAR_SIZE, GEAR_TYPE, MESH_SIZE, OP, MIN_FISH, TOT_LIVE)
                 )

bgsrec <- subset(bgsrec,
                 select=c(STATIONID, INVRECID, BGSID, GENUS_BGS, SPEC_BGS, IS_SAMPLE, CNT, CNTEXP,
                          SAMPLE_BGS, SELECT_BGS, BIO_BGS )
                 )
##############################################################################################

#how many starec, envrec, invrec, bgsrec records do i have
print(paste('STAREC',nrow(starec),sep=' '))
print(paste('ENVREC',nrow(envrec),sep=' '))
print(paste('INVREC',nrow(invrec),sep=' '))
print(paste('BGSREC',nrow(bgsrec),sep=' '))

#let do some further review
#how many unique invrecid do i have in the invrec data?
#number of unqiue invrecid translates into the number of trawls conducted
length(unique(invrec$INVRECID))
#how many unique stationid do i have in the invrec data?
#examining this because i am expecting 1 inventory record at each station
length(unique(invrec$STATIONID))

#make it easier to examine species names by concatenating GENUS_BGS AND SPEC_BGS using paste()
bgsrec$TAXON <- paste(bgsrec$GENUS_BGS,bgsrec$SPEC_BGS,sep='_')

#how many unique species do i have using the unique()?
length(unique(bgsrec$TAXON))
#what are they?
unique(bgsrec$TAXON)
# how many trawls (INVRECID's) are represented in the catch (bgsrec)
length(unique(bgsrec$INVRECID))
# does this count match the number of trawls taken....

#assuming i have n trawls and n species a full record set would be how many observations would I in a
#full datset that would include all invrecid's and taxon?
length(unique(invrec$INVRECID)) * length(unique(bgsrec$TAXON))

#lets experiment with the merge() function to link/relate individual data frames based on their primary
#keys in the database
?merge
#specifically, lets relate trawls(invrec) and their catch(bgsrec)
inv_bgs1 <- merge(invrec,bgsrec,by="INVRECID",all=T,sort=T)
#how many records did we end up with
nrow(inv_bgs1)

#lets open the inv_bgs1 in our viewer
#which observations has no matching catch data?

#why did we not get the expcected result?

##No catch for our targe species or no catch at all for that particular trawl

##We related the invrec (trawls) and bgsrec (catch) based on our INVRECID field, when we really need
##to merge on INVRECID and TAXON.  Unfortunately, our relational database does not record all combinations
##of INVRECID and TAXON

#We typically work around this by developing an intermediate dataset that contains all combinations of
#unique INVREC (trawls) and species of interest we want to examine.
trawls <- unique(invrec$INVRECID)
trawls
species <- unique(bgsrec$TAXON)
species

#A R function that allows the creation of a dataframe for all cominations of our trawls an species 
#is the is expand.grid()
t_x_s <- expand.grid(INVRECID = trawls,TAXON = species)
#how many records does this contain
nrow(t_x_s)
#does this match our expected number of observations?

#So what happens if we merge/relate our bgsrec data to t_x_s based on INVRECID and TAXON
allcatch <- merge(t_x_s,bgsrec,by=c("INVRECID","TAXON"),all=T,sort=T)

#lets get rid of some variables for a cleare picture
allcatch <- subset(allcatch,
                 select=c(STATIONID, INVRECID, TAXON, CNTEXP, SELECT_BGS)
                  )

#Expected records match, but what about all the NA values...
#Thes represent the no catch an can be set to zero for counts (CNTEXP) and weight(SELECT_BGS)
allcatch$CNTEXP[is.na(allcatch$CNTEXP)]<- 0
allcatch$SELECT_BGS[is.na(allcatch$SELECT_BGS)]<- 0

#Congrats, we know have counts and weights for each species for each trawl.

##############################################################################################
#Next Session We will look at relating our final catch data with our Station (starec) and
#environmental information (envrec) for further analysis.
##############################################################################################

#add environmental to starec
sta_env<- merge(starec,envrec,by="STATIONID",all.x=T,sort=T)
#add trawl information to sta_env
sta_env_inv <- merge(sta_env,invrec,by="STATIONID",all.y=T,sort=T)
#add catch to sta_evn_inv
trawlcatch <- merge(sta_env_inv,allcatch,by="INVRECID",all=T,sort=T) 

by1 = as.factor(trawlcatch$TAXON)
meancnt <- aggregate(x = trawlcatch$CNTEXP, by=list(by1), FUN = "mean")



