##### Header #####
## author: Robert P. McGuinn
## startdate: 20231115
## filename: 20231115-0_data_inventory_RPMcGuinn
## purpose: working with the data inventory for the deep sea coral project

##### packages #####
library(tidyverse)
library(googledrive)
library(googlesheets4)

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### read in master data inventory using googlesheets4 #####
sheetid <- "1g2KDl0KGKjDcjdzsLHf_YGkHSiaGL5UNmge3l21h73s"
di <- read_sheet(sheetid)

##### create dashlink #####
filt$dashlink <- paste('https://www.ncei.noaa.gov/waf/dsc-data/dashboards/',
                       filt$DatasetID, '.html', sep = '')

##### check #####
table(di$CoralAndSpongeAnnotations, useNA = 'always')

x <- di %>%
  filter(CoralAndSpongeAnnotations=='Received' | CoralAndSpongeAnnotations == 'Expected') %>%
  group_by(ProjectID, Vessel, Vehicle, Year, BeginDate, EndDate, SurveyID, FMCRegion, CoralAndSpongeAnnotations, PrimaryContacts) %>%
  summarize(n=n())
View(x)

filt %>% filter(grepl('Shearwater', Vessel)) %>%
  group_by(DatasetID, Vessel, PI,Reporter,DataContact, ObservationYear, dashlink) %>%
  summarize(n=n()) %>% View()

##### Results #####

c('2012_MarthaNizinski_Northeast_Cruise_Henry Bigelow_HB1204',
 'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_HB-12-04.html',
 'https://vlab.noaa.gov/redmine/issues/112800',
 'https://vlab.noaa.gov/redmine/issues/82243')

c('2009_SteveRoss_South Atlantic_Seward Johnson_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_SJ-09-08.html',
  'https://vlab.noaa.gov/redmine/issues/50694')

c('2010_Bowlby/Bright_Pacific_Cruise_MacArthur II_Leg1?',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_M2-10-06-L1-AUV.html')

c('2010_Bowlby/Bright_Pacific_Cruise_MacArthur II_Leg2?',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_M2-10-06-L2-ROV.html')

c('2010_DanHoward_Pacific_MacArthur II_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_M2-10-06-L2-ROV.html')

c('2010_ElizabethClarke_Pacific_R/V Pacific Storm_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_PS-10-01-L1.html',
  'https://vlab.noaa.gov/redmine/issues/35760')

c('2010_GeorgeSedberry_South Atlantic_Pisces_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_PC-10-02.html',
  'https://vlab.noaa.gov/redmine/issues/26826')

c('2010_MaryYoklavich_Pacific_MacArthur II_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_M2-10-06-L3-AUV.html',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_M2-10-06-L3-ROV.html',
  'https://vlab.noaa.gov/redmine/issues/61729')

c('2010_SteveRoss/SandraBrooke_South Atlantic_Ron Brown_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_RB-10-08.html')

c('2010_Yoklavich/Love_Pacific_F/V Velero IV_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_VO-10-10.html',
  'https://vlab.noaa.gov/redmine/issues/35355')

c('2011_AndyDavid_South Atlantic_Pisces_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_PC-11-03.html')

c('2011_BobStone/JenniferBright_North Pacific_DataMining')

c('2011_BobStone_North Pacific_DataMining')

c('2011_JohnReed_South Atlantic_Nancy Foster_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_NF-11-09.html',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_NF-11-09-L3.html')

c('2012_ChrisRooper_North Pacific_HabitatSuitabilityModelling',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_SS-12-08.html')

c('2012_ChrisRooper_North Pacific_ParameterMeasurement')

c('2012_ChristinaConrath_North Pacific_LifeHistory',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_FMP-12-02.html',
  'https://vlab.noaa.gov/redmine/issues/35700')

c('2012_DanielleLipski_Pacific_DataMining_Okeanos_EX1101')

c('2013_BobStone/PeterEtnoyer_North Pacific_Alaska Provider_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_AP-13-08.html')

c('2013_BobStone_North Pacific_Medeia_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_MD-13-08.html',
  'https://vlab.noaa.gov/redmine/issues/18659')

c('2013_DavePacker/SteveAuster_Northeast_Cruise_Connecticut_1307Auster',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_CT-13-07.html')

c('2013_Katz/Caldow_Pacific_Shearwater_Cruise',
  'https://www.ncei.noaa.gov/waf/dsc-data/dashboards/NOAA_SW-13-06.html',
  'https://vlab.noaa.gov/redmine/issues/37562')




























  '
'




























































