##### bring in data #####
setwd("C:/data/geoindata.gdb/")
mpas_sp <- readOGR(".", "mpas_RPMcGuinn_20201019_0")
oculina_sp <- readOGR(".", "oculina_bank_po_RPMcGuinn_20201019_0")
oculinaexp_sp <- readOGR(".", "oculina_bank_exp_po_RPMcGuinn_20201019_0")
hapc_sp <- readOGR(".", "coral_hapc_RPMGuinn_20191019_0")

##### transform to sf #####
points <- st_as_sf(x_geo, wkt = "geom")
mpas <- st_as_sf(mpas_sp, wkt = "geom")
oculina <- st_as_sf(oculina_sp, wkt = "geom")
oculinaexp <- st_as_sf(oculinaexp_sp, wkt = "geom")
hapc <- st_as_sf(hapc_sp, wkt = "geom")

##### get everything into the same projection #####
mpas <- st_transform(mpas, crs = proj4string(x_geo))
oculina <- st_transform(oculina, crs = proj4string(x_geo))
oculinaexp <- st_transform(oculinaexp, crs = proj4string(x_geo))
hapc <- st_transform(hapc, crs = proj4string(x_geo))

##### spatial joins with points #####
points_mpas <- st_join(points, mpas) #NAME
points_oculina <- st_join(points, oculina) #AREA_NAME
points_oculinaexp <- st_join(points, oculinaexp) #AreaName
points_hapc <- st_join(points, hapc) #NAME

##### stripping off CatalogNumbers from spatial joins #####
mpas_cats <- points_mpas %>% filter(is.na(NAME) == F)
oculina_cats <- points_oculina %>%  filter(is.na(AREA_NAME) == F)
oculinaexp_cats <- points_oculinaexp %>% filter(is.na(AreaName) == F)
hapc_cats<- points_hapc %>% filter(is.na(NAME) == F)




## checking
names(points_oculina)
z <- points_oculina %>% filter(is.na(AREA_NAME) == F)
plot(z["AREA_NAME"])

names(points_mpas)
z <- points_mpas %>% filter(is.na(NAME) == F)
plot(z["NAME"])

names(points_mpas)
z <- points_oculinaexp %>% filter(is.na(AREA_NAME) == F)
plot(z["AREA_NAME"])


## write test shapefile for testing #####
z <- points_oculina %>%
  filter(is.na(AREA_NAME) == F)














