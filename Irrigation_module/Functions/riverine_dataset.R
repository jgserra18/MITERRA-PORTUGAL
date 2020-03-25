source('./GIS_module/Function/General_GIS_functions.R')
source('./Irrigation_module/Functions/NO3_spatial_modelling.R')



## ----------------------- SURFACE WATER EXPLORATORY VARS ----------------------- ##
## -------------------------------------------------------------------------------##


create_r_surface <- function() {
  
  muni <- get_muni_shp()
  r <- raster(ext=extent(muni), crs=crs(muni), res=1000)
  
  surf <- load_shp('surface_water')
  surf <- spTransform(surf, proj4string(r))
  surf <- sf::st_as_sf(surf)
  surf <- subset(surf, agua_leg != 'Aguas de transicao')
  
  r_surf <- fasterize(surf, r)
  
  return(r_surf)
  rm(list=c('muni', 'r', 'surf'))
}

get_surface_water_dataset <- function(dataset) {
  # gets the dataset for surface water algorithm
  
  dataset <- list.files(path = './Activity data/Irrigation_data/SW_dataset/', 
                        pattern = dataset, 
                        full.names = TRUE)
  dataset <- read.csv(dataset, stringsAsFactors = F)
  return(dataset)
}


process_AGLO_params <- function() {
  
  
  df <- get_surface_water_dataset('Agglo') %>% 
    dplyr::select(aggLatitude, aggLongitude, aggC1,aggGenerated)
  
  pt <- load_shp('Municipalit')
  r <- raster(ext=extent(pt), crs=crs(pt), res=1000)
  r_list <- list()
  calc_cols <- seq(3, ncol(df))
  for (i in calc_cols) {
    col_name <-  colnames(df)[i]
    print(col_name)
    
    new_df <- df[, c(1,2,i)]
    new_df <- na.omit(new_df)
    new_df$aggLatitude <- as.numeric(new_df$aggLatitude)
    SP <- SpatialPoints(new_df[, 2:1], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
    SPdf <- SpatialPointsDataFrame(SP, new_df)
    SPdf <- spTransform(SPdf, proj4string(pt))
    SPdf <- crop(SPdf, extent(pt))
    SPdf <- rgeos::gBuffer(spgeom = SPdf, byid = TRUE, width = 5000)
    r_SPdf <- rasterize(SPdf, r,col_name)
    
    r_list <- append(r_SPdf, r_list)
  } 
  
  return(r_list)
  rm(list=c('df', 'pt', 'r', 'r_list', 'calc_cols', 'col_name', 'new_df', 'SP', 'SPdf', 'r_SPdf'))
}



process_WWTPS_params <- function() {
 
  df <- get_surface_water_dataset('WWTPS')%>%
    dplyr::filter(dcpWaterBodyType=='FW', rptMStateKey=='PT') %>%
    dplyr::select(uwwLatitude, uwwLongitude, 
           uwwCapacity, uwwTOTALNDischarge, uwwNRemoval,
           uwwOtherTreatment, uwwPrimaryTreatment, uwwSecondaryTreatment)
  calc_cols <- seq(3, ncol(df))
  r_list <- list()
  pt <- load_shp('Municipalit')
  r <- raster(ext=extent(pt), crs=crs(pt), res=1000)
  
  for (i in calc_cols) {
    col_name <-  colnames(df)[i]
    print(i)
    
    new_df <- df[, c(1,2,i)]
    new_df <- na.omit(new_df)
    new_df[, seq(1,3)] <- sapply(new_df[,seq(1,3)], as.numeric)
    new_df <- na.omit(new_df)
    SP <- SpatialPoints(new_df[, 2:1], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
    SPdf <- SpatialPointsDataFrame(SP, new_df)
    SPdf <- spTransform(SPdf, proj4string(pt))
    SPdf <- crop(SPdf, extent(pt))
    SPdf <- rgeos::gBuffer(spgeom = SPdf, byid = TRUE, width = 5000)
    r_SPdf <- rasterize(SPdf, r,col_name)
    
    r_list <- append(r_SPdf, r_list)
  }
  
  return(r_list)
  rm(list=c('df', 'calc_cols', 'r_list', 'pt', 'r', 'col_name', 'new_df', 'SP', 'SPdf', 'r_SPdf'))
}


process_SW_params <- function() {
  
  df <- get_surface_water_dataset('SW_info')
    
  calc_cols <- seq(6, ncol(df))
  r_list <- list()
  pt <- load_shp('Municipalit')
  r <- raster(ext=extent(pt), crs=crs(pt), res=1000)
  
  for (i in calc_cols) {
    col_name <-  colnames(df)[i]
    print(i)
    
    new_df <- df[, c(2,3,i)]
    new_df <- na.omit(new_df)
    
    SP <- SpatialPoints(new_df[, 1:2], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
    SPdf <- SpatialPointsDataFrame(SP, new_df)
    SPdf <- spTransform(SPdf, proj4string(pt))
    SPdf <- rgeos::gBuffer(spgeom = SPdf, byid = TRUE, width = 5000)
    
    r_SPdf <- rasterize(SPdf, r,col_name)
  }
  
  return(r_list)
  rm(list=c('df', 'calc_cols', 'r_list', 'pt', 'r', 'col_name', 'new_df', 'SP', 'SPdf', 'r_SPdf'))
}


aggregate_river_dataset <- function() {
  
  agglo <- process_AGLO_params()
  wwtp <- process_WWTPS_params()
  sw <- process_SW_params()
 # wb <- create_r_surface()
  
  agglo <- append(wwtp, c(agglo, sw))
  agglo <- stack(agglo)
  
  return(agglo)
  rm(list='wwtp')
}


## ----------------------- SURFACE WATER MONITORING ----------------------- ##
## -------------------------------------------------------------------------##


create_SPdf_SW_NO3 <- function(year) {
  # create adequately projected SPdf of mean NO3 levels for a given year
  # unit: mg NO3 l-1 yr-1
  
  NO3_stations <-  get_activity_data(subfolder = 'Irrigation_data', 
                                     subfolderX2 = 'NO3 stations', 
                                     file_pattern = 'NO3_surface')
  SP <- SpatialPoints(NO3_stations[, 1:2],  
                      proj4string = CRS('+proj=tmerc +lat_0=39.66666666666666 +lon_0=-8.131906111111112 +k=1 +x_0=200000 +y_0=300000 +ellps=intl +towgs84=-288.885,-91.744,126.244,-1.691,0.410,-0.211,-4.598 '))
  SPdf <- SpatialPointsDataFrame(SP, NO3_stations)
  # reproject it to LAEA
  SPdf <- spTransform(SPdf, 
                      CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'))
  # fine tune the extent 
  new_bb <- c(2635900,  1729700, 2977200, 2298200)
  names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") = "bbox"
  SPdf <- st_as_sf(SPdf)
  attr(st_geometry(SPdf), "bbox") <- new_bb
  SPdf <- sf::as_Spatial(SPdf)
  SPdf <- st_as_sf(SPdf)
  
  
  SPdf <- subset(SPdf, Year==year)
  SPdf <- SPdf %>%
    dplyr::select(Longitude, Latitude, Mean)
  
  return(SPdf)
  rm(list=c('NO3_stations', 'SP', 'new_bb'))
}

d <- create_SPdf_SW_NO3(2009)
expvars <- aggregate_river_dataset()

normalize_SW_ExpVars <- function(year) {
  
  no3_stations <- create_SPdf_SW_NO3(year)
  expvars <- aggregate_river_dataset()
  
  for (i in 1:nlayers(expvars)) {
    
    shp_name <- as.character(names(expvars)[i])
    no3_stations[, shp_name] <- raster::extract(expvars[[i]], no3_stations)
  }
  
  df <- na.omit(no3_stations)
  
  return(df)
  rm(list=c('no3_stations', 'expvars'))
}
d <- normalize_SW_ExpVars(2009)
View(d)
partition <- create_partition(d)
