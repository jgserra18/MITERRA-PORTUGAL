source('./GIS_module/Function/General_GIS_functions.R')

library(raster)
library(gstat)
library(rgdal)

get_climatic_folder <- function() {
  
  climatic_folder <- select_maindata_pattern('Climatic')
  return(climatic_folder)
}

get_climatic_subfolder <- function(subfolder) {
  
  climatic_folder <- get_climatic_folder()
  climatic_subfolder <- list.files(climatic_folder, pattern=subfolder, full.names=T)
  return(climatic_subfolder)
}

#dafuw1
prec_folder <- function() {
  ## prec source: SNIRH 2019 (in mm/yr)

  prec_folder <- get_climatic_subfolder('Precipitation')
  return(prec_folder)
}

#dafuq2
evapo_folder <- function() {
  ## evapo source: JRC GRIDDED AGRO-METEREOLOGICAL DATA
  
  evapo_folder <- get_climatic_subfolder('Evapotranspiration')
  return(evapo_folder)
}

get_idw_stations <- function(year, idw_source) {
  #prepares the idw data of the respective source, either evapo or precipitation
  
  if (idw_source=='precipitation') {
    data_folder <- prec_folder()
    select_idw <- list.files(data_folder, pattern=paste0('idw', year_prefix(year)))
  }
  else if(idw_source=='evapo') {
    data_folder <- evapo_folder()
    select_idw <- list.files(data_folder, pattern=paste0('idw', year_prefix(year)))
  }
  
  idw_filepath <- file.path(data_folder, select_idw)
  idw_file <- read.csv(idw_filepath)
  
  if(year==1999 && idw_source=='precipitation') {idw_file <- idw_file[1:305, ]}
  return(idw_file)
}


georeference_idw_stations <- function(year, source, df) {
  #georeferences the spatialpoints coordinates
  
  ifelse(missing(df)==TRUE,
         idw_stations <- get_idw_stations(year, source),
         idw_stations <- df)
  SP <- SpatialPoints(idw_stations[, 3:2], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
  SPdf <- SpatialPointsDataFrame(SP, idw_stations)
  
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  SPdf <- spTransform(SPdf, proj4string(caa))
  SPdf@bbox <- as.matrix(extent(caa))
  rm(caa)
  return(SPdf)
}

interpolate_idw <- function(year, source, SPdf_station) {
  
  if(missing(SPdf_station)==TRUE) {
         SPdf <- georeference_idw_stations(year, source) #SpatialPointsDataFrame
  } else {SPdf <- SPdf_station}

  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  r <- raster(extent(caa), crs=crs(caa), res=100)
  
  if (missing(SPdf_station)==TRUE) {
    ifelse(source=='precipitation', gs <- gstat(formula=Annual~1, locations=SPdf, nmax=5, set=list(idp=0)),
           gs <- gstat(formula=evapo~1, locations=SPdf, nmax=5, set=list(idp=0)))
  } else {
    gs <- gstat(formula=sum~1, locations=SPdf, nmax=5, set=list(idp=0))
  }
  nn <- interpolate(r, gs)
  return(nn)
}


idw_cleaning <- function(year, source)
{
  #cleans the idw output and uses a PT mask
  
  idw_prec <- interpolate_idw(year, source)
  muni <- get_muni_shp()
  
  idw_prec_mask <- mask(idw_prec, muni)
  return(idw_prec_mask)
}


downscale_idw_caa <- function(year, caa_whole, source)
{
  prec <- idw_cleaning(year, source)
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  prec <- resample(prec, caa)
  
  if (caa_whole==TRUE){prec <- prec*caa}
  else  {prec <- prec}
  
  return(prec)
}


write_idw <- function(idw_rast, year, prec_name, source)
{
  #prec_nane -s either rast_p or rast_caa
  #writes to precipitation/evapotranspiration subfolder within activity data
  
  tifoptions <- c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  name <- paste0(prec_name, year_prefix(year), '.tif')
  ifelse(source=='precitation', p_folder <- prec_folder(), p_folder <- evapo_folder())
  path <- file.path(p_folder, name)
  
  writeRaster(idw_rast, path, format='GTiff', options=tifoptions, overwrite=TRUE)
}


