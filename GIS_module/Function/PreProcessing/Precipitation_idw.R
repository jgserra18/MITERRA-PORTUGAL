source('./GIS_module/Function/General_GIS_functions.R')

library(raster)
library(gstat)
library(rgdal)

prec_folder <- function() {
  ## prec source: SNIRH 2019 (in mm/yr)
  
  main_folder <- select_maindata_pattern('Climatic')
  prec <- list.files(main_folder, pattern='Precipitation')
  prec_folder <- file.path(main_folder, prec)
  return(prec_folder)
}


evapo_folder <- function() {
  ## evapo source: JRC GRIDDED AGRO-METEREOLOGICAL DATA
  
  main_folder <- select_maindata_pattern('Climatic')
  evapo <- list.files(main_folder, pattern='Evapotranspiration')
  evapo_folder <- file.path(main_folder, evapo)
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


georeference_idw_stations <- function(year, source) {
  #georeferences the spatialpoints coordinates
  
  idw_stations <- get_idw_stations(year, source)
  SP <- SpatialPoints(idw_stations[, 3:2], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
  SPdf <- SpatialPointsDataFrame(SP, idw_stations)
  
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  SPdf <- spTransform(SPdf, proj4string(caa))
  SPdf@bbox <- as.matrix(extent(caa))
  rm(caa)
  return(SPdf)
}


interpolate_idw <- function(year, source) {
  
  SPdf <- georeference_idw_stations(year, source) #SpatialPointsDataFrame
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  r <- raster(extent(caa), crs=crs(caa), res=1000)
  
  ifelse(source=='precipitation', gs <- gstat(formula=Annual~1, locations=SPdf, nmax=5, set=list(idp=0)),
         gs <- gstat(formula=evapo~1, locations=SPdf, nmax=5, set=list(idp=0)))
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

### ------------------------------------------------------------------------------
## Write rasters
### ------------------------------------------------------------------------------

## PRECIPITATION -----------------------------------------------------------------
#whole precipitation
p99 <- downscale_idw_caa(1999, caa_whole=FALSE, 'precipitation')
p09 <- downscale_idw_caa(2009, caa_whole=FALSE, 'precipitation')

write_idw(p99, 1999, 'rast_p', 'precipitation')
write_idw(p09, 2009, 'rast_p', 'precipitation')

#precipitation within UAA
prec99 <- downscale_idw_caa(1999, caa_whole=TRUE, 'precipitation')
prec09 <- downscale_idw_caa(2009, caa_whole=TRUE, 'precipitation')

write_idw(prec99, 1999, 'rast_caa', 'precipitation')
write_idw(prec09, 2009, 'rast_caa', 'precipitation')

## EVAPO --------------------------------------------------------------------------
e99 <- downscale_idw_caa(1999, caa_whole=FALSE, 'evapo')
e09 <- downscale_idw_caa(2009, caa_whole=FALSE, 'evapo')

write_idw(e99, 1999, 'rast_e', 'evapo')
write_idw(e09, 2009, 'rast_e', 'evapo')

evapo99 <- downscale_idw_caa(1999, caa_whole=TRUE, 'evapo')
evapo09 <- downscale_idw_caa(2009, caa_whole=TRUE, 'evapo')

write_idw(evapo99, 1999, 'rast_caa', 'evapo')
write_idw(evapo09, 2009, 'rast_caa', 'evapo')


