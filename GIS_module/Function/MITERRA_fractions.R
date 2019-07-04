source('./Main_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./Leaching_module/Function/compute_correct_surface_water_areas.R')

get_miterra_folder <- function(folder_pattern)
{
  env_conditions <- select_maindata_pattern('EnvironmentalConditions')
  miterra <- list.files(env_conditions, pattern='MITERRA')
  miterra_path <- file.path(env_conditions, miterra)
  
  select_folder <- list.files(miterra_path, pattern=folder_pattern)
  folder_path <- file.path(miterra_path, select_folder)
  return(folder_path)
}

select_files_miterra_folder <- function(folder_pattern, file_pattern)
{
  folder <- get_miterra_folder(folder_pattern)
  select_raster <- list.files(folder, pattern=file_pattern)
  raster <- file.path(folder, select_raster)
  raster <- raster(raster)
  
  return(raster)
}

#user specifies folder e.g. ('Common') and alll the rasters within are sampled to the baseliine raster (CAA) 
preProcessing_rasters <- function(folder)
{
  folder <- get_miterra_folder(folder)
  baseline_r <- raster('./GIS_module/Output/LandCover/caaRP09.tif')
  
  tifoptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")
  files <- list.files(folder)
  
  for (i in 1:length(files))
  {
    path <- file.path(folder, files[i])
    r <- raster(path)
    rr <- resample(baseline_r)
    writeRaster(rr, path, overwrite=T, format='GTiff', options=tifoptions)
  }
}

#function to process all folders
precProcess_allfolders <- function()
{
  folders <- c('Common', '1999', '2009')
  
  pre_process <- sapply(folders, function(x) preProcessing_rasters(x))
}


get_YearSpecific_raster <- function(year, raster_pattern)
{
  year <- as.character(year)
  select_raster <- select_files_miterra_folder(year,raster_pattern)
  return(select_raster)
}

#condition to write or return raster output
write_raster_condition <- function(year, raster, write, name)
{
  name <- paste0(name, year_prefix(year))
  
  ifelse(write==T, 
         write_raster(raster, name, 'MITERRA_fractions'),
         return(raster))
}

##min = prec surplus * depthRock * soiltype_runoff
compute_Rf_min <- function(year)
{
  prec_surplus <- get_YearSpecific_raster(year, 'prec_surplus')
  depth_rock <- select_files_miterra_folder('Common', 'DepthRock')
  rf_soiltype <- select_files_miterra_folder('Common', 'Rf_soiltype_runoff')
  
  rf_min <- min(prec_surplus,depth_rock,rf_soiltype)
  return(rf_min)
}

#slope x LU
compute_Rf <- function(year, write)
{
  rf_min <- compute_Rf_min(year)
  slope <- select_files_miterra_folder('Common', 'Slope')
  lu <- select_files_miterra_folder('Common', 'Rf_LU')
  
  rf <- (rf_min*slope*lu)/1000000
  
  write_raster_condition(year, rf, write, 'Rf')
}

#(rc_root, rc_Lf_PS, rc_t99, rc_OC)
compute_Lf_min <- function(year)
{
  oc <- select_files_miterra_folder('Common', 'Lf_OC')
  root <- select_files_miterra_folder('Common', 'Lf_root')
  temp <- get_YearSpecific_raster(year, 'Lf_temp')
  prec_surplus_soil <- get_YearSpecific_raster(year, 'Lf_PrecSurplus')
  
  lf_min <- min(oc, root, temp, prec_surplus_soil)
  rm(list = c('oc', 'root', 'temp', 'prec_surplus_soil'))
  return(lf_min)
}

# rc_texture*rc_LU/100*Lf_min99/100
compute_Lf <- function(year, write)
{
  lf_min <- compute_Lf_min(year)
  texture <- select_files_miterra_folder('Common', 'Lf_SoilType')
  lu <- select_files_miterra_folder('Common', 'Lf_LU')
  
  lf <- (lf_min*texture*lu)/1000000
  write_raster_condition(year, lf, write, 'Lf')
}

#returns list with all fractions computed
#id 1 - Rf_min, id 2- Rf, i3 _ Lf_min, id 4 - Lf
compute_all_fractions <- function(year)
{
  year <- year
  
  for (i in year)
  {
    rf_min <- compute_Rf_min(i)
    rf <- compute_Rf(i, FALSE)
    lf_min <- compute_Lf_min(i)
    lf <- compute_Lf(i, FALSE)
  }
  
  store <- list(rf_min, rf, lf_min, lf)
  return(store)
}

#this doesn't work
#function to write them raster files
write_runoff_leaching_fractions <- function()
{
  year <- c(1999, 2009)
  
  write_raster <- sapply(year, function(x)
    {
      compute_Rf(x, TRUE)
      compute_Lf(x, TRUE)
    }
  )
}

## ------------------------------------------------------------------------------------------------
##  SMALL AND LARGE SURFACE WATERS
## ------------------------------------------------------------------------------------------------

file_names <- c('intersect_large09', 'intersect_large99', 'intersect_small09', 'intersect_small09')

library(raster)
df <- load_surface_shp('intersect_large09')
df  <- spTransform(df, CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
ext <- c(2635900, 2977200, 1729700, 2298200)
df <- raster::crop(df, ext)

r <- raster(extent(df), res=100, crs=crs(df))#0.01)
#proj4string(r) <- sr
r <- rasterize(df, r, field='surface_id', background=NA)
