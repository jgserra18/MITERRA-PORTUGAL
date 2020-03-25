source('./GIS_module/Function/GW_computation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./GIS_module/Function/LU_irrigation_allocation.R')
source('./WaterBalance_module/Functions/Water_Balance_funcs.R')

get_soil_data <- function(filename) {
  #this is used to get soil porosity and water table depth
  
  data_folder <- select_maindata_pattern('EnvironmentalConditions')
  soil <- list.files(data_folder, pattern = 'Soil', full.names=T)
  select_file <- list.files(soil, pattern=filename, full.names = T)
  r_file <- raster(select_file)
}

get_porosity <- function() {
  get_soil_data('porosity')
}

get_water_table <- function() {
  get_soil_data('water_table')
}

#this is not implemented anymore
# September 2019
get_total_water_percolation <- function(year, irrig_mode) {
  # gets total drainage raster file, alteernitatvely also called 'total recharge' (in meters/year)
  # total drainage = irrigation_percolation + aquifer_recharge
  # e.g. d <- get_total_water_percolation(2009, TRUE)
  
  ifelse(irrig_mode==TRUE, irrig <- 'Irrig', irrig <- 'Default')
  ifelse(irrig <- 'Irrig', filename <- 'total_drainage_m', filename <- 'gw_recharge')
  recharge <- get_modelling_files('Drainage', paste0(filename, as.character(year_prefix(year))), irrig) #in meters
  
  return(recharge)
}


get_total_water_balance <- function(year) {
  # gets the output of the water balance, which gives a rough proxy of the water percolated from below the root area
  # unit: m3/ha/yr
  
  r_wb <- select_WB_subfolder_file(subfolder = 'Water_surplus',file = 'MOSAIC_WS', year = year)
  r_wb <- raster(r_wb)
  return(r_wb)
}

## EXPORT DATA ------------------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------------------------------

create_index_dir <- function(subfolder_name) {
  
  index_folder <- select_module_output('Leaching_risk')
  subfolder <- file.path(index_folder, subfolder_name)
  dir.create(path = subfolder)
  return(subfolder)  
}

write_index <- function(subfolder, filename, file, format) {
  # format is either csv or raster
  # writes the leaching risk index output 
  
  index_folder <- select_module_output('Leaching_risk')
  select_sub <- list.files(index_folder, pattern = subfolder, full.names = TRUE)
  ifelse(format=='csv', type <- '.csv', type <- '.tif')
  file_path <- file.path(select_sub, paste0(filename, type))
  
  if (format=='csv') {
    write.csv(x = file, file = file_path)
  } else {
    tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
    writeRaster(file, file_path, options=tifoptions, format='GTiff', overwrite=TRUE)
  }
}

get_risk_data <- function(subfolder, filename) {
  
  index_folder <- select_module_output('Leaching_risk')
  select_sub <- list.files(index_folder, pattern = subfolder, full.names = TRUE)
  select_file <-  list.files(select_sub, pattern = filename, full.names = TRUE)
  return(select_file)
}

## ------------------------------------------------------------------------------------------------------------------------

compute_nitrate_velocity <- function(year) {
  #computes total nitrate velocity (in m/y)
  
  print('Looping around NO3 velocity.')
  path <- create_index_dir('Residence_time')
  tot_drainage_m <- get_total_water_balance(year)/10000 #convert from m3/ha/yr to m/yr
  p <- get_porosity()
  v_no3 <- tot_drainage_m/p #in meters/year
  
  return(v_no3)
  write_index('Residence_time', filename = paste0('v_no3', year_prefix(year)), file = v_no3, format = 'raster')
}

compute_mean_residence_time <- function(year) {
  # computes general mean residence time for Portugal
  
  print('Looping around general MRT.')
  path <- create_index_dir('Residence_time')
  v_no3 <- compute_nitrate_velocity(year)
  wtable <- get_water_table()
  rs_wtable <- resample(wtable, v_no3)
  
  rt_no3 <- rs_wtable/v_no3
  #rt_no3[rt_no3<1] <- 1
  write_index('Residence_time', filename = paste0('residence_time', year_prefix(year)), file = rt_no3, format = 'raster')
  rm(list=c('wtable', 'rs_wtable', 'v_no3', 'rt_no3'))
}



raster_median <- function(raster) {
  median <- quantile(raster)[3]
  return(median)
}



compute_aquifer_MRT <- function(year) {
  # computes aquifer mean residence time (in years)
  
  print('Looping around aquifer MRT.')
  path <- create_index_dir('Residence_time')
  mrt_no3 <- raster(get_risk_data(subfolder = 'Residence_time', 
                                       filename = paste0('residence_time', year_prefix(year))))
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  
  gw_ids <- gw$GW_ID
  calc_df <- data.frame(GW_ID=gw_ids)
  #calc_df$parameter  <- ''
  
  for (i in gw_ids) {
    sb_gw <- subset(gw, GW_ID==i)
    crop <- crop(mrt_no3, extent(sb_gw))
    mask <- mask(crop, sb_gw)
    avg <- cellStats(mask, 'mean')
    med <- raster_median(mask)
    calc_df[i, 'avg_mrt'] <- avg
    calc_df[i, 'median_mrt'] <- med
    rm(list=c('crop', 'mask'))
  }
  
  print('Done.')
 # calc_df[, 'parameter'] <- as.numeric(calc_df[, 'parameter'])
  #calc_df[, 'parameter'] <- round(calc_df[, 'parameter'], 3)
  write_index('Residence_time', filename = paste0('gw_avg_MRT', year_prefix(year)), file = calc_df, format = 'csv')
}


loop_MRT_computations <- function() {
  # note: compute_aquifer_MRT calculates in a sequenced order nitrate velocity and general MRT
  
  year <- c(1999, 2009)
  for (i in year) {
    compute_nitrate_velocity(i)
    compute_mean_residence_time(i)
    compute_aquifer_MRT(i)
  }
}


MRT_reclass_index <- function() {
  #general reclass dataframe
  rcl_df <- data.frame(MRT=c('<3', '3-5', '5-10', '10-20', '>20'),
                       rcl <- c(1,0.8,0.6,0.4,0.2))
  return(rcl_df)
}

MRT_reclass <- function() {
  # raster reclass vector  
  rcl <- c(0, 3, 1, 3, 5, 0.8, 5, 10, 0.6, 10, 20, 0.4, 20, +Inf, 0.2)
  return(rcl)
}


create_gw_shapefile_MRT <- function(year) {
  # creates a shapefile with MRT for each aquifer
  
  gw_mrt <- read.csv(get_risk_data(subfolder = 'Residence_time', filename = paste0('gw_avg_MRT', year_prefix(year))))
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  
  gw <- merge(gw, gw_mrt, 'GW_ID')
  return(gw)
  rm(list='gw_mrt')
}


MRT_raster_reclassifiction <- function(year) {
  # creates the reclassification raster for GW MRT
  
  path <- create_index_dir('GIS_index')
  gw_mrt_shp <- create_gw_shapefile_MRT(year)
  gw_r <- general_rasterize(shp = gw_mrt_shp, fiel_rast = 'median_mrt')
  gw_r <- reclassify(gw_r, rcl=MRT_reclass())
  
  write_index('GIS_index', filename = paste0('MRT_reclass', year_prefix(year)), file = gw_r, format = 'raster')
  rm(list=c('path', 'gw_mrt_shp', 'gw_r'))
}

loop_MRT_reclass <- function() {
  #loops around the annual reclassification of GW MRT
  year <- c(1999, 2009)
  sapply(year, function(x) MRT_raster_reclassifiction(x))
}


mask_aquifer_reclass_index <- function(rasterfile) {
  
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  gw <- subset(gw, gw_data0_1=='Aq')
  
  r <- crop(rasterfile, extent(gw))
  r <- mask(r, gw)
  return(r)
}


## SLOPE AND SOIL TEXTURE  -------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------------------------------

source('./GIS_module/Function/MITERRA_fractions.R')

recl_soilTexture <- function() {
  # reclassifies MITERRA soil texture
  
  soil_text <- select_files_miterra_folder('Common', 'Lf_SoilType')
  soil_text <- reclassify(soil_text, rcl = c(0,50,0.33,
                                             50,75, 0.63, 
                                             75,100, 1))
}

recl_slope <- function() {
  
  slope <- get_soil_data(filename = 'slope')
  slope <- reclassify(slope, rcl = c(0,1,1,
                                  1,2,0.8,
                                  2,3,0.6,
                                  3,4,0.4,
                                  4,+Inf,0.2))
}


recl_RT <- function(year) {
  
  rt <- get_module_subfolder_output(module = 'Leaching_risk_index', submodule = 'Residence_time', file_pattern = paste0('time', year_prefix(year)))
  rt <- reclassify(rt, rcl = MRT_reclass())
  return(rt)
}

## VULNERABILITY -------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------------------------------


compute_vulnerability <- function(year, write) {
  
  slope <- recl_slope()
  text <- recl_soilTexture()
  rt <- recl_RT(year)
  
  vulnerability <- 0.6 * rt + 0.2 * slope + 0.2 * text
  
  if (missing(write)==TRUE) {
    return(vulnerability)
  } else {
    write_index('GIS_index', filename = paste0('Vulnerability_', year_prefix(year)), file = vulnerability, format = 'raster')
  }
  rm(list=c('slope','text','rt'))
}

loop_vulnerability <- function() {
  year <- c(1999, 2009)
  sapply(year, function(x) compute_vulnerability(x, T))
}



compute_gw_vulnerability <- function(year, write) {
  
  vul <- compute_vulnerability(year)
  gw <- load_shp('gw')
  gw <- subset(gw, gw_data0_1=='Aq')
  gw <- spTransform(gw, proj4string(vul))
  gw <- st_as_sf(gw)
  
  ids <- gw$GW_ID
  df <- data.frame(GW_ID = ids)
  gw_vul <- exactextractr::exact_extract(vul, gw, 'mean')
  df <- cbind(df, gw_vul)
  names(df)[2] <- 'Vulnerability'
  
  gw <- merge(gw, df, 'GW_ID')
  
  if (missing(write)==TRUE) {
    return(gw)
  } else {
    return(gw)
    write_index('GIS_index', filename = paste0('Vulnerability_GW', year_prefix(year)), file = df, format = 'csv')
  }
  rm(list=c('vul', 'df', 'ctr', 'sb_gw', 'sb_ri'))
}

