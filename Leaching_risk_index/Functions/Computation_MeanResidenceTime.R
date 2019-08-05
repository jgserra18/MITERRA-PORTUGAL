source('./GIS_module/Function/GW_computation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./GIS_module/Function/LU_irrigation_allocation.R')

#this is used to get soil porosity and water table depth
get_soil_data <- function(filename)
{
  data_folder <- select_maindata_pattern('EnvironmentalConditions')
  soil <- list.files(data_folder, pattern = 'Soil', full.names=T)
  select_file <- list.files(soil, pattern=filename, full.names = T)
  r_file <- raster(select_file)
}

get_porosity <- function()
{
  get_soil_data('porosity')
}

get_water_table <- function()
{
  get_soil_data('water_table')
}

get_irrig_percolation <- function(year)
{
  irrig_percolation <- get_irrig_LU_data(year, 'Volumes', 'leaching', 'meter')
  r_file <- raster(irrig_percolation)
}

get_total_water_percolation <- function(year)
{
  #sums the water volume lost through percolation in irrigation and aquifer recharge
  #unit: both in meters/yr
  
  irrig_perco <- get_irrig_percolation(year) #in meters
  gw_recharge <- get_modelling_files('Drainage', paste0('gw_recharge', year_prefix(year)), 'Default')/1000 #in meters
  
  tot_drainage <- irrig_perco+gw_recharge
  return(tot_drainage)
}

compute_nitrate_velocity <- function(year) {
  
  #computes total nitrate velocity (in m/y)
  tot_drainage_m <- get_total_water_percolation(year)
  p <- get_porosity()
  v_no3 <- tot_drainage_m/p #in meters/year
  
}

compute_mean_residence_time <- function(year) {
  
  v_no3 <- compute_nitrate_velocity(year)
  wtable <- get_water_table()
  rs_wtable <- resample(wtable, v_no3)
  
  rt_no3 <- rs_wtable/v_no3
  #rt_no3[rt_no3<1] <- 1
  
}

compute_aquifer_MRT <- function(year) {
  
  mrt <- compute_mean_residence_time(year)
  
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  
  gw_ids <- gw$GW_ID
  calc_df <- data.frame(GW_ID=gw_ids)
  calc_df$parameter  <- ''
  
  for (i in gw_ids)
  {
    sb_gw <- subset(gw, GW_ID==i)
    crop <- crop(rt_no3, extent(sb_gw))
    mask <- mask(crop, sb_gw)
    sum <- cellStats(mask, 'mean')
    med <- cellStats(mask, 'median')
    calc_df[i, 2] <- med
    calc_df[i, 3] <- sum
    rm(list=c('crop', 'mask'))
  }
  
  print('Done.')
  calc_df[, 'parameter'] <- as.numeric(calc_df[, 'parameter'])
  calc_df[, 'parameter'] <- round(calc_df[, 'parameter'], 3)
  write.csv(calc_df, 'avg_resi_time_no3.csv')
  
  
}

FALTAM OS FOLDERS
create_gw_shapefile_MRT <- function(year) {
  
  gw_mrt <- compute_aquifer_MRT(year)
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  
  gw <- merge(gw, gw_mrt, 'GW_ID')
  
}



