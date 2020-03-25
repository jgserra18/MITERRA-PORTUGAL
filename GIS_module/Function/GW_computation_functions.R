## THIS SUBMODULE HAS ALL THE NECESSARY FUNCTIONS TO 

# (1) - COMPUTE THE RECHARGE RATES PER AQUIFER
# (2) - COMPUTE PRECIPITATION RUNOFF, SOIL DRAINAGE (BOTH USING MITERRA RF) AND AQUIFER RECHARGE (USING RECHARGE RATES AND PRECIPITATION)
# (3) - COMPUTE THE NO3 CONCENTRATION IN LEACHED WATER

############ READ ME --------------------- #############################
## drainage calculated by aquifer recharge rates + irrigation is not correct but I wont erase these sections
## drainage is now given as a proxy of water balance from below the root zone

source('./GIS_module/Function/compute_GIS_leaching_fractions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./GIS_module/Function/compute_GIS_leaching_pathways.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./WaterBalance_module/Functions/GW_Water_Balance.R')

## ----------------------------------------------------------------------------------------------------------------
## RECHARGE RATE COMPUTATION --------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------------------

get_gw_recharge_df <- function() {
  rech_df <- get_modellingDf_file('df_recharge_rates_gw', 'Recharge')
  return(rech_df)
}

#this is because some polygons are not be rasterized
rasterize_subset <- function(){
  #this is because some polygons are not be rasterized
  
  wrong_sub <- readOGR('./GIS_module/Output/Modelling/Recharge/subset_wrong.shp')
  wrong_sub$df_recha_1 <- as.numeric(wrong_sub$df_recha_1)
  rast <- general_rasterize(wrong_sub, 'df_recha_1')
  return(rast)
}

rasterize_gw_recharge <- function() {
  df <- get_gw_recharge_df()
  
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  colnames(gw@data)[5] <- 'aquifer_ID'
  
  gw <- merge(gw, df, 'aquifer_ID')
  
  rast <- general_rasterize(gw, 'recharge')
  wrong_subset <- rasterize_subset()
  rast <- mosaic(rast, wrong_subset, fun='sum')
  write_raster_modelling(rast, 'rast_recharge_gw', 'Recharge')
}

get_rast_gw_recharge <- function() {
  gw_rech <- get_modelling_files('Recharge', 'rast_recharge_gw')
}

## ----------------------------------------------------------------------------------------------------------------
## get data --------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------------------

get_drainage_rasters <- function(pattern_file, irrig_mode) {
  #gets rasters within the Drainage folder
  # d <- get_drainage_rasters('rf_prec99', 'Default)
  
  drainage_rasters <- get_nloading_gw(pattern_file, irrig_mode, main_subfolder = 'Drainage')
}

get_Nc_rasters <- function(pattern_file, irrig_mode) {
  drainage_rasters <- get_nloading_gw(pattern_file, irrig_mode, main_subfolder = 'Nc')
}

## ---------------------------------------------------------------------------------------
## Write Drainage rasters ------------------------------------------
## ---------------------------------------------------------------------------------------

write_gw_rasters <- function(write, rasterfile, filename, irrig_mode, main_subfolder) {
  if (write==TRUE) {
    write_pathway_leaching(rasterfile, filename, irrig_mode, main_subfolder)
  }
}

## ---------------------------------------------------------------------------------------
## Drainage as calculated with runoff fractions ------------------------------------------
## ---------------------------------------------------------------------------------------

#d <- rf_precipitation_runoff(1999, TRUE, 'Default')
rf_precipitation_runoff <- function(year, write, irrig_mode) {
  rf <- get_GIS_file(paste0('Rf', year_prefix(year)), 'MITERRA')
  prec <- get_precipitation(year, 'rast_caa')
  
  rf_prec <- rf*prec
  write_gw_rasters(write, rf_prec, paste0('rf_prec', year_prefix(year)), irrig_mode, 'Drainage')
  
 return(rf_prec)
}

rf_precipitation_drainage <- function(year, write, irrig_mode) {
  prec <- get_precipitation(year, 'rast_caa')
  rf_prec <- rf_precipitation_runoff(year, FALSE,irrig_mode)
  
  drainage_gw <- prec-rf_prec
  rm(rf_prec)
  write_gw_rasters(write, drainage_gw, paste0('rf_drainage', year_prefix(year)), irrig_mode, 'Drainage')
  
 # return(drainage_gw)
}

## ---------------------------------------------------------------------------------------
## Drainage as calculated with GW recharge rates------------------------------------------
## ---------------------------------------------------------------------------------------

gw_drainage <- function(year, write, irrig_mode)
{
  rech_rates <- get_rast_gw_recharge()/100
  prec <- get_precipitation(year, 'rast_caa')
  
  gw_recharge <- rech_rates*prec
  write_gw_rasters(write, gw_recharge, paste0('gw_recharge', year_prefix(year)), irrig_mode, 'Drainage')
  
  if(write==FALSE){return(gw_recharge)}
}

#corrects drainage based on the adjustment factor
#only to be used in compute_drainage_enetering_gw as this is already computed in Nc
correct_gw_drainage <- function(year, write, irrig_mode)
{
  drainage <- gw_drainage(year, write, irrig_mode)
  adj_factor <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  
  drainage <- drainage*adj_factor*10000
  return(drainage)
}

compute_drainage_entering_gw <- function(year, irrig_mode)
{
  computation_gw_general_func(correct_gw_drainage, year, irrig_mode, 'drainage')
}

## ---------------------------------------------------------------------------------------
## Computation of Nc using both methods --------------------------------------------------
## ---------------------------------------------------------------------------------------

# general function to compute Nc


method_parameters <- function(method) {
  #method can be either 'RF' or 'GW'
  #method_df <- method_parameters('RF')
  #load conditions;; position 1 - drainage specification || position 2 - Nc filename when writing
  
  method_rf <- c('rf_drainage', 'rf_Nc') 
  method_gw <- c('gw_recharge', 'gw_Nc')
  
  if (method=='RF'){return(method_rf)} else if (method=='GW'){return(method_gw)}
}

general_Nc_func <- function(year, write, irrig_mode) {
  # general function to calculate Nc (mg N/L) for each cellgrid
  
  # get water balance for each cellgrid
  water_balance <- raster(select_WB_subfolder_file('Water_surplus', 'MOSAIC', year))*1000 #Litres
  n_loads <- correct_nloads_gw(year, FALSE, irrig_mode) #N-loads are already corrected to the adj factor but in kg N
  adj_factor <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  
  water_balance <- water_balance*adj_factor
  n_loads <- n_loads *1000*1000 #in mg N
  Nc <- n_loads/water_balance
  if (write==TRUE) {
    write_gw_rasters(write, rasterfile = Nc, filename = paste0('Nc_WB_', year_prefix(year)), irrig_mode = irrig_mode, 'Nc')
  }
  return(Nc)
  rm(list=c('water_balance', 'adj_factor', 'n_loads'))
}


## FUNCTION DISREGARDED DUE TO CHANGES IN DRAINAGE (IE RECHARGE) TO WATER BALANCE
general_Nc_func_OLD <- function(year, write, irrig_mode, method) {
  #general function to compute Nc based on method parameters
  

  method_df <- method_parameters(method)
  #load data
  drainage <- get_drainage_rasters(paste0(method_df[1], year_prefix(year)), irrig_mode)
  n_loads <- correct_nloads_gw(year, FALSE, irrig_mode) #N-loads are already corrected to the adj factor but in kg N
  adj_factor <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  
  #conversions
  drainage <- drainage*adj_factor*10000 #10000 is the conversion factor mm*ha to m3 then L
  n_loads <- n_loads *1000*1000 #in mg N
  
  Nc <- n_loads/drainage
  write_gw_rasters(write, Nc, paste0(method_df[2], year_prefix(year)), irrig_mode, 'Nc')
}

#Nc using soil drainage
# wrong
rf_Nc_gw <- function(year, write, irrig_mode)
{
  general_Nc_func_OLD(year, write, irrig_mode, 'RF')
}

#Nc using aquifer recharge rates
# wrong
gw_Nc_gw <- function(year, write, irrig_mode)
{
  general_Nc_func_OLD(year, write, irrig_mode, 'GW')
}

#Nc using WATER BALANCE
wb_Nc_gw <- function(year, write, irrig_mode) {
  ifelse(missing(irrig_mode)==TRUE, irrig <- 'Default', irrig <- 'Irrig')
  general_Nc_func(year, write, irrig)
}

## VERY IMPORTANT
df_compute_gw_Nc <- function(year, write, irrig_mode) {
  # READ: correct approach to calculate NO3 in leached water
  # USES THE WATER BALANCE FROM BELOW THE ROOT ZONE AS A PROXY (see WaterBalance_module)
  ifelse(missing(irrig_mode)==TRUE, irrig <- 'Default', irrig <- 'Irrig')
  
  gw_WB <- read.csv(select_WB_subfolder_file('GW', 'df', year)) #in litres
  nload_df <-get_modellingDf_file(paste0('nload_df_gw', year_prefix(year)), 'Pathway_leaching', irrig)[, -1] #in kg N
  
  gw_WB$n_load_kgN <- nload_df$n.load
  gw_WB$Nc_mgNL <- gw_WB$n_load_kgN*1000*1000/gw_WB$wsurplus_L #in mg N/L
  gw_WB$Nc_mgNO3L <- gw_WB$Nc_mgNL*50/11.3 #in mg NO3/L
  if (write==TRUE){write_csv_modelling('Nc', subfolder_pattern = irrig, gw_WB, paste0('nc_df_gw', year_prefix(year)))}
  
  return(gw_WB)
}

## DISREGARDED 
df_compute_gw_nc_OLD <- function(year,write, irrig_mode) {
  #compute Nc of each GW in a dataframe format
  
  ifelse(missing(irrig_mode)==TRUE, irrig <- 'Default', irrig <- 'Irrig')
  rech_df <- get_modellingDf_file(paste0('drainage_df_gw', year_prefix(year)), 'Drainage', irrig)[, -1]
  nload_df <-get_modellingDf_file(paste0('nload_df_gw', year_prefix(year)), 'Pathway_leaching', 'Default')[, -1] #in kg N
  
  Nc <- nload_df$n.load*1000*1000/rech_df$drainage #in mg N/L
  
  df <- cbind(rech_df, nload_df[, 2], Nc)
  df$NcN <- df$Nc*50/11.3
  
  colnames(df)[3] <- 'N-loads[mg N]'
  colnames(df)[4] <- 'Nc[mg N/L]'
  colnames(df)[5] <- 'Nc[mg NO3/L]'
  
  if (write==TRUE){write_csv_modelling('Nc', subfolder_pattern = irrig, df, paste0('nc_df_gw', year_prefix(year)))}
  
  return(df)
}

get_df_Nc <- function(year, irrig_mode) {
  
  ifelse(missing(irrig_mode)==TRUE, irrig <- 'Default', irrig <- 'Irrig')
  nc_df <- get_modellingDf_file(paste0('nc_df_gw', year_prefix(year)), 'Nc', irrig)[, -1]
  return(nc_df)
}

get_main_df_gw_dataset <- function(year, write) {
  gw_df <- get_df_Nc(year, irrig_mode = 'Irrig')
  rech <- get_gw_recharge_df()
  colnames(rech)[1] <- 'GW_ID'
  #get HUs
  hydro <- select_maindata_pattern('Hydrogeological')
  hydro_df <- read.csv(list.files(hydro, full.names = T))
  
  gw_df <- merge(gw_df, c(hydro_df, rech), 'GW_ID', all.x=F)
  if (write==TRUE){write_csv_modelling('Nc', 'Default', gw_df, paste0('dataset_gw_', year_prefix(year)))}
  return(gw_df)
}

