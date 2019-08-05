source('./GIS_module/Function/LU_irrigation_allocation.R')
source('./GIS_module/Function/GW_computation_functions.R')

## CORRECT DRAINAGE --------------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------------------

load_aquifer_recharge_meters <- function(year, volume, write) {
  # this function computes the total drainage for (i) Nc calculatioen and (ii) leaching risk index
  # note that the depth of irrigation percolation and water recharge rates are different
  # this fucntion can return total drainage in either meters or cubic meters
  # e.g. d <- load_aquifer_recharge_meters(2009, FALSE, TRUE)
  
  #load aquifer recharge (in mm but then converted to meters)
  aq_rech_adj_fact <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  aq_recharge_mm <- get_drainage_rasters(paste0('gw_recharge', year_prefix(year)), 'Default')
  aq_recharge_m <- aq_recharge_mm*aq_rech_adj_fact/1000

  #load irrigation percolation (in meters)
  irrig_perco_m <- get_drainage_rasters(paste0('irrig_perco', year_prefix(year)), irrig_mode = TRUE)
  
  total_drainage <- irrig_perco_m + aq_recharge_m
  if(volume==TRUE) {total_drainage <- total_drainage*10000}
  #rm(list = list(irrig_perco_m, aq_recharge_m, aq_recharge_mm))
  
  if (write==TRUE) {
    ifelse(volume==TRUE, unit <- 'm3', unit <- 'm')
    write_gw_rasters(write = TRUE, rasterfile = total_drainage, filename = paste0('total_drainage_', unit, year_prefix(year)), 
                     irrig_mode = TRUE, main_subfolder = 'Drainage')
  }
  return(total_drainage)
}

loop_func_total_drainage <- function() {
  
  year <- c(1999, 2009)
  volume <- c(TRUE, FALSE)
  
  for (i in year) { 
    print(paste0('Working in ', i)) 
    for (j in volume) { 
    load_aquifer_recharge_meters(i, j, T)
    }
  }
}

## CORRECT Nc --------------------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------------------------------

load_correct_Nc_mgNL <- function(year) {
  
  aq_recharge_L <- get_drainage_rasters(paste0('total_drainage_m3', year_prefix(year)), TRUE)*1000 #in litres/yr
  n_loads_mgN <- correct_nloads_gw(year, FALSE, 'Default')*1000*1000 #in mg N
  
  Nc <- n_loads_mgN/aq_recharge_L
  #return(Nc)
  write_gw_rasters(TRUE, Nc, paste0('corrected_Nc', year_prefix(year)), 'Default', 'Nc')
}

get_total_drainage_litre <- function(year, write, irrig_mode) {
  # this function is an adapted version of get_drainage_rasters to fit the "general" function computation_gw_general_func
  
  tot_drainage <-  get_drainage_rasters(paste0('total_drainage_m3', year_prefix(year)), TRUE)*1000
  if (write==FALSE) {return(tot_drainage)}
}


dd <- computation_gw_general_func(compute_func = get_total_drainage_litre, year = 2009, irrig_mode = TRUE, parameter = 'drainage_L')
d <- df_compute_gw_nc(1999, TRUE)
