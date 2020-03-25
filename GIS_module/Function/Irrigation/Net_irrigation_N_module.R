source('./GIS_module/Function/Irrigation/N2O_N_module.R')
source('./GIS_module/Function/Irrigation/Runoff_N_module.R')

## VERY IMPORTANT ------------------------------------------------
## gross irrigation N aggregated per individual irrigation systems are available in raster format in the activity data
## subfolder in N2O_N


get_irrigN_irrig_sys <- function(year, irrig_system) {
  # this goes to the gross irrigation N subfolder in the N2O activity data folder
  # searches for the mosaic raster of the specified irrigatation system
  # output: raster
  # e.g. get_irrigN_irrig_sys(2009, 'drip')
  
  n2oNha_folder <- create_n2o_subfolders(year = year, n2o_subfolder =  'Activity_data')
  r_mosaic_irrig <- raster(
    list.files(path = n2oNha_folder, pattern = paste0('MOSAIC_', irrig_system), full.names = TRUE))
  return(r_mosaic_irrig)
}


get_irrig_sys_names <- function() {
  irrig_sys <- irrig_sys_assumptions_df()[, 2]
  return(irrig_sys)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------------------

write_net_LU <- function(year, rasterfile, filename) {
  
  path <- create_dir_volumes_path(year, 'N_irrigation', 'static') 
  output_folder <- file.path(path, filename)
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(rasterfile, output_folder, options=tifoptions, format='GTiff')
}

## -----------------------------------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------------------------------

set_irrig_efficiency <- function(year, eff_temporal) {
  # sets the irrigation system efficiencies according to eff_temporal
  # eff_temporal is either TRUE (temporal increase) or FALSE (Static)
  # output: dataframe with irrig sys names and respective efficiency coefficients
  
  ifelse(year==1999,
         idx <- 2,
         idx <- 3)
  ifelse(eff_temporal==TRUE,
         irrig_eff <- get_temporal_irrig_sys_efficiency()[, c(1, idx)], # temporal increase in efficiency (- losses)
         irrig_eff <- get_irrig_sys_efficiency()) #static efficiencies
  
  return(irrig_eff)
}

select_efficiency <- function(year, eff_temporal, irrig_system) {
  # returns the efficiency for the specified irrig_system
  # to be used in compute_net_irrigatioN_irrig_sys
  
  eff_df <- set_irrig_efficiency(year = year, eff_temporal = eff_temporal)
  idx <- which(eff_df$irrig_system==irrig_system)
  irrig_eff <- eff_df[idx, 2]
  return(irrig_eff)
}

compute_net_irrigatioN_irrig_sys <- function(year, eff_temporal) {
  # calls all irrigation system names, creates directory for net_irrigation (ie static or temporal, same shit)
  # loops around irrigation system, calls the respective irrigatioN raster mosaics
  # and calculates the net irrigation N
  
  irrig_sys <- get_irrig_sys_names()
  create_dir_volumes_path(year, 'N_irrigation', 'static') 
  
  for (i in irrig_sys) {
    print(paste0('Working with ', i))
    r_mosaic_irrig <- get_irrigN_irrig_sys(year = year, irrig_system = i)
    irrig_eff <- select_efficiency(year, eff_temporal, i)
    net_irrigN <- r_mosaic_irrig*irrig_eff
    write_net_LU(year = year, rasterfile = net_irrigN, filename = paste0('MOSAIC_', i))
  }
}


