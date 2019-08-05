source('./GIS_module/Function/LU_irrigation_allocation.R')

## ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## ##

## ## ## ## THIS IS FOR PAPER#2 WHERE A VERY SIMPLISTIC APPROACH IS USED ## ## ## ## 

# for instance, runoff does not account for different irrigation systems but only f_runoff
# this will thus affect water percolation


## IMPORTANT NOTE ----------------------------------------------------

## irrig_eff can be either missing, static, temporal, runoff or leaching

## GROSS/NET WATER REQUIREMENTS ------------------------------------------
## -------------------------------------------------------------------

#The computation of gross water requirements could use a little bit of 
# tweak and implementation of a loop_through_all function
# but for the sake of my understanding in the future I will hardcode it here
## Function that : 
## 1 - Computes water volume (in m3) and accounts for LU area
## giving an output in m3/ha per LU class
## 2 - ALlocates this inputo to LU classes based on CLC for both years

# compute gross water volume in (m3/ha)
# just an example
static_non_irrigated99 <- compute_LU_m3_ha(LU_class = 'non', year = 1999, irrig_eff = 'static')

## compute and allocate gross and net water volume (m3/ha) for each LU class 
year <- c(1999, 2009)
effs <- c('static', 'temporal')

for (i in year) {
  for (j in effs) {
    #allocate_LU_gross <- allocate_LU_volume_GIS(year = i)
    allocate_LU_net <- allocate_LU_volume_GIS(year = i, irrig_eff = j)
    map_LU_GIS(i, 'Volumes', 'net_irrigation', file_pattern = paste0(j, '_LU_vol'), write = TRUE)
  }
}

## create the raster moscaid for gross water volume
## aggregtes every separate LU into one raster
map_LU_GIS(1999, 'Volumes', 'net_irrigation', file_pattern = 'static_LU_vol', write = TRUE)
map_LU_GIS(2009, 'Volumes', 'gross_irrigation', file_pattern = 'LU_vol', write = TRUE)

## create the raster moscaid for net water volume
loop_map_net_vol_LU_GIS()

## WATER RUNOFF ------------------------------------------------------
## -------------------------------------------------------------------

## Computes generic runoff losses without taking account different irrig systems
compute_LU_runoff_vol(1999)
compute_LU_runoff_vol(2009)

## createes runoff LU class mosaic
LU_runoff_vol_mosaic(1999)
LU_runoff_vol_mosaic(2009)

## WATER PERCOLATION ------------------------------------------------------
## -------------------------------------------------------------------

year <- c(1999, 2009)
#effs <- c('static', 'temporal')
effs <- 'static'
for (i in year) { for (j in effs) {
  allocate_LU_percolation <- compute_LU_percolation_vol(i, j)
}}

# apply adjustment factors to irrigation losses through percolation
# and export it to drainage/irrigation subfolder
export_irrig_percolation_drainage_output(1999)
export_irrig_percolation_drainage_output(2009)
