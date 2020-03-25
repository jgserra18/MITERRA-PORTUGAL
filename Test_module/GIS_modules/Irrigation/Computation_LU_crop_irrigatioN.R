source('./GIS_module/Function/LU_irrigation_allocation.R')
source('./GIS_module/Function/Irrigation/Net_irrigation_N_module.R')

## ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## ##

## ## ## ## COMPUTATION OF IRRIGATION FOR INDIVIDUAL CROPS AND CROP CATEGORIES ## ## ## ## 
## ## ## ## DIFFERENT LU CLASSES ARE DISAGGREGATED ACCORDING TO PAPER2 ## ## ## ## 

#allocation routine - LU to heterogeneous areas and corrected
# optional
allocate_rout09 <- loop_allocation_LU_routine(2009)

# compute the sum of crop irrigation N and allocate to LU class (except heterogeneous)
# irrigation N is computed using statistical data 
# optional
irrigN_perma_irrig09 <- compute_cropN_LU(year = 2009, LU_class = 'perma_irrigated')

# irrigation N to heterogeneous areas, includes: 
  # default assumptions to distribute 4 main LU classes onto heterogeneous areas
  # allocates "surplus" statistical areas according to 2 different conditions expressed in allocation_LU_routine()
irrigN_hetero09 <- compute_cropN_heterogeneous(year = 2009)

# sums total irrigation N at the municipality scale based on LU allocations
irrigN_LU_muni09 <- aggregate_irrigatioN_LU_municipality(year = 2009)

# check balance between irrigation N LU allocation and that from statistical modelling at the municipality scale
irrigatioN_check_balance(year = 2009)

# VERY IMPORTANT
# computes irrigation N (in kg N/LU ha) for each LU and aggregates these into one main dataset
irrigNha_LU_muni09 <- compute_irrigation_nha_muni_LU(year = 2009)

# LU MODELLING - irrigation N (in kg N/LU ha) 
# writes to gross irrigation
allocate_LU_irrigN(year = 2009)

# aggregate into one mosaic
LU_gross_irrigN_mosaic(year = 2009)

## RESULTS DATA ------------------------------------------------------------------

##  MAIN CROP HIERARCHY
main.crop_hierarchy09 <- compute_maincropN_hierarchy(year = 2009)

## CROP HIERARCHY
crop_hierarchy09 <- compute_cropN_hierarchy(year = 2009)


# compute irrigation N of a specified LU class (kg N)
#example
#optional
non99 <- compute_cropN_LU(year = 1999, LU_class = 'non')

# compute irrigation N of a specified LU class (kg N/ha)
#example
#optional
non99 <- compute_cropNha_LU(year = 1999, LU_class = 'non')

## LOOP AROUND ALL LU CLASSES AND COMPUTE CROPNHa (kg N/Ha)
allocate_LU_irrigN(1999)
allocate_LU_irrigN(2009)

## CREATE RASTER MOSAIC OF DIFFERENT LU CLASSES (kg N/ha)
LU_gross_irrigN_mosaic(1999) # I dont tuink so 1999
LU_gross_irrigN_mosaic(2009)

## -----------------------------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------------------------
## Compute net irrigation N for each irrigation system 
compute_net_irrigatioN_irrig_sys(year = 2009, eff_temporal = FALSE)