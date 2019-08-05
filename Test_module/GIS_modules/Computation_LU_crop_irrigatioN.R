source('./GIS_module/Function/LU_irrigation_allocation.R')

## ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## #### ## ## ## ##

## ## ## ## COMPUTATION OF IRRIGATION FOR INDIVIDUAL CROPS AND CROP CATEGORIES ## ## ## ## 
## ## ## ## DIFFERENT LU CLASSES ARE DISAGGREGATED ACCORDING TO PAPER2 ## ## ## ## 

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
LU_gross_irrigN_mosaic(1999)
LU_gross_irrigN_mosaic(2009)


