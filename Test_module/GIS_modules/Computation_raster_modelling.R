source('./GIS_module/Function/General_GIS_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')

## Compute the SSNB at the CAA scale
ssnb99 <- raster_ssnb_caa(1999, 'tier2_irrig', write=FALSE)
ssnb09 <- raster_ssnb_caa(2009, 'tier2_irrig', write=FALSE)

## Compute and write leaching
leach99 <- compute_leaching_caa('SSNB99', 1999, FALSE)
leach09 <- compute_leaching_caa('SSNB09', 2009, FALSE)