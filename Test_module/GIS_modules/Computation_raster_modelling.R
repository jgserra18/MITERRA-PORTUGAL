source('./GIS_module/Function/General_GIS_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')

## Compute the SSNB at the CAA scale
ssnb99 <- raster_ssnb_caa(1999, 'tier2_irrig', write=TRUE)
ssnb09 <- raster_ssnb_caa(2009, 'tier2_irrig', write=TRUE)

## Compute and write leaching
leach99 <- compute_leaching_caa('ssnb99', 1999, write = TRUE)
leach09 <- compute_leaching_caa('ssnb09', 2009, write = TRUE)
