library(raster)
library(ncdf4)
library(tmap)

source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')

#################################################################################################
#=============================== PLOT PRECIPITATION MAPS =====================================#

#load precipitation data
p99 <- './Activity data/Climatic_data/Precipitation/prec99_raster.tif'
p15 <- './Activity data/Climatic_data/Precipitation/prec15.tif'
p09 <- './Activity data/Climatic_data/Precipitation/prec09_raster.tif'
p_hist <- './Activity data/Climatic_data/Precipitation/prec_19712000.nc'

#load raster data
prec_hist <- raster(p_hist)
prec99 <- raster(p99)
prec09 <- raster(p09)
prec15 <- raster(p15)
prec15 <- resample(prec15, prec_hist)



func_resamp <- function()
{
  prec99 <- resample(prec99, prec_hist)
  prec09 <- resample(prec09, prec_hist)
  
  prec_stack <- stack(prec99, prec09)
  return(prec_stack)
}

df_stack <- func_resamp()
diff_99 <- (df_stack[[1]]-prec_hist)
diff_09 <- (df_stack[[2]]-prec_hist)
diff_15 <- prec15-prec_hist




