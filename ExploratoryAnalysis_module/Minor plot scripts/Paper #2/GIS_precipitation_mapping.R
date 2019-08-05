library(raster)
library(ncdf4)
library(tmap)

source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')

#################################################################################################
#=============================== PLOT PRECIPITATION MAPS =====================================#

#load precipitation data
p99 <- './Activity data/Climatic_data/Precipitation/rast_p99.tif'
p09 <- './Activity data/Climatic_data/Precipitation/rast_p09.tif'
p15 <- './Activity data/Climatic_data/Precipitation/Soraia_PS16.tif'

#load raster data
prec15 <- raster(p15)
prec99 <- raster(p99)
prec09 <- raster(p09)

#load muni shp data
muni_shp <-load_shp('Muni')
#downscale spatial resolution by a factor of 5
pp09 <- aggregate(prec09, fact=2, fun=mean)
pp99 <- aggregate(prec99, fact=5, fun=mean)
pp15 <- aggregate(prec15, fact=5, fun=mean)

#plot individual precipitation maps
p_99 <- prec_map(pp99, '1999')
p_09 <- prec_map(pp09, '2009')
p_15 <- prec_map(pp15, '2016')

#zonal statistics
z_stat99 <- zonal_statistic(muni_shp, pp99, 'rast_p99')
z_stat09 <- zonal_statistic(muni_shp, pp09, 'rast_p09')
z_stat15 <- zonal_statistic(muni_shp, pp15, 'Soraia_PS16')

#GIS PLOT with histogram
prec09 <- gis_prec_map_hist(z_stat09, 'rast_p09', 'Precipitation 2009')
prec99 <- gis_prec_map_hist(z_stat99, 'rast_p99', 'Precipitation 1999')
prec15 <- gis_prec_map_hist(z_stat15, 'Soraia_PS16', 'Precipitation 2016')
rech_rate <- rech_rate ##from exploratory_GIS

path <- plot_output()
main_plot <- tmap_arrange(prec99, prec09, rech_rate, ncol=3)
tmap_save(main_plot, filename = paste0(path, 'TMAP_PRECIP_rech.jpeg'), dpi=600, height = 6.5, width = 12)
