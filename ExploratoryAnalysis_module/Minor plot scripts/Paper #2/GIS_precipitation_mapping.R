library(raster)
library(ncdf4)
library(tmap)
library(GADMTools)
library(rworldmap)
library(rworldxtra)

world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')

source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')

#################################################################################################
#=============================== PLOT PRECIPITATION MAPS =====================================#

#load precipitation data
p99 <- './Activity data/Climatic_data/Precipitation/rast_p99.tif'
p09 <- './Activity data/Climatic_data/Precipitation/rast_p09.tif'

#load raster data
prec99 <- raster(p99)
prec09 <- raster(p09)

#load muni shp data
muni_shp <-load_shp('Muni')
muni_shp <- spTransform(muni_shp, proj4string(prec09))
#downscale spatial resolution by a factor of 5
pp09 <- aggregate(prec09, fact=5, fun=mean)
pp99 <- aggregate(prec99, fact=5, fun=mean)

#plot individual precipitation maps
p_99 <- prec_map(pp99, '1999')
p_09 <- prec_map(pp09, '2009')

#zonal statistics
z_stat99 <- zonal_statistic(muni_shp, pp99, 'rast_p99')
z_stat09 <- zonal_statistic(muni_shp, pp09, 'rast_p09')

#GIS PLOT with histogram
prec09 <- gis_prec_map_hist(z_stat09, 'rast_p09', 'Precipitation 2009')
prec99 <- gis_prec_map_hist(z_stat99, 'rast_p99', 'Precipitation 1999')


#ind_gw_inters <- load_shp('ind_aquifer')
gw <- load_shp('gw')
gw <- spTransform(gw, proj4string(pp09))
colnames(gw@data)[ncol(gw)] <- 'aquifer_ID'
hydro <- load_shp('main_hydro')
hydro <- spTransform(hydro, proj4string(pp09))

p <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey') + 
  tm_layout(bg.color='lightblue')
p3 <- p + HU_plot
p2 <- p + prec09  
p1 <- p + prec99

# map gw within HU
HU_plot <- tm_shape(hydro, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_polygons(col='nome', palette = c('orange', 'red1', 'green1', 'yellow1'), title = 'Main HUs') + 
  tm_shape(gw,  projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_legend(show=T, position=c(0.68, 0.3), frame=F)+
  tm_scale_bar(color.dark = 'black', text.color = 'black',
               position=c(0.57, 0.009), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.5,
             position=c(0.025, 0.9)) +
  tm_layout(frame=T,
            legend.text.size = 0.9,
            panel.show = T,
            legend.title.size = 1.2,
            panel.labels = 'Groundwater bodies',
            fontfamily = 'serif',
            legend.width = 2.3,
            panel.label.height = 1.2,
            #bottom, left, top, right
            inner.margins = c(0, 0, 0, 0))




path <- plot_output()
main_plot <- tmap_arrange(p1, p2, p3, ncol=3)
tmap_save(main_plot, filename = paste0(path, 'TMAP_PRECIP_HUs_new.pdf'), dpi=600, height = 6.5, width = 12)
