library(raster)
library(tmap)
library(GADMTools)
library(rworldmap)
library(rworldxtra)

world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')


rf <- raster('./WaterBalance_module\\Output\\Water_surplus\\MOSAIC_WS99.tif')
rff <- raster('./WaterBalance_module\\Output\\Water_surplus\\MOSAIC_WS09.tif')
r <- stack(rf, rff)
tmap_options(max.raster=c(plot= 7500000, view=7500000))

p <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey') + 
  tm_borders(col='black') + 
  tm_shape(r,  projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(title='  Water balance (m3/ha/yr)', style='cont', breaks = c(0, 500, 1000, 1500, 3000, 8000, 16000),
            palette = c('blue', 'green', 'yellow', 'red1'), legend.is.portrait=FALSE, legend.show = TRUE) + 
  tm_layout(legend.outside = TRUE, legend.outside.position = 'bottom', legend.position = c(0.45, 0.7), 
            outer.margins = c(0, 0, 0 ,0), frame=T, bg.color='lightblue',
            panel.labels = c('1999', '2009')) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7, 
               position=c(0.63, 0.03), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.8,
             position=c(0.1, 0.9))
pp <- p +  tm_layout(legend.position=c("center", "center"),
             legend.outside = TRUE,
             legend.outside.position = 'bottom',
             legend.text.size = 0.8, legend.title.size = 1.1, 
             legend.outside.size = 0.2) +
  tm_layout( attr.position=c(1,1), 
             attr.just = c("center", "center"),
             attr.outside.position = "bottom",
             attr.outside.size = 1,
             fontfamily = 'serif', panel.label.size = 1, panel.label.height = 1)
pp

tmap_save(pp, 'WN_annual.pdf', dpi=600, height = 7, width = 7)

