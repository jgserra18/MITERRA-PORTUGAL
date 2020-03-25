source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')
source('./Leaching_module/Function/compute_correct_surface_water_areas.R')
source('./NLoading_module/Function/Compute_gw_loadings.R')
source('./GIS_module/Function/GW_computation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./Leaching_risk_index/Functions/Computation_Nc.R')
source('./Leaching_risk_index/Functions/Computation_Risk_index.R')
source('./Leaching_risk_index/Functions/Computation_MeanResidenceTime.R')

library(raster)
library(GADMTools)
library(rworldmap)
library(rworldxtra)
library(tmap)

world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Portugal')

p <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey85') + 
  tm_layout(bg.color='lightblue')
gw <- load_shp('gw')

# load Nc for each GW
db09 <- create_gw_shapefile_Nc(year = 2009, irrig_mode = FALSE)
db99 <- create_gw_shapefile_Nc(year = 1999, irrig_mode = FALSE)

db99 <- subset(db99, GW_type=='Aq')
db09 <- subset(db09, GW_type=='Aq')

idx99 <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(1999))))
idx09 <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(2009))))

mrt99 <- raster(get_risk_data(subfolder = 'GIS_index', filename = 'MRT_reclass99'))
mrt99 <- mask(crop(mrt99, extent(db09)), db09)

mrt09 <- raster(get_risk_data(subfolder = 'GIS_index', filename = 'MRT_reclass09'))
mrt09 <- mask(crop(mrt09, extent(db09)), db09)


p1 <- p + tm_shape(gw, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_shape(db99) + 
  tm_polygons(col='Nc_mgNO3L',
              breaks = c(0, 25, 50, 75, 100, +Inf), 
              textNA = NULL, 
              title = expression("Nc (mg NO"[3]^-{}*" L"^-1*")"),
              palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
              labels = c('<25', '25-50', '50-75', '75-100', '>100')) + 
  tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1, text.size = 1,
             position=c(0.1, 0.9)) +
  tm_layout(legend.title.size =0.9, legend.text.size=0.7, panel.label.size = 1.1, # legend.width=2, 
            frame=T,                                 
            fontfamily = 'serif', panel.label.height = 1.1, panel.labels='1999')
p2 <- p + tm_shape(gw, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_shape(db09) + 
  tm_polygons(col='Nc_mgNO3L',
              breaks = c(0, 25, 50, 75, 100, +Inf), 
              textNA = NULL, 
              title = expression("Nc (mg NO"[3]^-{}*" L"^-1*")"),
              palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
              labels = c('<25', '25-50', '50-75', '75-100', '>100')) + 
  tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1, text.size = 1,
             position=c(0.1, 0.9)) +
  tm_layout(legend.title.size =0.9, legend.text.size=0.7, panel.label.size = 1.1, # legend.width=2, 
            frame=T,                                 
            fontfamily = 'serif', panel.label.height = 1.1, panel.labels='2009')

p3 <- p +   tm_shape(idx99, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(col='Leaching_index99',
              breaks = c(0, 1, 2, 3, 3.5), 
              labels = c('<1', '1-2', '2-3', '3-3.5'),
              title = expression('RI'[NPo]),
              palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1')) + 
  tm_shape(gw, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1, text.size = 1,
             position=c(0.1, 0.9)) +
  tm_layout(legend.title.size =0.9, legend.text.size=0.7, panel.label.size = 1.1, # legend.width=2, 
            frame=T,                                 
            fontfamily = 'serif', panel.label.height = 1.1, panel.labels='1999')

p4 <- p + tm_shape(idx09, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(col='Leaching_index09',
            breaks = c(0, 1, 2, 3, 3.5), 
            labels = c('<1', '1-2', '2-3', '3-3.5'),
            title = expression('RI'[NPo]),
            palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1')) + 
  tm_shape(gw, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1, text.size = 1,
             position=c(0.1, 0.9)) +
  tm_layout(legend.title.size =0.9, legend.text.size=0.7, panel.label.size = 1.1, # legend.width=2, 
            frame=T,                                 
            fontfamily = 'serif', panel.label.height = 1.1, panel.labels='2009')

p5 <- p +   tm_shape(mrt99, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(col='MRT_reclass99',
              breaks = c(0, 1, 2, 3, 4, 5), 
              textNA = NULL, 
              title = 'Residence time\n(year)',
              palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
              labels = c('>20', '10-20', '5-10', '3-5', '<3')) + 
  tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1, text.size = 1,
             position=c(0.1, 0.9)) +
  tm_shape(gw, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_layout(legend.title.size =0.9, legend.text.size=0.7, panel.label.size = 1.1, # legend.width=2, 
            frame=T,                                 
            fontfamily = 'serif', panel.label.height = 1.1, panel.labels='1999')

p6 <- p +   tm_shape(mrt09, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(col='MRT_reclass09',
            breaks = c(0, 1, 2, 3, 4, 5), 
            textNA = NULL, 
            title = 'Residence time\n(year)',
            palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
            labels = c('>20', '10-20', '5-10', '3-5', '<3')) + 
  tm_legend(legend.outside=F, legend.position = c(0.7, 0.3)) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7,
               position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1, text.size = 1,
             position=c(0.1, 0.9)) +
  tm_shape(gw, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_layout(legend.title.size =0.9, legend.text.size=0.7, panel.label.size = 1.1, # legend.width=2, 
            frame=T,                                 
            fontfamily = 'serif', panel.label.height = 1.1, panel.labels='2009')


master_plot_nload2(p1, p2, p5, p6, p3, p4, width = 6.5, height = 13, name = 'GIS_Nc_Index.jpeg')
