source('./ExploratoryAnalysis_module/Command function/MAIN_plot_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./GIS_module/Function/LU_irrigation_allocation.R')
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


supp_map_arrange <- function(p1, p2, name, heigth, width, ncol)
{
  path <- './ExploratoryAnalysis_module/Results/Paper#2/Supplementary_material/'
  
  ifelse(missing(ncol)==TRUE, ncol <- 2, ncol <- ncol)
  pl <- tmap_arrange(p1, p2, ncol=2)
  
  print('Saving this now...')
  tmap_save(pl, paste0(path, name), dpi =600, height = 6.5, width = 8) #width =8 by default +
}


mrt_plot <- function(shp, col_map, panel_plot, legend, only_aq) {
  
  if (class(shp)=='SpatialPolygonsDataFrame') {
    
    plot <- tm_shape(shp, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
      tm_polygons(col=col_map, breaks=c(0, 3, 5, 10, 20, +Inf), palette = c('red1', 'orange1', 'yellow1', 'green1', 'blue1'),
                  title='Median residence\ntime (years)',
                  midpoint=6000, labels = c('< 3', '3 - 5', '5 - 10', '10 - 20', '> 20')) + 
      tm_legend(show=legend, position=c(0.67, 0.3), frame=F)+
      tm_scale_bar(color.dark = 'black', text.color = 'black',
                   position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
      tm_compass(type='4star', size=1.1, text.size = 0.7,
                 position=c(0.1, 0.9)) +
      tm_layout(frame=T,
                legend.text.size = 0.9,
                panel.show = T,
                legend.title.size = 1.1,
                panel.labels = panel_plot,
                fontfamily = 'serif',
                panel.label.height = 1)  }
  else { 
    # it is reclassified MRT
    gw <- load_shp('gw')
    if(only_aq==TRUE) {
           gw <- subset(gw, gw_data0_1=='Aq')
           gw <- spTransform(gw, proj4string(shp))
           shp <- mask(crop(shp, extent(gw)), gw)
    }
    plot <- tm_shape(shp, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) +
      tm_raster(col=col_map, breaks=c(0, 1, 2, 3, 4, 5), palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
                  title='Risk score\nRT') + 
      tm_legend(show=legend, position=c(0.67, 0.3), frame=F)+
      tm_shape(gw, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
      tm_shape(pt,projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
      tm_scale_bar(color.dark = 'black', text.color = 'black',
                   position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
      tm_compass(type='4star', size=1.1, text.size = 0.7,
                 position=c(0.1, 0.9)) +
      tm_layout(frame=T,
                legend.text.size = 0.9,
                panel.show = T,
                legend.title.size = 1.1,
                panel.labels = panel_plot,
                fontfamily = 'serif',
                panel.label.height = 1)   }
}

general_plot <- function(raster_map, col_map, title, panel_plot, breaks, legend) {
  
  pt <- getData('GADM', country='PRT', level=0)
  
  plot <- tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='azure4') + tm_fill(col='azure4') + 
    tm_shape(raster_map, projection = 'longlat', bbox=c(-10, 36.8, -5.5, 42.25)) +
    tm_raster(col=col_map, breaks = breaks, style='cont', 
              palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'), 
              #labels=labels,
              title = title) +
    tm_legend(show=legend, position=c(0.7, 0.3), frame=F)+
    tm_scale_bar(color.dark = 'black', text.color = 'black',
                 position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.1, text.size = 0.7,
               position=c(0.1, 0.9)) +
    tm_layout(frame=T,
              legend.text.size = 0.9,
              panel.show = T,
              legend.title.size = 1.1,
              panel.labels = panel_plot,
              fontfamily = 'serif',
              panel.label.height = 1)
}

all_plot <- function(raster_map, col_map, title, panel_plot, breaks, legend) {
  
  plot <- tm_shape(raster_map, projection = 'longlat', bbox=c(-10, 36.8, -5.5, 42.25)) +
    tm_raster(col=col_map, breaks = breaks, style='cont', 
              palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'), 
              #labels=labels,
              title = title) +
    tm_legend(show=legend, position=c(0.7, 0.3), frame=F)+
    tm_scale_bar(color.dark = 'black', text.color = 'black',
                 position=c(0.63, 0.05), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.1, text.size = 0.7,
               position=c(0.1, 0.9)) +
    tm_layout(frame=T,
              legend.text.size = 0.9,
              panel.show = T,
              legend.title.size = 1.1,
              panel.labels = panel_plot,
              fontfamily = 'serif',
              panel.label.height = 1)
}

# ssnb
breaks <- c(0, 50, 100, 200, 700)
ssnb99 <- get_modelling_files('SSNB', 'ssnb99')
ssnb09 <- get_modelling_files('SSNB', 'ssnb09')
p_ssnb99 <- general_plot(ssnb99, 'ssnb99', 'SSNB\n(kg N/ha)', '1999', breaks, F)
p_ssnb09 <- general_plot(ssnb09, 'ssnb09', 'SSNB\n(kg N/ha)', '2009', breaks, T)
ssnb <-  supp_map_arrange(p_ssnb99, p_ssnb09, 'ssnb_supp_fig.pdf')

# runoff fraction
breaks <- c(0, 5, 10, 15, 37.5)
rf99 <- get_GIS_file('Rf99', 'MITERRA_fractions')*100
rf09 <- get_GIS_file('Rf09', 'MITERRA_fractions')*100
p_rf99 <- all_plot(rf99, 'Rf99', 'Runoff fraction\n(% N input)', '1999', breaks, F)
p_rf09 <- all_plot(rf09, 'Rf09', 'Runoff fraction\n(% N input)', '2009', breaks, T)
runoff <- supp_map_arrange(p_rf99, p_rf09, 'runoff_supp_fig.pdf')

# total leaching fraction
breaks <- c(0, 5, 10, 20, 75)
lf99 <- get_GIS_file('Lf99', 'MITERRA_fractions')*100
lf09 <- get_GIS_file('Lf09', 'MITERRA_fractions')*100
p_lf99 <- all_plot(lf99, 'Lf99', 'Leaching fraction\n(% SSNB)', '1999', breaks, F)
p_lf09 <- all_plot(lf09, 'Lf09', 'Leaching fraction\n(% SSNB)', '2009', breaks, T)
leaching <- supp_map_arrange(p_lf99, p_lf09, 'leaching_supp_fig.pdf')

# denitrification maps
breaks <- c(25, 45, 65, 85, 100)
denit99 <- 100-lf99
denit09 <- 100-lf09
p_denit99 <- all_plot(denit99, 'Lf99', 'Denitrification\nfraction\n(% SSNB)', '1999', breaks, F)
p_denit09 <- all_plot(denit09, 'Lf09', 'Denitrification\nfraction\n(% SSNB)', '2009', breaks, T)
denit <- supp_map_arrange(p_denit99, p_denit09, 'denit_supp_fig.pdf')

# gross irrigation
breaks <- c(0, 1000, 2000, 4000, 15000)
i_gross99 <- raster(get_irrig_LU_data(1999, 'Volumes', 'gross_irrigation', 'mosaic'))
i_gross09 <- raster(get_irrig_LU_data(2009, 'Volumes', 'gross_irrigation', 'mosaic'))
p_i99 <- general_plot(i_gross99, 'LU_mosaic_LU_vol', 'Gross irrigation\n(m3/ha)', '1999', breaks, F)
p_i09 <- general_plot(i_gross09, 'LU_mosaic_LU_vol', 'Gross irrigation\n(m3/ha)', '2009', breaks, T)
gross_irrig <- supp_map_arrange(p_i99, p_i09, 'gross_irrig_supp_fig.pdf')




# runoff irrigation
breaks <- c(0, 50, 100, 250, 4100)
i_runoff99 <- raster(get_irrig_LU_data(1999, 'Volumes', 'runoff', 'mosaic'))
i_runoff09 <- raster(get_irrig_LU_data(2009, 'Volumes', 'runoff', 'mosaic'))
p_i_runoff99 <- general_plot(i_runoff99, 'LU_mosaic_LU_vol', 'Runoff irrigation\n(m3/ha)', '1999', breaks, F)
p_i_runoff09 <- general_plot(i_runoff09, 'LU_mosaic_LU_vol', 'Runoff irrigation\n(m3/ha)', '2009', breaks, T)
p_i_runoff09
runoff_irrig <- supp_map_arrange(p_i_runoff99, p_i_runoff09, 'runoff_irrig_supp_fig.pdf')

# irrigation percolation
breaks <- c(0, 50, 100, 250, 5000)
i_leach99 <- raster(get_irrig_LU_data(1999, 'Volumes', 'leaching', 'm3'))
i_leach09 <- raster(get_irrig_LU_data(2009, 'Volumes', 'leaching', 'm3'))

p_i_leach99 <- general_plot(i_leach99, 'LU_percolation_vol_m3', 'Leached water\nirrigation\n(m3/ha)', '1999', breaks, F)
p_i_leach09 <- general_plot(i_leach09, 'LU_percolation_vol_m3', 'Leached water\nirrigation\n(m3/ha)', '2009', breaks, T)
leach_irrig <- supp_map_arrange(p_i_leach99, p_i_leach09, 'leachedWater_irrig_supp_fig.pdf')

# total drainage 
breaks <- c(0, 100, 500, 1000, 2500, 15000)
tot_drainage99 <-  get_drainage_rasters('total_drainage_m399', TRUE)
tot_drainage09 <-  get_drainage_rasters('total_drainage_m309', TRUE)
p_tot99 <- general_plot(tot_drainage99, 'total_drainage_m399', 'Total drainage\n(m3/ha)', '1999', breaks, F)
p_tot09 <- general_plot(tot_drainage09, 'total_drainage_m309', 'Total drainage\n(m3/ha)', '2009', breaks, T)
tot_drainage <- supp_map_arrange(p_tot99, p_tot09, 'tot_drainage_supp_fig.pdf')

# Median RT
mrt99 <- create_gw_shapefile_MRT(1999)
mrt09 <- create_gw_shapefile_MRT(2009)
p_mrt99 <- mrt_plot(mrt99, 'median_mrt', '1999', F)
p_mrt09 <- mrt_plot(mrt09, 'median_mrt', '2009', T)
median_rt <- supp_map_arrange(p_mrt99, p_mrt09, 'gw_MRT_supp_fig.pdf')

# reclassified reclassified RT
mrt99 <- raster(get_risk_data(subfolder = 'GIS_index', filename = 'MRT_reclass99'))
mrt09 <- raster(get_risk_data(subfolder = 'GIS_index', filename = 'MRT_reclass09'))
p_mrt99 <- p + mrt_plot(mrt99, 'MRT_reclass99', '1999', F, only_aq = T)
p_mrt09 <- p + mrt_plot(shp = mrt09, col_map = 'MRT_reclass09', panel_plot = '2009', legend = T, only_aq = T)
median_rt <- supp_map_arrange(p_mrt99, p_mrt09, 'risk_RT_supp_fig.pdf')


p_mrt09
