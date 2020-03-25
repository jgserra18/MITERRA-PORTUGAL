source('./WaterBalance_module/Functions/Water_Balance_funcs.R')

irrig99 <- raster(select_WB_subfolder_file('Irrigation', 'Gross_irrig_vol', 1999))
irrig09 <- raster(select_WB_subfolder_file('Irrigation', 'Gross_irrig_vol', 2009))
irrig_stack <- stack(irrig99, irrig09)

wb99 <- raster(select_WB_subfolder_file('Water_surplus', 'MOSAIC_WS', 1999))
wb09 <- raster(select_WB_subfolder_file('Water_surplus', 'MOSAIC_WS', 2009))
wb_stack <- stack(wb99, wb09)

prec99 <- raster(list.files(path = prec_folder(), 'rast_caa99', full.names = T))
prec09 <- raster(list.files(path = prec_folder(), 'rast_caa09', full.names = T))

runoff_mosaic_list <- function() {
  # aggregates the seasonal runoff water losses for both years
  # returns a list where idx1 --> 1999 and idx2 --> 2009
  
  r_store <- list()
  season <- c('winter', 'summer', 'autumn')
  year <- c(1999, 2009)
  for (i in year) {
    rf <- lapply(season, function(x) raster(select_WB_subfolder_file('Water_surplus', paste0('Runoff_', x), i)))
    rf$fun <- sum
    rf_m <- do.call(mosaic, rf)
    caa <- get_GIS_file(paste0('caaRP', year_prefix(i)), 'LandCover')
    rf_m <- rf_m*caa
    r_store <- append(r_store, rf_m)
  }
  names(r_store) <- c('runoff99', 'runoff09')
  return(r_store)
  rm(list=c('rf', 'caa', 'rf_m'))
}

rf99 <- runoff_mosaic_list()[[1]]
rf09 <- runoff_mosaic_list()[[2]]
rf_stack <- stack(rf99, rf09)

r_db99 <- raster::stack(irrig99, wb99, prec99,  rf99)
r_db09 <- raster::stack(irrig09, wb09, prec09,  rf09)

compile_WB_data_df <- function() {
  # computes descriptive statistics to the most important parameters in the WB calculation
  
  year <- c(1999, 2009)
  vars_name <- c('irrig', 'wb', 'prec', 'rf')
  df <- data.frame(wb_var=vars_name)
  df99 <- df
  df09 <- df
  
  for (j in year) {
    if (j==1999) {
      for (i in 1:4) {
        avg <- cellStats(r_db99[[i]], 'mean')
        sd <- cellStats(r_db99[[i]], 'sd')
        max <- cellStats(r_db99[[i]], 'max')
        sum <- cellStats(r_db99[[i]], 'sum')
        df99[i, 'avg'] <- round(avg, 0)
        df99[i, 'sd'] <- round(sd, 0)
        df99[i, 'max'] <- round(max, 0)
        df99[i, 'sum'] <- round(sum, 0)
      }
    }
      else if (j==2009) {
        for (i in 1:4) {
        avg <- cellStats(r_db09[[i]], 'mean')
        sd <- cellStats(r_db09[[i]], 'sd')
        max <- cellStats(r_db09[[i]], 'max')
        sum <- cellStats(r_db09[[i]], 'sum')
        df09[i, 'avg'] <- round(avg, 0)
        df09[i, 'sd'] <- round(sd, 0)
        df09[i, 'max'] <- round(max, 0)
        df09[i, 'sum'] <- round(sum, 0)
  
        }
      }
    ifelse(j==1999,
           df99$year <- 1999,
           df09$year <- 2009)
    }
    df <- rbind(df99, df09)
    return(df)
}
d <- compile_WB_data_df()
write_wb_output(file = d, filename = 'WB_data_compilation_df', output_folder = 'DB_compilation')


# main hydro exploratory analysis WB


compute_HU_wb <- function() {
  # 1 - masks the WB rasters to the main aquifer systems
  # 2 - loads the main HU
  # 3 - computes the WB for the main HU, for the main aquifer systems
  
  wb99 <- raster(select_WB_subfolder_file('Water_surplus', 'MOSAIC_WS', 1999))
  wb09 <- raster(select_WB_subfolder_file('Water_surplus', 'MOSAIC_WS', 2009))
  
  aq <- load_shp('gw')
  aq <- subset(aq, gw_data0_1=='Aq')
  aq <- spTransform(aq, proj4string(wb99))
  
  # mask wb rasters
  wb99 <- mask(crop(wb99, extent(aq)), aq)
  wb09 <- mask(crop(wb09, extent(aq)), aq)
  
  hu <- load_shp('main_hydro')
  hu <- spTransform(hu, proj4string(wb99))
  hu_id <- unique(hu$nome)
  hu_df <- data.frame(hu_id=hu_id)
  year <- c(1999, 2009)
  ctr <- 1
  
  for (z in year) {
    ctr <- ctr + 1
    if (z==1999) {wbb <- wb99} else {wbb <- wb09}
    idx <- 0
    for (i in hu_id) {
      idx <- idx + 1 
      sb <- subset(hu, nome==i)
      m_wb <- mask(crop(wbb, extent(sb)), sb)
      sum <- cellStats(m_wb, 'sum')/1e6
      hu_df[idx, ctr] <- sum
    }
  }
  colnames(hu_df) <- c('hu_id', '1999', '2009')
  return(hu_df)
  rm(list=c('idx', 'sb', 'm_wb', 'wb99', 'wb09'))
}
d <- compute_HU_wb()
write_wb_output(file = d, filename = 'HU_WB_data.csv', output_folder = 'DB_compilation')


## Show preliminary plots

library(raster)
library(GADMTools)
library(rworldmap)
library(rworldxtra)
library(tmap)

world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Portugal')


r_plot_me <- function(gis_file, title, breaks, filename) {
  
  p <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey85') + 
    tm_layout(bg.color='lightblue') + tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
    tm_shape(gis_file, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_raster(title=title, style='cont', breaks = breaks, 
                                  palette = c('blue1', 'green1', 'yellow1', 'red1'), legend.is.portrait=F, legend.show=TRUE) +
    tm_layout(legend.outside = TRUE, legend.outside.position = 'bottom', legend.position = c(0.45, 0.7), 
              outer.margins = c(0, 0, 0 ,0), frame=T, bg.color='lightblue',
              panel.labels = c('1999', '2009')) + 
    tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7, 
                 position=c(0.75, 0.03), breaks = c(0, 50, 100)) + 
    tm_compass(type='4star', size=1.1, text.size = 0.8,
               position=c(0.1, 0.9))
  plot <-  p +  tm_layout(legend.position=c("center", "center"),
                       legend.outside = TRUE,
                       legend.outside.position = 'bottom',
                       legend.text.size = 0.8, legend.title.size = 1.1, 
                       legend.outside.size = 0.2) +
    tm_layout( attr.position=c(1,1), 
               attr.just = c("center", "center"),
               attr.outside.position = "bottom",
               attr.outside.size = 1,
               fontfamily = 'serif', panel.label.size = 1, panel.label.height = 1)
  
  #write output file
  path <- './ExploratoryAnalysis_module/Results/Paper#2/Supplementary_material/'
  tmap_save(plot, paste0(path, filename),  dpi =600, height = 6.5, width = 8) #width =8 by default +
}

p1 <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey85') + 
  tm_layout(bg.color='lightblue') + tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_shape(wb99, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(style='cont', breaks = c(0, 250, 500, 1000, 5000, 16000), palette = c('blue1', 'green1', 'yellow1', 'red1'), legend.is.portrait=F, legend.show=F) +
  tm_layout(legend.outside = TRUE, legend.outside.position = 'bottom', legend.position = c(0.45, 0.7), 
            outer.margins = c(0, 0, 0 ,0), frame=T, bg.color='lightblue', legend.stack = 'horizontal',
            panel.labels = c('1999', '2009'),  fontfamily='serif',) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7, 
               position=c(0.6, 0.03), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.8,
             position=c(0.1, 0.9))

p2 <- tm_shape(sb, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_fill(col='grey85') + 
  tm_layout(bg.color='lightblue') + tm_shape(pt, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + tm_borders(col='black') + 
  tm_shape(wb09, projection = 'longlat', bbox = c(-9.8, 36.8, -5.5, 42.25)) + 
  tm_raster(title='Percolated water\n(m3/ha/yr)', style='cont', breaks = c(0, 250, 500, 1000, 5000, 16000), 
            palette = c('blue1', 'green1', 'yellow1', 'red1'), legend.is.portrait=T, legend.show=TRUE) +
  tm_layout(legend.outside = F,  legend.position = c(0.7, 0.35), fontfamily='serif',
            outer.margins = c(0, 0, 0 ,0), frame=T, bg.color='lightblue', 
            panel.labels = '2009', legend.title.size = 1, legend.text.size = 0.9) + 
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.7, 
               position=c(0.6, 0.03), breaks = c(0, 50, 100)) + 
  tm_compass(type='4star', size=1.1, text.size = 0.8,
             position=c(0.1, 0.9))

pp <- tmap_arrange(p1, p2, ncol=2)
path <- './ExploratoryAnalysis_module/Results/Paper#2/Supplementary_material/perc_plot.tiff'
tmap_save(tm = pp, filename = path,  dpi =600, height = 6.5, width = 8) #width =8 by default +

wb_plot <- r_plot_me(wb_stack, 'Percolated water\n(m3/ha/yr)', breaks = c(0, 250, 500, 1000, 5000, 16000), 'wb_water_plot.tiff')
rf_plot <- r_plot_me(rf_stack, 'Runoff (m3/ha/yr)', breaks = c(0, 25, 50, 100, 500, 3800), 'runoff_water_plot.pdf')
irrig_plot <- r_plot_me(irrig_stack, 'Irrigation (m3/ha/yr)', breaks = c(0, 100, 250, 1000, 4000, 9300), 'gross_irrig_plot.pdf')
