source('./WaterBalance_module/Functions/Water_Balance_funcs.R')

library(foreach)
library(doParallel)


create_gw_df <- function() {
  # creates the baseline gw df with aquifer names and IDs
  
  main_df <- list.files(select_maindata_pattern('Main_sheet'), pattern='gw', full.names=T)
  gw_df <- read.csv(main_df)
  return(gw_df)
}

compute_gw_WaterBalance_df <- function(year, gw_shp, r_wb, gw_df) {
  # computes the Water Balance for each GW body
  # output: main gw df
  # unit : Litre
  cl <- makeCluster(2)
  registerDoParallel(cl)
  
  gw_wb <- foreach(i=1:93, .packages=c('rgdal', 'rgeos', 'raster'), .combine=rbind)%dopar% {
    
    sb_gw <- subset(gw_shp, GW_ID==i)
    r_mosaic <- crop(r_wb, extent(sb_gw))
    r_mask <- mask(r_mosaic, sb_gw)
    wsurplus_L <- round(cellStats(r_mask, 'sum'), 0)*1000
    data.frame(gw_df[i,], wsurplus_L)
  }
  stopCluster(cl)
  write_wb_output(file = gw_wb, filename = paste0('GW_wb_df', year_prefix(year)), output_folder = 'GW')
  return(gw_wb)
  rm(list=c('r_mosaic', 'r_mask'))
}

compute_gw_WaterBalance <- function(year) {
  # computes the water balance for each groundwater body
    # (1) - in a dataframe (in litres)
    # (2) - in raster format (in litrees)
  
  # data dump
  gw_df <- create_gw_df()
  r_WB <- raster(select_WB_subfolder_file('Water_surplus', 'MOSAIC', year))
  adj_factor <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  r_WB <- r_WB * adj_factor
  gw_shp <- spTransform(load_shp('gw'), proj4string(r_WB))
  
  # compute WB for each aquifer (dataframe) and export these data
  gw_WB_df <- compute_gw_WaterBalance_df(year = year, gw_shp = gw_shp, r_wb = r_WB, gw_df = gw_df)
  # rasterize WB data for each GW
  gw_shp <- merge(gw_shp, gw_WB_df, 'GW_ID')
  r_gw <- general_rasterize(gw_shp, 'wsurplus_L')
  write_wb_output(file = r_gw, filename = paste0('GW_wb_r', year_prefix(year)), output_folder = 'GW')
  rm(list=c('gw_shp', 'r_WB', 'gw_WB_df'))
}

