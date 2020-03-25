source('./GIS_module/Function/Compute_correct_GW_parameters.R')
source('./Leaching_risk_index/Functions/Computation_MeanResidenceTime.R')
source('./GIS_module/Function/GW_computation_functions.R')


NC_reclass_index <- function() {
  #general reclass dataframe
  
  rcl_df <- data.frame(MRT=c('<10', '10-25', '25-50', '50-75', '>75'),
                       rcl <- c(0.2,0.4,0.6,0.8,1))
  return(rcl_df)
}

NC_reclass <- function() {
  # raster reclass vector  
  
  rcl <- c(0, 10, 0.2, 10, 25, 0.4, 25, 50, 0.6, 50, 75, 0.8, 75, +Inf, 1)
  return(rcl)
}

# discontinued in the recent version
get_NC_df <- function(year, irrig_mode) {
  #gets the Nc dataset for each GW from GIS module
  
  ifelse(irrig_mode==TRUE, irrig <- 'Irrig', irrig <- 'Default')
  nc_df <- get_modellingDf_file(file_pattern = paste0('nc_df_gw', year_prefix(year)), folder_name = 'Nc', irrig_mode = irrig)
  return(nc_df)
}

# discontinued in the recent version
create_gw_shapefile_Nc <- function(year, irrig_mode) {
  # creates a shapefile with Nc for each aquifer
  
  gw_nc <- get_NC_df(year, irrig_mode)
  gw <- load_shp('gw')
  gw <- spTransform(gw, 
                    CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  
  gw <- merge(gw, gw_nc, 'GW_ID')
  return(gw)
}

# discontinued in the recent version
NC_raster_reclassifiction <- function(year, irrig_mode) {
  # creates the reclassification raster for GW NC
  
  path <- create_index_dir('GIS_index')
  gw_mrt_shp <- create_gw_shapefile_Nc(year, irrig_mode)
  gw_r <- general_rasterize(shp = gw_mrt_shp, fiel_rast = 'Nc_mgNO3L')
  gw_r <- reclassify(gw_r, rcl=NC_reclass())
  
  write_index('GIS_index', filename = paste0('NC_reclass', year_prefix(year)), file = gw_r, format = 'raster')
}


compute_hazard <- function(year) {
  
  #  caa <- reclassify(get_module_subfolder_output(module = 'GIS', submodule = 'LandCover', file_pattern = paste0('caaRP', year_prefix(year))),
  #                   rcl = c(0,1,0))
  nc <- raster(paste0('./GIS_module/Output/Modelling/Nc/Default/Nc_WB_', year_prefix(year), '.tif'))
  haz <- reclassify(nc, rcl = NC_reclass())
  # haz[is.na(haz[])] <- 0
  # haz <- haz + caa
  
  write_index('GIS_index', filename = paste0('Hazard', year_prefix(year)), file = haz, format = 'raster')
  rm(list=c('nc', 'haz'))
}



loop_NC_reclass <- function(irrig_mode) {
  #loops around the annual reclassification of GW MRT
  
  year <- c(1999, 2009)
  sapply(year, function(x) NC_raster_reclassifiction(x, irrig_mode))
}



nleach09 <- raster('./GIS_module/Output/Modelling/Total_leaching/tot_leaching09.tif') * 1000 * 1000 # kg 
wb09 <- raster('./WaterBalance_module/Output/Water_surplus/MOSAIC_WS09.tif') * 1000 # L 

nleach99 <- raster('./GIS_module/Output/Modelling/Total_leaching/tot_leaching99.tif') * 1000 * 1000 # kg 
wb99 <- raster('./WaterBalance_module/Output/Water_surplus/MOSAIC_WS99.tif') * 1000 # L 

nc09 <- nleach09/wb09 * 50/11.3
nc09[nc09>5000] <- -1
nc09 <- reclassify(nc09, rcl = c(-Inf,0,0,
                             0,10,0.2,
                             10,25,0.4,
                             25,50,0.6,
                             50,75,0.8,
                             75,+Inf,1))
nc09 <- mask_aquifer_reclass_index(nc09)

nc99 <- nleach99/wb99 * 50/11.3
nc99[nc99>5000] <- -1
nc99 <- reclassify(nc99, rcl = c(-Inf,0,0,
                             0,10,0.2,
                             10,25,0.4,
                             25,50,0.6,
                             50,75,0.8,
                             75,+Inf,1))
nc99 <- mask_aquifer_reclass_index(nc99)

vul09 <- raster('./Leaching_risk_index/Output/GIS_index/Vulnerability_09.tif')
vul09 <- mask_aquifer_reclass_index(nc99)

vul99 <- raster('./Leaching_risk_index/Output/GIS_index/Vulnerability_99.tif')
vul99 <- mask_aquifer_reclass_index(vul99)


ri09 <- nc09 * vul09
ri09 <- mask_aquifer_reclass_index(ri09)

ri99 <- nc99 * vul99
ri99 <- mask_aquifer_reclass_index(ri99)


nc99 <- basic_r_plot(nc99, 'Hazard', '1999', gw)
nc09 <- basic_r_plot(nc09, 'Hazard', '2009', gw)
vul99 <- basic_r_plot(vul99, 'Vulnerability',  '1999', gw)
vul09 <- basic_r_plot(vul09, 'Vulnerability', '2009', gw)
ri99 <- basic_r_plot(ri99,  'Risk', '1999', gw)
ri09  <- basic_r_plot(ri09, 'Risk', '2009', gw)

m_plot <- tmap_arrange(nc99, nc09, vul99, vul09, ri99, ri09, ncol=2, nrow=3)
tmap_save(m_plot,'./cat_allv2.pdf', dpi=100, height = 10, width = 7)

compute_hazard <- function(year) {
  
#  caa <- reclassify(get_module_subfolder_output(module = 'GIS', submodule = 'LandCover', file_pattern = paste0('caaRP', year_prefix(year))),
 #                   rcl = c(0,1,0))
  nc <- raster(paste0('./GIS_module/Output/Modelling/Nc/Default/Nc_WB_', year_prefix(year), '.tif'))
  haz <- reclassify(nc, rcl = NC_reclass())
 # haz[is.na(haz[])] <- 0
 # haz <- haz + caa
  
  write_index('GIS_index', filename = paste0('Hazard', year_prefix(year)), file = haz, format = 'raster')
  rm(list=c('nc', 'haz'))
}

loop_hazard <- function() {
  #loops around the annual reclassification of GW MRT
  
  year <- c(1999, 2009)
  sapply(year,  compute_hazard)
}


compute_hazard_gw <- function(year) {
  
  hazard <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Hazard', year_prefix(year))))
  gw <- load_shp('gw')
  gw <- subset(gw, gw_data0_1=='Aq')
  gw <- spTransform(gw, proj4string(hazard))
  gw <- st_as_sf(gw)
  
  ids <- gw$GW_ID
  df <- data.frame(GW_ID = ids)
  gw_hazard <- exactextractr::exact_extract(hazard, gw, 'mean')
  df <- cbind(df, gw_hazard)
  names(df)[2] <- 'Hazard'
  
  gw <- merge(gw, df, 'GW_ID')
  return(gw)
  rm(list=c('RI_npo', 'df', 'ctr', 'sb_gw', 'sb_ri'))
}

