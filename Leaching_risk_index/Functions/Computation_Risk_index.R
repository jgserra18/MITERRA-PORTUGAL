source('./Leaching_risk_index/Functions/Computation_Nc.R')
source('./Leaching_risk_index/Functions/Computation_MeanResidenceTime.R')


compute_risk_index <- function(year) {
  # computes the risk index as the weighted overlay of Nc and MRT of the main aquifer systems in Portugal
  # weights are set to 0.5 for both as these are equally important in diffuse N pollution to groundwater
  
  #load reclassified hazard indexes
  rc_mrt <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Vulnerability_', year_prefix(year))))
  rc_nc <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Hazard', year_prefix(year))))
  
  risk_index <- rc_nc*rc_mrt

  write_index('GIS_index', filename = paste0('Leaching_index', year_prefix(year)), file = risk_index, format = 'raster')
  rm(list=c('rc_mrt', 'rc_nc', 'risk_index'))
}

loop_risk_index <- function() {
  
  year <- c(1999, 2009)
  sapply(year, compute_risk_index)
}


compute_risk_index_df <- function(year) {
  
  RI_npo <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(year))))
  gw <- load_shp('gw')
  gw <- subset(gw, gw_data0_1=='Aq')
  gw <- spTransform(gw, proj4string(RI_npo))
  
  ids <- gw$GW_ID
  df <- data.frame(GW_ID = ids)
  gw_RI <- exactextractr::exact_extract(RI_npo, gw, 'mean')
  df <- cbind(df, gw_RI)
  names(df)[2] <- 'RI'
  
  return(df)
  rm(list=c('RI_npo', 'gw'))
}


compute_RI_gw <- function(year) {
  
  RI_npo <- raster(get_risk_data(subfolder = 'GIS_index', filename = paste0('Leaching_index', year_prefix(year))))
  gw <- load_shp('gw')
  gw <- subset(gw, gw_data0_1=='Aq')
  gw <- spTransform(gw, proj4string(RI_npo))
  gw <- st_as_sf(gw)
  
  ids <- gw$GW_ID
  df <- data.frame(GW_ID = ids)
  gw_RI <- exactextractr::exact_extract(RI_npo, gw, 'mean')
  df <- cbind(df, gw_RI)
  names(df)[2] <- 'RI'
  
  gw <- merge(gw, df, 'GW_ID')
  return(gw)
  rm(list=c('RI_npo', 'df', 'ctr', 'sb_gw', 'sb_ri'))
}

compute_unreclassified_RI <- function(year) {
  
  rt <- raster(paste0('./Leaching_risk_index/Output/GIS_index/Vulnerability_', year_prefix(year),'.tif'))
  rt <- reclassify(rt, rcl=c(0, 3, 1, 3, 5, 0.8, 5, 10, 0.6, 10, 20, 0.4, 20, +Inf, 0.2))
  
  nc <- raster(paste0('./GIS_module/Output/Modelling/Nc/Default/Nc_WB_', year_prefix(year),'.tif'))
  nc <- reclassify(nc, rcl=c(0, 10, 0.2, 10, 25, 0.4, 25, 50, 0.6, 50, 75, 0.8, 75, +Inf, 1))
  
  RI <- rt * nc
  
  RI <- mask_aquifer_reclass_index(RI)
  return(RI)
  rm(list0c('rt', 'nc', 'aq'))
}

master_plot <- function() {
  
  kword <- c('Hazard', 'Vulnerability_', 'Leaching_index')
  yr <- c(1999, 2009)
  store <- list()
  
  for (i in kword) {
    for (j in yr) {
      caa <- reclassify(get_module_subfolder_output(module = 'GIS', submodule ='LandCover', file_pattern = paste0('caaRP',year_prefix(j))),
                        rcl = c(0,1,0))
      r_file <-  raster(get_risk_data(subfolder = 'GIS_index', filename = paste0(i, year_prefix(j))))
      r_file[is.na(r_file[])] <- -999
      r_file <- r_file + caa
      store <- append(store, r_file) 
    }
  }
  store <- stack(store)
  store <- mask_aquifer_reclass_index(store)
  
  return(store)
  rm(list=c('kword', 'yr', 'r_file'))
}

haz09 <- raster('./Leaching_risk_index/Output/GIS_index/Hazard09.tif')
haz09 <- mask_aquifer_reclass_index(haz09)

vul09 <- raster('./Leaching_risk_index/Output/GIS_index/Vulnerability_09.tif')
vul09 <- mask_aquifer_reclass_index(vul09)

RI09 <- haz09 * vul09

haz99 <- raster('./Leaching_risk_index/Output/GIS_index/Hazard99.tif')
haz99 <- mask_aquifer_reclass_index(haz99)

vul99 <- raster('./Leaching_risk_index/Output/GIS_index/Vulnerability_99.tif')
vul99 <- mask_aquifer_reclass_index(vul99)

RI99 <- haz99 * vul99


gw <- spTransform(gw, proj4string(ri99))

basic_r_plot <- function(r_file, titlee, panel.label, gw) {
  
  p <- tm_shape(gw) + tm_fill(col='grey') + tm_borders(col='black') + 
    tm_shape(r_file) + 
    tm_raster(breaks = c(0, 0.0001, 0.2,0.4,0.6,0.8,1), palette = c('black','blue1','green1','yellow1','orange1','red1'), title = titlee,
              labels = c('Perc=0','<0.2','0.2-0.4','0.4-0.6','0.6-0.8','0.8-1')) + 
    tm_layout(panel.labels = panel.label)
  return(p)
}

hazz99
hazz99 <- basic_r_plot(haz99, 'Hazard', '1999', gw)
haz09 <- basic_r_plot(haz09, 'Hazard', '2009', gw)
vul99 <- basic_r_plot(vul99, 'Vulnerability',  '1999', gw)
vul09 <- basic_r_plot(vul09, 'Vulnerability', '2009', gw)
RI99 <- basic_r_plot(RI99,  'Risk', '1999', gw)
RI09  <- basic_r_plot(RI09, 'Risk', '2009', gw)


haz99 <- compute_hazard_gw(1999)
haz09 <- compute_hazard_gw(2009)
vul99 <- compute_gw_vulnerability(1999, FALSE)
vul09 <- compute_gw_vulnerability(2009, FALSE)
ri99 <- compute_RI_gw(1999)
ri09 <- compute_RI_gw(2009)

haz99 <- general_rasterize(haz99, 'Hazard')
haz09 <- general_rasterize(haz09, 'Hazard')
vul99 <- general_rasterize(vul99, 'Vulnerability')
vul09 <- general_rasterize(vul09, 'Vulnerability')
ri09 <- general_rasterize(ri09, 'RI')
ri99 <- general_rasterize(ri99, 'RI')


hazz99 <- tmap_risk(haz99, 'Hazard', 'Hazard', '1999')
hazz09 <- tmap_risk(haz09, 'Hazard', 'Hazard', '2009')
vull99 <- tmap_risk(vul99, 'Vulnerability', 'Vulnerability', '1999')
vull09 <- tmap_risk(vul09, 'Vulnerability', 'Vulnerability', '2009')
rii99 <- tmap_risk(ri99, 'RI', 'Risk', '1999')
rii09 <- tmap_risk(ri09, 'RI', 'Risk', '2009')


m_plot <- tmap_arrange(hazz99, hazz09, vull99, vull09, rii99, rii09, ncol=2, nrow=3)
tmap_save(m_plot,'./cat_all.pdf', dpi=100, height = 10, width = 7)
require(tmap)

tmap_risk <- function(shp_file, col_name, title, label_title) {
  
  p <- tm_shape(shp_file) + 
    tm_polygons(col = col_name , title = title, legned.show=T, legend.position = c(0.7,0.5), textNA = '', colorNA = 'blue1',
                breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
                labels = c('Very low', 'Low', 'Moderate', 'High', 'Very high')) + 
    tm_layout(panel.labels = label_title)
  return(p)
}



names(ri09)[ncol(ri09)-1] <- 'RI09'
ri99 <- cbind(ri99, ri09$RI09)
ri99[which(ri99$RI09=='NaN'), 'RI09'] <- 0



p <- tm_shape(ri99) + 
  tm_polygons(col = c('RI', 'ri09.RI09'), title = c('RI99', 'RI09'),
              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), palette = c('blue1', 'green1', 'yellow1', 'orange1', 'red1'),
              labels = c('Very low', 'Low', 'Moderate', 'High', 'Very high'))

p
tmap_save(p,'./test4.pdf', dpi=600)


caa <- raster('./GIS_module/Output/LandCover/caaRP09.tif')
caa <- reclassify(caa, rcl=c(0,1, 0))

year <- 2009
nc <- raster(paste0('./GIS_module/Output/Modelling/Nc/Default/Nc_WB_', year_prefix(year),'.tif'))
nc09 <- reclassify(nc, rcl=c(0, 10, 0.2, 10, 25, 0.4, 25, 50, 0.6, 50, 75, 0.8, 75, +Inf, 1))
nc09[is.na(nc09[])] <- 0
nc09 <- nc09 + caa

st <- stack(nc99, nc09)
st <- mask_aquifer_reclass_index(st)

gw <- spTransform(gw, proj4string(st))


p <-   tm_shape(st) + 
  tm_raster(title = c('Risk99', 'Risk09'),
              breaks = c(0,0.0001, 0.2, 0.4, 0.6, 0.8, 1), palette = c('gray','blue1', 'green1', 'yellow1', 'orange1', 'red1'),
              labels = c('Water Balance 0', 'Very low', 'Low', 'Moderate', 'High', 'Very high')) + 
  tm_shape(gw) + tm_borders(col='black')  

p


haz <- stack('./Leaching_risk_index/Output/GIS_index/NC_reclass99.tif', 
             './Leaching_risk_index/Output/GIS_index/NC_reclass09.tif')
vul <- stack('./Leaching_risk_index/Output/GIS_index/Vulnerability_99.tif',
             './Leaching_risk_index/Output/GIS_index/Vulnerability_09.tif')
ri <- stack('./Leaching_risk_index/Output/GIS_index/Leaching_index99.tif',
            './Leaching_risk_index/Output/GIS_index/Leaching_index09.tif')

st <- stack(haz, vul, ri)

