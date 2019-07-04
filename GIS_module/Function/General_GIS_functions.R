library(raster)
library(rgdal)

#load GIS functions to export shapefile
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Main_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')

#get_muni_shp <- function()
#{
 # muni <- load_shp('Muni')
 # #standardize Muni_ID for further merges
 # muni$Muni_ID <- as.character(muni$Muni_ID)
 # muni$Muni_ID <- as.integer(muni$Muni_ID)
  #standardize muni_shp to intersect with CAA
  #muni <- spTransform(muni, crs('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
#muni <- spTransform(muni, crs('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))
  
 # return(muni)
#}

get_muni_shp <- function()
{
  path <- select_GIS_output_submodule('Muni')
  file_path <- file.path(path, list.files(path, pattern='shp'))
  muni <- readOGR(file_path)
  colnames(muni@data)[1] <- 'Muni_ID'
  muni$Muni_ID <- as.character(muni$Muni_ID)
  muni$Muni_ID <- as.integer(muni$Muni_ID)
  
  ext <- c(2635900, 2977200, 1729700, 2298200)
  muni <- crop(muni, ext)
  
  return(muni)
}

#rasterize_data_muni(nleaching09, 'Muni_ID', 'leaching_nha')
rasterize_data_muni <- function(df_to_rasterize, merged_col, field_rasterize)
{
  muni <- get_muni_shp()
  muni <- merge(muni, df_to_rasterize,merged_col)
  
  #create empty raster
  #extent <- c(-9.491946, -6.19784, 37.00798, 42.1539)
  #extent <- extent(muni)
  #sr <- crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  r <- raster(extent(muni), res=100, crs=crs(muni))#0.01)
  #proj4string(r) <- sr
  r <- rasterize(muni, r, field=field_rasterize, background=NA)
  
  return(r) 
}

rasterize_caa_muni <- function(caa_df, df_to_rasterize, merged_col, field_rasterize)
{
  caa <- merge(caa_df, df_to_rasterize, merged_col)
  
 # sr <- crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  r <- raster(extent(caa_df), res=100)#0.01)
  #proj4string(r) <- sr
  r <- rasterize(caa, r, field=field_rasterize, background=NA)
  
  return(r)
}

rasterize_caa <- function(year)
{
  if(year==1999)
  {
    caa <- load_shp('CAA99_test')
  }
  else if (year==2009)
  {
    caa <- load_shp('CAA09')
  }
  
  #sr <- crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  rcaa <- raster(extent(muni), res=100)#0.01)
  #proj4string(rcaa) <- sr
  rcaa <- rasterize(caa, rcaa, field=1, background=0)
  
  return(rcaa)
}

#merges a muni_df of a specific field (e.g. SSNB), merges with muni_shp and intersects with the CAA scale
spatial_disagg_muni_caa <- function(df_to_merge, id_col, col_plot, year)
{
  #load conditionals
  ifelse(year==2009, caa <- get_GIS_file('caaRP09', 'LandCover'), caa <- get_GIS_file('caaRP99', 'LandCover'))
  
  #merge with muni and rasterize
  muni <- rasterize_data_muni(df_to_merge, id_col, col_plot)
  muni_caa <- caa*muni
  
  return(muni_caa)
}

#returns the path of a specific GIS output subfolder
#select_GIS_output_submodule('LandCover')
select_GIS_output_submodule <- function(folder_name)
{
  output_path <- select_module_output('GIS')
  subfolders <- list.files(output_path, pattern = folder_name)
  subfolder_path <- file.path(output_path, subfolders)
  
  return(subfolder_path)
}

#gets a specified file within a certain folder
#d <- get_GIS_file('CAA99', 'LandCover')
get_GIS_file <- function(file_pattern, folder_name)
{
  subfolder_path <- select_GIS_output_submodule(folder_name)
  files <- list.files(subfolder_path, pattern = file_pattern)
  file_path <- file.path(subfolder_path, files)
  
  read_file <- raster(file_path)
}

## LOAD OTHER DATA ------------------------------------------------------------------------

#tier_ssnb can be either tier2_irrig or tier2_ssnb
get_ssnb_data <- function(year, tier_ssnb)
{
  load_ssnb_muni(year, TRUE, tier_ssnb)
}

## WRITE DATA ------------------------------------------------------------------------

#write output of raster modelling
#d <- raster_modelling_subfolders('SSNB')
raster_modelling_subfolders <- function(subfolder)
{
  folder_modelling <- select_GIS_output_submodule('Modelling')
  subfolder_pattern <- list.files(folder_modelling, pattern=subfolder)
  subfolder_path <- file.path(folder_modelling, subfolder_pattern)
  
  return(subfolder_path)
}

#writes raster to a specified subfolder within GIS outputs
write_raster <- function(rasterfile, filename, subfolder, modelling)
{
  ifelse(missing(modelling)==TRUE,
         subfolder <- select_GIS_output_submodule(subfolder),
         subfolder <- raster_modelling_subfolders(subfolder))
  
  filename <- paste0(filename, '.tif')
  file_path <- file.path(subfolder, filename)
  
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(rasterfile, file_path, options=tifoptions)
}

write_raster_modelling <- function(rasterfile, filename, subfolder)
{
  write_raster(rasterfile, filename, subfolder, 'Yes')
}

################################ WRITE RASTERS ################################

#NOTE THESE ARE NOT PROJECTED TO LAEA SO THESE ARE DISREGARDED HERE
#THIS SUBMODULE MAY BE HAVE TO BE PROPERLY IMPLEMENTED IN THE FUTURE
r <- rasterize_caa(2009)
write_raster(r, 'CAA09', 'LandCover')

## COMPUTATION FUNCTIONS ------------------------------------------------------------------------

raster_ssnb_caa <- function(year, tier_ssnb, write)
{
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  ssnb <- get_ssnb_data(year, tier_ssnb)
  print('Starting to rasterize the data----')
  r_ssnb <- rasterize_data_muni(ssnb,'Muni_ID', 'ssnb')
  print('Now computing the cropland SSNB----')
  ssnb_caa <- r_ssnb*caa
  
  ifelse(write==T,
    write_raster_modelling(ssnb_caa, paste0('ssnb', year_prefix(year)), 'SSNB'),
    return(ssnb_caa))
}

#loads rasterized SSNB
get_ssnb_raster <- function(filename)
{
  folder <- raster_modelling_subfolders('SSNB')
  file <- list.files(folder, pattern = filename)
  file_path <- file.path(folder, file)
  r <- raster(file_path)
}

#note: total N-leaching (kg N/ha)
compute_leaching_caa <- function(ssnb_filename, year, write)
{
  ssnb <- get_ssnb_raster(ssnb_filename)
  lf09 <- get_GIS_file(paste0('Lf', year_prefix(year)), 'MITERRA')
  
  leaching <- ssnb*lf09
  
  write_raster_modelling(leaching, paste0('tot_leaching', year_prefix(year)), 'Leaching')
}




