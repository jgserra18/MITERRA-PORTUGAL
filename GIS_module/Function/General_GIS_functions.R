library(raster)
library(rgdal)

#load GIS functions to export shapefile
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Main_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')


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

general_rasterize <- function(shp,  fiel_rast)
{
  r <- raster(ext=extent(2635900, 2977200, 1729700, 2298200), 
              crs=CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'),
              res=100)
  
  rast <- rasterize(shp, r, field=fiel_rast)
  return(rast)
}

#rasterize_data_muni(nleaching09, 'Muni_ID', 'leaching_nha')
rasterize_data_muni <- function(df_to_rasterize, merged_col, field_rasterize)
{
  muni <- get_muni_shp()
  muni <- merge(muni, df_to_rasterize,merged_col)
  
  #create empty raster
  r <- raster(extent(muni), res=100, crs=crs(muni))#0.01)

  r <- rasterize(muni, r, field=field_rasterize, background=NA)
  
  return(r) 
}

rasterize_caa_muni <- function(caa_df, df_to_rasterize, merged_col, field_rasterize)
{
  caa <- merge(caa_df, df_to_rasterize, merged_col)
  
  r <- raster(extent(caa_df), res=100)#0.01)
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
  
  rcaa <- raster(extent(muni), res=100)#0.01)
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
## ----------------------------------------------------------------------------------------

#tier_ssnb can be either tier2_irrig or tier2_ssnb
get_ssnb_data <- function(year, tier_ssnb)
{
  load_ssnb_muni(year, TRUE, tier_ssnb)
}

#this is recycled from Precipitation_idw.R
get_precipitation <- function(year, pattern_file)
{
  main_folder <- select_maindata_pattern('Climatic')
  prec <- list.files(main_folder, pattern='Precipitation')
  prec_folder <- file.path(main_folder, prec)
  
  pattern <- paste0(pattern_file, year_prefix(year))
  prec_file <- list.files(prec_folder, pattern = pattern)
  file_path <- file.path(prec_folder, prec_file)
  prec <- raster(file_path)
}


## WRITE DATA -----------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------

raster_modelling_subfolders <- function(subfolder) {
  #write output of raster modelling
  #d <- raster_modelling_subfolders('SSNB')
  
  folder_modelling <- select_GIS_output_submodule('Modelling')
  subfolder_pattern <- list.files(folder_modelling, pattern=subfolder)
  subfolder_path <- file.path(folder_modelling, subfolder_pattern)
  
  return(subfolder_path)
}

get_modelling_files <- function(folder, filename, irrig_mode) {
  #gets raster modelling files from each folder
  #d <- get_modelling_files('Total_leaching', 'tot_leaching99')
  
  folder <- raster_modelling_subfolders(folder)
  
  if (missing(irrig_mode)==TRUE) {
    file <- list.files(folder, pattern = filename)
    file_path <- file.path(folder, file)
    r <- raster(file_path)
  }
  else {
    select_subfolder <- list.files(folder, pattern=irrig_mode)
    path <- file.path(folder, select_subfolder)
    
    files <- list.files(path, pattern = filename)
    file_path <- file.path(path, files)
    
    read_file <- raster(file_path)
  }
}

get_modellingDf_file <-function(file_pattern, folder_name, irrig_mode) {
  #function to get CSV files from subfolders of Modelling
  #d <- get_modellingDf_file('df_adj_fac', 'Adjustment_factor')
  
  subfolder_path <- raster_modelling_subfolders(folder_name)
  
  if (missing(irrig_mode)==TRUE) {
    files <- list.files(subfolder_path, pattern = file_pattern)
    file_path <- file.path(subfolder_path, files)
    
    read_file <- read.csv(file_path)
  }
  else  {
    select_subfolder <- list.files(subfolder_path, pattern=irrig_mode)
    path <- file.path(subfolder_path, select_subfolder)
    
    files <- list.files(path, pattern = file_pattern)
    file_path <- file.path(path, files)
    
    read_file <- read.csv(file_path)
  }
}

write_raster <- function(rasterfile, filename, subfolder, modelling) {
  #writes raster to a specified subfolder within GIS outputs
  
  ifelse(missing(modelling)==TRUE,
         subfolder <- select_GIS_output_submodule(subfolder),
         subfolder <- raster_modelling_subfolders(subfolder))
  
  filename <- paste0(filename, '.tif')
  file_path <- file.path(subfolder, filename)
  
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(rasterfile, file_path, options=tifoptions)
}

write_raster_modelling <- function(rasterfile, filename, subfolder) {
  write_raster(rasterfile, filename, subfolder, 'Yes')
}


################################ WRITE RASTERS ################################

#NOTE THESE ARE NOT PROJECTED TO LAEA SO THESE ARE DISREGARDED HERE
#THIS SUBMODULE MAY BE HAVE TO BE PROPERLY IMPLEMENTED IN THE FUTURE
#r <- rasterize_caa(2009)
#write_raster(r, 'CAA09', 'LandCover')

## COMPUTATION FUNCTIONS ------------------------------------------------------------------------
raster_ssnb_caa <- function(year, tier_ssnb, write) {
  #raster_ssnb_caa(1999, 'tier2_ssnb', TRUE)
  
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

get_ssnb_raster <- function(filename) {
  #loads rasterized SSNB
  
  get_modelling_files('SSNB', filename)
}

get_leaching_raster <- function(filename)
{
  get_modelling_files('Total_leaching', filename)
}

#note: total N-leaching (kg N/ha)
compute_leaching_caa <- function(ssnb_filename, year, write)
{
  ssnb <- get_ssnb_raster(ssnb_filename)
  lf09 <- get_GIS_file(paste0('Lf', year_prefix(year)), 'MITERRA')
  
  leaching <- ssnb*lf09
  
  write_raster_modelling(leaching, paste0('tot_leaching', year_prefix(year)), 'Total_leaching')
}

write_csv_modelling <- function(folder_path, subfolder_pattern, df_write, filename)
{
  filename <- paste0(filename, '.csv')
  
  modelling_subfolder <- raster_modelling_subfolders(folder_path)
  
  if (missing(subfolder_pattern)==FALSE)
  {
    subfolder <- list.files(modelling_subfolder, pattern=subfolder_pattern)
    subfolder_path <- file.path(modelling_subfolder, subfolder)
    path <- file.path(subfolder_path, filename)
  }
  else 
  {
    path <- file.path(modelling_subfolder, filename)
  }
  
  write.csv(df_write, path)
}
