source('./GIS_module/Function/compute_GIS_leaching_fractions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')


#selects irrig or default subfolder
select_pathway_folder <- function(irrig_mode, main_subfolder)
{
  ifelse(missing(main_subfolder)==TRUE, sub <- 'Pathway_leaching', sub <- main_subfolder)
  path <- raster_modelling_subfolders(sub)
  ifelse(irrig_mode==T, pat <- 'Irrig', pat <- 'Default')
  subfolder <- file.path(path, pat)
  return(subfolder)
}

# write leachhing to different pathways according to the specified irrigation or default
write_pathway_leaching <- function(rasterfile, filename, irrig_mode, main_subfolder)
{
  subfolder <- select_pathway_folder(irrig_mode, main_subfolder)
  
  filename <- paste0(filename, '.tif')
  file_path <- file.path(subfolder, filename)
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  print('Writing raster -----')
  writeRaster(rasterfile, file_path, options=tifoptions, overwrite=TRUE)
}


#compute_nloading_gw(1999, FALSE)
compute_nloading_DeeperGroundwater <- function(year, irrig_mode)
{
  lf_gw <- get_modelling_files(folder = 'Leaching_fracs_source', paste0('lf_gw',year_prefix(year)))
  
 # ifelse(irrig_mode==T, )    IMPLEMENT WHEN ASSESSING IRRIGATION
  ssnb <- get_modelling_files(folder = 'SSNB', paste0('ssnb', year_prefix(year)))
  
  nload_gw <- lf_gw*ssnb
  nload_gw[nload_gw<0] <-0
  write_pathway_leaching(nload_gw, paste0('nload_gw', year_prefix(year)), irrig_mode)
}


#nl_gw <- get_nloading_gw(paste0('nload_gw', year_prefix(year)), FALSE)
#d <- get_nloading_gw('rf_prec09', 'Default', 'Drainage')
get_nloading_gw <- function(filename, irrig_mode, main_subfolder)
{
  subfolder <- select_pathway_folder(irrig_mode, main_subfolder)
  file <- list.files(subfolder, pattern=filename)
  file_path <- file.path(subfolder, file)
  
  r <- raster(file_path)
}

#apply GIS correction factor to correct the CLC UAA
#d <- correct_nloads_gw(1999, TRUE, 'Default')
#in kg N
correct_nloads_gw <- function(year, write, irrig_mode)
{
  nl_gw <- get_nloading_gw(paste0('nload_gw', year_prefix(year)), FALSE)
  adj_factor <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  
  nl_gw <- nl_gw*adj_factor
  if (write==TRUE){write_pathway_leaching(nl_gw, paste0('nload_correct_gw', year_prefix(year)), irrig_mode)}
  else {return(nl_gw)}
}

computation_gw_general_func <- function(compute_func, year, irrig_mode, parameter)
{
  compute <- compute_func(year, FALSE, irrig_mode)
  
  gw <- load_shp('gw')
  gw <- spTransform(gw, CRS(' +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
  
  gw_ids <- gw$GW_ID
  calc_df <- data.frame(GW_ID=gw_ids)
  calc_df[, parameter] <- ''
  
  print(paste0('Starting to compute the ', parameter, ' for each groundwater body...'))
  for (i in gw_ids)
  {
    sb_gw <- subset(gw, GW_ID==i)
    crop <- crop(compute, extent(sb_gw))
    mask <- mask(crop, sb_gw)
    sum <- cellStats(mask, 'sum')
    calc_df[i, 2] <- sum
    rm(list=c('crop', 'mask'))
  }
  
  print('Done.')
  calc_df[, parameter] <- as.numeric(calc_df[, parameter])
  calc_df[, parameter] <- round(calc_df[, parameter], 3)
  
  ifelse(parameter=='n-load', folder <- 'Pathway_leaching', folder <- 'Drainage')
  ifelse(parameter=='n-load', name <- 'nload_df_gw', name <- 'drainage_df_gw')
  ifelse(irrig_mode==TRUE, irrig <- 'Irrig', irrig <- 'Default')
  write_csv_modelling(folder_path = folder, subfolder_pattern = irrig, df_write = calc_df, filename = paste0(name, year_prefix(year)))
  
  return(calc_df)
}

#in kg N
compute_nload_entering_gw <- function(year, irrig_mode)
{
  computation_gw_general_func(correct_nloads_gw, year, irrig_mode, 'n-load')
}



#d <- compute_nload_entering_gw(2009, 'Default')

