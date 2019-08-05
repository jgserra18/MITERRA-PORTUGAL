source('./GIS_module/Function/General_GIS_functions.R')


library(raster)
library(rgdal)

get_clc <- function(year)
{
  lulcc_folder <- select_maindata_pattern('LULCC')
  file <- list.files(lulcc_folder, pattern = as.character(year))
  
  clc_file <- file.path(lulcc_folder, file)
  clc_file <- raster(clc_file)
}


get_lulcc_file <- function(pattern)
{
  lulcc_folder <- select_maindata_pattern('LULCC')
  file <- list.files(lulcc_folder, pattern = pattern)
  clc_file <- file.path(lulcc_folder, file)
  clc_file <- read.csv(clc_file)
}

get_reclass_LC <- function()
{
  lc_rc <- get_lulcc_file('reclass')
}


exclude_nonUAA <- function(year)
{
  clc <- get_clc(year)
  df <- get_reclass_LC()
  rcl_clc <- reclassify(clc, rcl=df[, -3])
  return(rcl_clc)
}

reclass_hetero <- function(year) {
  clc <- get_clc(year)
  df <- get_reclass_LC()
  rcl_clc <- reclassify(clc, rcl=df[, -3])
  return(rcl_clc)
}


write_raster_lulcc <- function(rasterfile, name, year)
{
  subfolder <- select_GIS_output_submodule('LandUse')
  select_year <- file.path(subfolder, year)
  
  filename <- paste0(name, '.tif')
  file_path <- file.path(select_year, filename)

  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(rasterfile, file_path, options=tifoptions, overwrite=T)
}

#main assumption: heterogeneous agricultural mosaic is assumed to be all rainfed
export_LU <- function(year)
{
  lu <- exclude_nonUAA(year)
  lu_allocation <- get_lulcc_file('LULCC')
  #focus only on relevant LU for irrigation
  lu_allocation <- lu_allocation[which(grepl('Heterogeneous', lu_allocation$clc_name)==TRUE), ]
  
  ifelse(year==2000, yr <- 1999, yr <- 2009)
  
  for (i in 1:nrow(lu_allocation))
  {
    spec_lu <- lu[[1]]==lu_allocation[i, 1]
    spec_lu[spec_lu!=1] <- NA
    lu_name <- lu_allocation[i,3]
    write_raster_lulcc(spec_lu, lu_name, yr)
  }
}

## ------------------------------------------------------------------------------
## Write UAA Corine Land Cover to LandCover in GIS_module
reclassify_UAA_clc <- function(year)
{
  clc <- exclude_nonUAA(year)
  clc <- reclassify(clc, rcl=c(1, 250, 1))
}

write_UAA_LC <- function(year)
{
  clc_rc <- reclassify_UAA_clc(year)
  ifelse(year==2012, name <- 'caaRP09', name <- 'caaRP00')
  
  write_raster(rasterfile = clc_rc, filename = name , subfolder = 'LandCover')
}


