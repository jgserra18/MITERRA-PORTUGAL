source('./GIS_module/Function/General_GIS_functions.R')
source('./Main_functions.R')

library(raster)
library(gstat)
library(rgdal)
library(foreach)
library(doParallel)


get_atmospheric_data <- function(Ndepo_source, year) {
  # unit: mg N/m2/yr converted to kg N/ha/yr

  atmN_file <- get_activity_data(subfolder = 'Atmospheric_deposition', 
                                 subfolderX2 = Ndepo_source, 
                                 file_pattern = 'N_deposition')
  names(atmN_file) <- gsub(pattern = 'X', replacement = '', x = names(atmN_file))
  
  atmN_file <- atmN_file[, c('LON', 'LAT', as.character(year))]
  atmN_file[, as.character(year)] <- atmN_file[, as.character(year)] / 100
  return(atmN_file)
}


compute_total_atmN <- function(year) {
  # sums the reduced and oxidized N compounts
  # unit: kg N/ha/yr
  
  atmN_reduced <- get_atmospheric_data(Ndepo_source = 'Reduced', year = year)
  atmN_oxidized <- get_atmospheric_data(Ndepo_source = 'Oxidized', year = year)
  
  atmN_total <- atmN_reduced
  atmN_total[, as.character(year)] <- atmN_reduced[, as.character(year)]  + atmN_oxidized[, as.character(year)] 
  return(atmN_total)
  rm(list=c('atmN_reduced', 'atmN_oxidized'))
}


atmN_interpolation <- function(year) {
  # interpolates (IDW) the sum of reduced and oxidized N deposition
  # unit: kg N ha-1 yr-1
  
  atmN_total <- compute_total_atmN(year)
  
  # create spatial points based on coordinates
  SP <- SpatialPoints(atmN_total[, 1:2], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
  SPdf <- SpatialPointsDataFrame(SP, atmN_total)
  names(SPdf)[3] <- 'atmN'
  
  # reproject SPdf based on LAEA projection + extent and creation of empty raster with the correct specifications
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  SPdf <- spTransform(SPdf, proj4string(caa))
  SPdf@bbox <- as.matrix(extent(caa))
  r <- raster(extent(caa), crs=crs(caa), res=100)
  
  # spatial interpolation of total atmN with default parameters
  gs <- gstat(formula=atmN~1, locations=SPdf, nmax=5, set=list(idp=2))
  atmN <- interpolate(r, gs)
  
  # mask the interpolation
  muni <- get_muni_shp()
  atmN_mask <- mask(atmN, muni)
  
  write_annual_data(module_name = 'AtmN_module', 
                    subfolder_name = 'Total_atmN',
                    file = atmN_mask, 
                    year = year, 
                    filename = 'idw_atmN')

  rm(list=c('caa', 'SPdf', 'r', 'gs', 'atmN', 'muni', 'atmN_mask'))
}


compute_atmN_municipality <- function(year) {
  # computes the average atmospheric N deposition at the municipality level
  # unit : kg N ha-1 yr-1
  
  atmN <- get_module_subfolder_output(module = 'AtmN', 
                                      submodule = 'Total_atmN', 
                                      submoduleX2 = year,
                                      file_pattern = 'idw_atmN')
  muni <- get_muni_shp()
  atmN_df <- create_main_csv()
  # start parallel
  cl <- makeCluster(3)
  registerDoParallel(cl)
  
  atmN_muni <- foreach(i=1:278, .packages = c('rgdal', 'rgeos', 'raster'), .combine=rbind) %dopar% {
    sb_muni <- muni[i, ]
    crop_atmN <- crop(atmN, extent(sb_muni))
    mask_atmN <- mask(crop_atmN, sb_muni)
    # calculate avg atmN for each municipality (kg N ha-1 yr-1)
    avg_atmN_muni <- round(cellStats(mask_atmN, 'mean'), 2)
    data.frame(atmN_df[i,], avg_atmN_muni)
  }
  stopCluster(cl)
  write_annual_data(module_name = 'AtmN_module', 
                    subfolder_name = 'AtmN_municipality',
                    file = atmN_muni, 
                    year = year, 
                    filename = 'idw_atmN')
  rm(list=c('atmN', 'muni', 'atmN_df', 'atmN_muni'))
}
compute_atmN_municipality(1999)



  

