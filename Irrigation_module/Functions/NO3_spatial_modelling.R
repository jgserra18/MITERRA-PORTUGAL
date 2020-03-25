source('./Irrigation_module/Functions/Global_irrigation_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')

library(sf)
library(rgdal)
library(raster)
library(fasterize)



export_nitrate_stack <- function(year, file_stack, filename, folder) {
  
  path <- './Irrigation_module/Output/NO3_modelling/'
  dir.create(path = path, showWarnings = F)
  
  folderpath <- file.path(path, folder)
  dir.create(path = folderpath, showWarnings = F)
  
  yearpath <- file.path(folderpath, year)
  dir.create(path = yearpath, showWarnings = F)
  
  filepath <- file.path(yearpath, paste0(filename, '.grd'))
  tifoptions=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6')
  writeRaster(file_stack, filepath, options=tifoptions, overwrite = TRUE)
}


subset_NO3_GW <- function(NO3_source, year) {
  # calls the NO3 monitoring stations data
  # the columns regarding the specified year are subset
  # and then average
  # NO3_source = 'NO3_GW' or !!!
  # unit: mg NO3 L-1 yr-1
  
  GW_db <- read_folder_files('NO3 stations', NO3_source)
  
  # subset GW db 
  yr_col <- which(grepl(as.character(year), names(GW_db))==TRUE)
  GW_yr <- GW_db[, c(1,2,3,4, yr_col)]
  names(GW_yr)[5:ncol(GW_yr)] <- paste0(year, '_', seq(1:length(yr_col)))
  
  # convert the dataset to an adequate format
  GW_yr[, seq(5, ncol(GW_yr))] <- sapply(GW_yr[, seq(5, ncol(GW_yr))], as.double)
  
  # compute the avg value of all stations for the specified year
  # subset again the dataset to account only the avg
  GW_yr$avg <- rowMeans(GW_yr[, seq(5, ncol(GW_yr))], na.rm = TRUE)
  GW_yr <- GW_yr[, c(1,2,3,4, ncol(GW_yr))]
  
  # remove NAs
  GW_yr <-  na.omit(GW_yr)
  return(GW_yr)
  rm(list='GW_db', 'yr_col')
}

create_SPdf_NO3 <- function(NO3_source, year) {
  # create adequately projected SPdf of mean NO3 levels for a given year
  # unit: mg NO3 l-1 yr-1
  
  NO3_stations <- subset_NO3_GW(NO3_source, year)
  SP <- SpatialPoints(NO3_stations[, 1:2],  
                      proj4string = CRS('+proj=tmerc +lat_0=39.66666666666666 +lon_0=-8.131906111111112 +k=1 +x_0=200000 +y_0=300000 +ellps=intl +towgs84=-288.885,-91.744,126.244,-1.691,0.410,-0.211,-4.598 '))
  SPdf <- SpatialPointsDataFrame(SP, NO3_stations)
  # reproject it to LAEA
  SPdf <- spTransform(SPdf, 
                   CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'))
  # fine tune the extent 
  new_bb <- c(2635900,  1729700, 2977200, 2298200)
  names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
  attr(new_bb, "class") = "bbox"
  SPdf <- st_as_sf(SPdf)
  attr(st_geometry(SPdf), "bbox") <- new_bb
  SPdf <- sf::as_Spatial(SPdf)
  SPdf <- st_as_sf(SPdf)
  
  return(SPdf)
  rm(list=c('NO3_stations', 'SP', 'new_bb'))
}


create_GW_Exp_Vars <- function(year, write) {
  # creates a dataset with all exploratory variables for groundwater NO3 prediciton
  
  uaa09 <- get_activity_data(subfolder = 'Irrigation_data', 
                             subfolderX2 = 'GW_dataset', 
                             file_pattern = 'rast_recharge_gw')
  gw <- get_muni_shp()
  r <- raster(ext=extent(uaa09), crs=crs(uaa09), res=1000)
  
  folder <- list.files(path = './Activity data/Irrigation_data/GW_dataset/', pattern = '.tif', full.names = T)
  r_list <- list()
  
  for (i in folder) {
    print(i)
    
    rr <- raster(i)
   # rr <- crop(rr, extent(r))
  #  rr <- resample(rr, r)
  #  rr <- mask(rr, gw)
    r_list <- append(r_list, rr)
    
  }
  
  rstack <- stack(r_list)
  if (write==TRUE) {
    export_nitrate_stack(year = year, file_stack = rstack, folder = 'Dataset_stack', filename = 'GW_DATASET_STACK')
  }
  else {
    return(rstack)
  }
  rm(list=c('uaa09', 'gw', 'r', 'folder'))
}

normalize_aggregate_exp_vars <- function(year) {
  # call exploratory datasets and normalize each exploratory parameter (0-1)
  # calls NO3 monitoring stations and extracts data from each exploratory parameter
  # converts to a dataframe and masks NAs
  
  rstack <- create_GW_Exp_Vars(year, F)
  gw_sp <- create_SPdf_NO3('NO3_GW', year)
  
  for (i in 1:nlayers(rstack)) {
    
    shp_name <- as.character(names(rstack)[i])
    gw_sp[, shp_name] <- raster::extract(rstack[[i]], gw_sp)
  }
  
  df <- na.omit(gw_sp)
  
  return(df)
  rm(list=c('rstack', 'gw_sp'))
}

create_partition <- function(df_no3) {
  # creates partition for further prediction
  # output: list where index 1 is the trainset and 2 is the testset
  
  set.seed(99)
  require(caTools)
  
  train <- sample.split(df_no3$x, SplitRatio = 0.8)
  train_set <- df_no3[train, ]
  test_set <- df_no3[-train, ]
  
  return(list(train_set, test_set))
}


predict_NO3_GW_rf <- function(year, write) {
  # spatially predicts nitrate concentration in groundwater using the randomforest algorithm 
  # unit: mg NO3 L-1

  library(randomForest)
  
  rstack <- create_GW_Exp_Vars(year, F)
  df_no3 <- normalize_aggregate_exp_vars(year)
  
  partition <- create_partition(df_no3)
  
  fm <- as.formula(paste('avg~', paste(names(rstack), collapse = '+')))
  print(fm)
  rf_model <- randomForest(formula=fm,
                           data=partition[[1]], ntree=1000, mtry=10, importance=T)
  
  predict_rf <- raster::predict(rstack, rf_model)
  predict_rf <- mask(predict_rf, rstack$clc_pt2012)
  predict_rf <- crop(predict_rf, extent(rstack$clc_pt2012))
  
  if (write==TRUE) {
    write_annual_data(module_name = 'Irrigation_module', 
                      subfolder_name = 'NO3_modelling', 
                      file = predict_rf, 
                      filename = 'NO3_GW_RF', 
                      year = year, 
                      subfolder_nameX2 = 'NO3_prediction')
  }
  else {
    return(predict_rf)
  }
  
  rm(list=c('rstack'))
}


test_RF_accuracy <- function(test_set, predict_rf) {
  
  test_set$test_No3 <- raster::extract(predict_rf, test_set)
  dff <- as.data.frame(test_set[, c('test_No3', 'avg')])
  
  library(ggplot2)
  p1 <- ggplot(dff, aes(test_No3, avg)) + geom_point() + 
    geom_smooth(method='lm') + 
    theme_test() 
  print(p1)
  summary(lm('avg~test_No3', dff))$r.squared
}


compute_avg_NO3_gw_muni <- function(year, write) {
  
  gw_no3 <- predict_NO3_GW_rf(year, F)
  
  muni <- get_muni_shp()
  df <- create_main_csv()
  
  for (i in 1:nrow(df)) {
    
    sb <- subset(muni, Muni_ID==df[i,1])
    sb_no3 <- crop(gw_no3, extent(sb))
    sb_no3 <- mask(sb_no3, sb)
    df[i, 'avg_no3'] <- cellStats(sb_no3, 'mean')
  }
  
  if (write==TRUE) {
    write_annual_data(module_name = 'Irrigation_module', 
                      subfolder_name = 'NO3_modelling', 
                      file = df, 
                      filename = 'AvgMuni_NO3_gw', 
                      year = year, 
                      subfolder_nameX2 = 'NO3_prediction')
  } else {
    return(predict_rf)
  }

  rm(list=c('gw_no3', 'muni', 'sb', 'sb_no3'))
}






