source('./Main_functions.R')

library(rgdal)

#identify the shapefile files and the file_name
identify_shapefile <- function(path, file_name)
{
  shp_file <- list.files(path, pattern='*.shp')
  
  shp_file <- shp_file[which(grepl(file_name, shp_file)==TRUE)]
  return(shp_file)
}

#correct col names
colname_corr <- function(shp)
{
  if ((colnames(shp@data)[1] != 'CAA_ID') | (colnames(shp@data)[1] != 'Muni_ID'))
  {
    if (nrow(shp) < 150)
    {
      colnames(shp@data)[1] <- 'aquifer_ID'
    }
    else if (nrow(shp) < 300)
    {
      colnames(shp@data)[1] <- 'Muni_ID'
    }
    else 
    {
      colnames(shp@data)[1] <- 'CAA_ID'
    }
  }

  return(shp)
}

#loads municipality shape
load_shp <- function(file_name)
{
  gis_db <- select_maindata_pattern('GIS')
  gis_db <- check_folder(gis_db)
  shp_file <- identify_shapefile(gis_db, file_name)
  
  path <- paste0(gis_db, shp_file)
  shp <- readOGR(path)
  colname_corr(shp)
  
  return(shp)
}

load_surface_shp <- function(file_name)
{
  gis_db <- select_maindata_pattern('SurfaceWater')
  gis_db <- check_folder(gis_db)
  shp_file <- identify_shapefile(gis_db, file_name)
  
  path <- paste0(gis_db, shp_file)
  shp <- readOGR(path)
  colname_corr(shp)
  
  return(shp)
}

export_surface_csv <- function(file, name)
{
  path <- check_folder(select_maindata_pattern('SurfaceWater'))
  write.csv(file, paste0(path, name, '.csv'))
  
}


#correct Muni_ID cols of a df
corr_muni_id <- function(df)
{
  for (i in 1:nrow(df))
  {
    if (nchar(df[i, 1])==3)
    {
      df[i, 1] <- paste0(0, df[i, 1])
    }
  }
  return(df)
}

#merges all this crap
merge_file <- function(shp, df, id)
{
  shp <- load_shp(shp)
  df <- corr_muni_id(df)
  
  shp_df <- merge(shp, db, by=id)
  shp_df <- data_cleaning(shp_df)
  
  return(shp_df)
}

#computes zonal statistics 
zonal_statistic <- function(shp, raster, colname)
{
  z_stat <- raster::extract(raster, shp, fun=mean, na.rm=TRUE,  df=TRUE)
  df <- cbind(shp, z_stat[, 2])
  colnames(df@data)[ncol(df)] <- colname
  
  return(df)
}

