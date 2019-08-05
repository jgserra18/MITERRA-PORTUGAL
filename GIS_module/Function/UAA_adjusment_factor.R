source('./GIS_module/Function/compute_GIS_leaching_fractions.R')
source('./GIS_module/Function/General_GIS_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')

#adjustment factors
create_uaa_df <- function(year)
{
  uaa <- load_uaa(year)
  df <- create_main_csv()
  df <- cbind(df, uaa)
  return(df)
}

create_muni_shp_uaa <- function(year)
{
  uaa <- create_uaa_df(year)
  muni <- get_muni_shp()
  muni <- merge(muni, uaa[, c(1, 4)], 'Muni_ID')
  return(muni)
}

#computes the UAA in each municipality based on CLC of 2000 and 2012
compute_clc_uaa_muni <- function(year)
{
  #load data ------------------------------------
  df <- create_uaa_df(year)
  uaa_shp <- create_muni_shp_uaa(year)
  uaa_caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  muni_ids <- uaa_shp$Muni_ID
  
  ctr <- 0
  
  for (i in muni_ids)
  {
    sub <- subset(uaa_shp, Muni_ID==i)
    uaa_c <- mask(uaa_caa, sub)
    sum <- cellStats(uaa_c, 'sum')
    id <- which(df$Muni_ID==i)
    df[id, 'clc'] <- sum 
    ctr <- ctr +1
    print(ctr)
  }
    return(df)
}

compute_adjustment_factor <- function(year, write)
{
  clc_uaa_muni <- compute_clc_uaa_muni(year)
  clc_uaa_muni$adj_factor <- clc_uaa_muni$uaa/clc_uaa_muni$clc #compute adjusment factor
  
  path <- './GIS_module/Output/Modelling/Adjustment_factor/'
  name <- paste0('df_adj_factor', year_prefix(year), '.csv')
  path <- file.path(path, name)
  
  ifelse(write==TRUE,
         write.csv(clc_uaa_muni, path),
         return(clc_uaa_muni))
}

rasterize_adjs_factor <- function(year)
{
  #load data --------------------------------------------------------------------------------------
  df_adj <- get_modellingDf_file(paste0('df_adj_factor', year_prefix(year)), 'Adjustment_factor')
  muni <- get_muni_shp()
  
  muni <- merge(muni, df_adj[, c(2,7)], 'Muni_ID')
  r <- raster(ext=extent(2635900, 2977200, 1729700, 2298200), 
              crs=CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'),
              res=100)
  rast_muni <- rasterize(muni, r, field='adj_factor')
  write_raster_modelling(rast_muni, paste0('rast_adj_factor', year_prefix(year)), 'Adjustment_factor')
}

