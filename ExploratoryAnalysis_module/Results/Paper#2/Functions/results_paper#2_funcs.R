source('./GIS_module/Function/GW_computation_functions.R')


compute_mainland_data <- function(year)
{
  #Load data
  adj <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  ssnb <- get_modelling_files('SSNB', paste0('ssnb', year_prefix(year)))
  lfrac <-get_GIS_file(paste0('Lf', year_prefix(year)), 'MITERRA')
  denit_frac <- 1-lfrac
  nl <- get_modelling_files('Total_leaching', paste0('tot_leaching', year_prefix(year)))
  
  ssnb_area <- ssnb*adj
  denit_area <- denit_frac*ssnb_area
  leach_area <- nl*adj
  
  df <- data.frame(source=c('ssnb', 'denit', 'leach'))  
  df$total_N <- c(cellStats(ssnb_area, 'sum'), cellStats(denit_area, 'sum'), cellStats(leach_area, 'sum'))
  df$frac <- 1
  
  df[, 3] <- sapply(df[1,2], function(x) df[, 2]/x*100)
  
  return(df)
}

compute_leaching_paper_data <- function(year)
{
  path <- './GIS_module/Output/Modelling/Leaching_fracs_source/'
  files <- list.files(path, pattern = year_prefix(year))
  
  df <- data.frame(source = NULL, y_1999 = NULL, y_2009 = NULL)
  ctr <- 1
  
  for (i in files)
  {
    i <- gsub('.tif', '', i)
    df[ctr, 1] <- i
    #load data -------------------------------------------------------------------------------
    caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
    adj  <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
    ssnb <-get_ssnb_raster(paste0('ssnb', year_prefix(year)))
    #compute avg fraction --------------------------------------------------------------------
    lf <- get_modelling_files('Leaching_fracs_source', i)
    lf[is.na(lf[[1]])] <- 0
    new_lf <- round(caa*lf*100, 2)
    mean <- cellStats(new_lf, 'mean')
    df[ctr, 2] <- mean
    #compute total leaching (in kg N) --------------------------------------------------------
    tl_kgN <- ssnb*lf*adj
    sum <- cellStats(tl_kgN, 'sum')
    df[ctr, 3] <- sum
    
    ctr <- ctr + 1
  }
  rm(list=c('tl_kgN', 'new_lf', 'lf', 'ssnb'))
  colnames(df) <- c('source', 'lf_frac[%]', 'N-leaching[kgN]')
  return(df)
}

#computes the N leached in 2009 where N-leaching was set to 0 in 1999
leaching_2009_1999_set_zero <- function()
{
  nl09 <- get_modelling_files('Total_leaching', paste0('tot_leaching', year_prefix(2009)))
  nl99 <- get_modelling_files('Total_leaching', paste0('tot_leaching', year_prefix(1999)))
  nl99_zero <- nl99[[1]]==0
  intersec <- nl99_zero*nl09
  adj  <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(2009)))
  
  intersec_area <- adj*intersec
  sum_masked_areas <- cellStats(intersec_area, 'sum')/1000
  rm(list = c('nl09', 'nl99', 'nl99_zero', 'intersect', 'adj'))
  
  return(sum_masked_areas)
}


compute_surface_leaching_alentejo <- function(surface_water, year, nuts2_shp)
{
  ssnb<-get_ssnb_raster(paste0('ssnb', year_prefix(year)))
  adj <- get_modelling_files('Adjustment_factor', paste0('rast_adj_factor', year_prefix(year)))
  lf <- get_modelling_files('Leaching_fracs_source', paste0(paste0('lf_', surface_water), year_prefix(year)))
  
  calc <- ssnb*adj*lf
  
  new_calc <- crop(calc, extent(nuts2_shp))
  new_calc <- mask(new_calc, nuts2_shp)
  
  sum <- cellStats(new_calc, 'sum')
}

#general function to compute the leaching N in alentejo of surface waters
#not optimized at all, you really call NUTs2 shapefile like 8 times???? dumb fuck
#you really know how to fuck the memory up
compute_all_surface_leaching_alentejo <- function()
{
  surface_water <- c('large', 'small')
  year <- c(1999, 2009)
  
  n2 <- load_shp('NUTS2')
  n2 <- spTransform(n2, CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'))
  n2_al <- subset(n2, NUTII_COD==18)
  
  df <- data.frame(source=c('large', 'small'), y_1999 <- 1, y_2009 <- 1)
  
  for (i in 1:length(surface_water))
  {
    for (j in 1:length(year))
    {
      calc <- compute_surface_leaching_alentejo(surface_water[i], year[j], n2_al)
      df[i, (j+1)] <- calc
    }
  }
  return(df)
}

d <- compute_surface_leaching_alentejo('large', 2009)
dd <- compute_all_surface_leaching_alentejo()

