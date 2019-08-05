source('./GIS_module/Function/General_GIS_functions.R')

## ------------------------------------------------------------------------------------------------------------
## Functions to compute GIS leaching fractions to small and large surface waters as well as to deeper groundwater 

# computes leaching fractions for small and large surface waterbodies
compute_surface_leaching_fracs <- function(year)
{
  #Load data ---------------------------------------------------------
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  f_leach <- get_GIS_file(paste0('Lf', year_prefix(year)), 'MITERRA')
  f_leach <- f_leach*caa
  #water body conditions ---------------------------------------------
  waterbodies <- c('large', 'small')
  
  compute_fracs <- sapply(waterbodies, function(x)
      {
        body <- get_GIS_file(paste0(x, year_prefix(year)), 'Surface_waters')
        frac_body <- body*f_leach
        print('Writing raster ----')
        write_raster_modelling(frac_body, paste0('lf_', x, year_prefix(year)), 'Leaching_fracs_source')
      }
    )
}

compute_gw_leaching_frac <- function(year)
{
  # load total f_leach -------------------------------------------------
  caa <- get_GIS_file(paste0('caaRP', year_prefix(year)), 'LandCover')
  f_leach <- get_GIS_file(paste0('Lf', year_prefix(year)), 'MITERRA')
  f_leach <- f_leach*caa
  # load small and large Lf ---------------------------------------------
  lf_large <- get_modelling_files(folder = 'Leaching_fracs_source', paste0('lf_large', year_prefix(year)))
  lf_small <- get_modelling_files(folder = 'Leaching_fracs_source', paste0('lf_small', year_prefix(year)))
  # create mosaic with small and large lf
  r_mosaic <- mosaic(lf_large, lf_small, fun=sum)
  #compute lf_gw
  lf_gw <- f_leach-r_mosaic
  lf_gw[lf_gw<0] <- 0
  print('Writing raster ----')
  write_raster_modelling(lf_gw, paste0('lf_gw', year_prefix(year)), 'Leaching_fracs_source')
}

all_fraction_computation <- function()
{
  year <- c(1999, 2009)
  compute_fracs <- sapply(year, function(x)
    {
      print('Computing leaching fraction to waterbodies ...')
      compute_surface_leaching_fracs(x)
      print('Computing leaching fraction to groundwater ...')
      compute_gw_leaching_frac(x)
    }
  )
}

#all_fraction_computation()
