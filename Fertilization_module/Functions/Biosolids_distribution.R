source('./Fertilization_module/Functions/Crop_N_requirements.R')
source('./Fertilization_module/Functions/Manure_distribution.R')
source('./Main_functions.R')

## ----------------------- GETTERS ----------------------- ##
##---------------------------------------------------------##

get_sludge_data <- function(subfolder_name) {
  # gets the data of sludge amount for a given subfolder_name (i.e., Municipality, National)
  # unit: tonnes dm yr-1 (Municipality) or tonnes N yr-1 (National)
  
  main_folder <- select_maindata_pattern('Sewage_sludge')
  subfolder <- file.path(main_folder, subfolder_name)
  subfolder <- read.csv(
    list.files(path = subfolder, pattern = '*.csv', full.names=T))
  return(subfolder)
}


get_uncalibrated_sludgeN <- function(year) {
  # note: THIS IS NOT CALIBRATED AND ACCOUNTS FOR THE ENTIRETY OF REGISTERED BIOSOLIDS PRODUCED IN PORTUGAL 
  # FOR AGRICULTURAL STORAGE/APPLICATION
  # gets the sludge amounts (in tonnes dm yr-1) and converts these to N 
  # this is based on APA (2017) sludge N content of 0.0363 kg N kg dm-1
  # unit: tonnes N yr-1
  
  r_sludge <- get_sludge_data('Municipality') #tonnes dm yr-1
  calc_cols <- seq(4, ncol(r_sludge))
  r_sludge[, calc_cols] <- sapply(r_sludge[, calc_cols], function(x) round(x * 0.0363, 2))
  names(r_sludge) <- gsub(pattern = 'X', replacement = '', x = names(r_sludge))
  
  if (missing(year)==FALSE) {
    yr_id <- which(colnames(r_sludge)==year)
    r_sludge <- r_sludge[, c(1,2,3, yr_id)]
  }
  
  return(r_sludge)
}

## ----------------------- SLUDGE N AND AML SLUDGE DISTRIBUTION ----------------------- ##
##--------------------------------------------------------------------------------------##


correct_sludgeN_muni <- function(year) {
  # calibrates the total sludge N produced in Portugal at the municipality scale (APA official data)
  # this does not correspond to the sludge N applied in agriculture
  # Therefore, we "calibrate" these amounts based on the official data available in the PT GHG inventory (APA, 2019)
  # unit: tonnes N yr-1
  # timeperiod (current): 2006-2017
  
  uncalibrated_sludge <- get_uncalibrated_sludgeN(year)
  historical_mainland_sludge <-  get_sludge_data('National')
  
  year_cols <- seq(4, ncol(uncalibrated_sludge))
  
  for (i in year_cols) {
    
    # sum total N amounts of sludge produced (tonnes N)
    sum_uncalibrated_sludge <- sum(uncalibrated_sludge[, i])
    
    # get mainland official data concerning sludge N applied in agriculture ( tonnes N )
    year <- as.integer(colnames(uncalibrated_sludge)[i])
    year_id <- which(historical_mainland_sludge[, 1]==year)
    mainland_total <- historical_mainland_sludge[year_id, 2]
    
    # calculate adjustment factor and calibrate sludge N applied for agriculture
    adjustment_factor <- mainland_total/sum_uncalibrated_sludge
    uncalibrated_sludge[, i] <- round(uncalibrated_sludge[, i] * adjustment_factor, 2)
  }
  return(uncalibrated_sludge)
  rm(list=c('historical_mainland_sludge', 'year_cols', 'year', 
            'year_id', 'sum_uncalibrated_sludge', 'mainland_total', 
            'adjustment_factor'))
}

## NEEDS TO BE BETTER IMPLEMENTED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## NEEDS TO BE BETTER IMPLEMENTED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## NEEDS TO BE BETTER IMPLEMENTED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

sludgeN_AML_uaa <- function(year) {
  # subsets the UAA only of AML's municipalities
  # used to distribute sewage sludge in accordance to acreage needs
  # unit: ha UAA yr-1
  
  uaa <- load_uaa(year = year)
  spatial_df <- call_spatial_disaggregation()
  spatial_df <- cbind(spatial_df, uaa)
  colnames(spatial_df)[ncol(spatial_df)] <- as.character(year)
  
  # select only AMP
  spatial_df <- spatial_df[which(spatial_df[, 'nuts2_ID']==17),]
  
  return(spatial_df)
}


sludgeN_AMP_uaa <- function(year) {
  # subsets the UAA only of AMP's municipalities
  # used to distribute sewage sludge in accordance to acreage needs
  # unit: ha UAA yr-1
  
  uaa <- load_uaa(year = year)
  spatial_df <- call_spatial_disaggregation()
  spatial_df <- cbind(spatial_df, uaa)
  colnames(spatial_df)[ncol(spatial_df)] <- as.character(year)
  
  # select  AMP and AML
  AMP_id <- which(spatial_df[, 'nuts3_ID']=='11A')

  spatial_df <- spatial_df[AMP_id, ]
  return(spatial_df)
}

sludgeN_AMareas_uaa_frac <- function(AM_city, year) {
  
  ifelse(AM_city=='AML',
         uaa_AM <- sludgeN_AML_uaa(year),
         uaa_AM <- sludgeN_AMP_uaa(year))
  sum_uaa <- sum(uaa_AM[, ncol(uaa_AM)])
  uaa_AM$frac_UAA <- uaa_AM[, ncol(uaa_AM)] / sum_uaa
  
  return(uaa_AM[, -c(4,5,6,7,8)])
}


sludgeN_LisbonPorto_distribution <- function(year) {
  # distributes lisbon municipality sludge N to the remaining AML municipalities according to UAA relative importance
  # unit: tonnes N yr-1
  
  if (year!= 2009) {
    tryCatch('Currently, only the year 2009 is implemented.\n
             Please implement an adequate UAA module and GIS sludgeN distribution routine.')
  }
  else {
    sludgeN <- correct_sludgeN_muni(year)
    calc_df <- create_main_csv()
    
    AM <- c('AML', 'AMP')
    for (i in AM) {
      print(paste0('Distributing ', i))
      uaa_AM_df <- sludgeN_AMareas_uaa_frac(i, year)
      # Lisbon and Porto municipality corrected sludgeN
      ifelse(i=='AML',
             id <- which(sludgeN[, 3]=='Lisboa'),
             id <- which(sludgeN[, 3]=='Porto'))
      city_sludgeN <- sludgeN[id, as.character(year)]
      
      # calculate sludgeN distribution based on UAA fractions and rrecreate the main dataframe
      uaa_AM_df[, 'dist_sludgeN'] <- sapply(uaa_AM_df[, 'frac_UAA'], function(x) x * city_sludgeN)
      muni_df <- create_main_csv()
      muni_df <- muni_df %>%
        full_join(uaa_AM_df[, c('Muni_ID', 'dist_sludgeN')], by='Muni_ID') #%>%
        #arrange(ID)
      muni_df <- data_cleaning(muni_df)
      
      # merge AML's lisbon or AMP's Porto distributed sludge N and the calibrated sludge N amounts per municipality
      muni_df <- muni_df %>%
        full_join(sludgeN[, c('Muni_ID', as.character(year))], by='Muni_ID')
      # compute the corrected sludgeN and correct Lisbon (municipality)
      muni_df[, as.character(year)] <- round (muni_df[, as.character(year)] + muni_df[, 'dist_sludgeN'], 2)
      muni_df[id, as.character(year)] <- muni_df[id, 'dist_sludgeN']
   #   muni_df <- muni_df[, -4]
      calc_df[, i] <- muni_df[, 4]
    }
    # sum AML and AMP new sludge N and subset the dt to only the total
    calc_df[, 'dist_sludgeN'] <- calc_df[, 4] + calc_df[, 5]
    calc_df <- calc_df[, c(1,2,3,6)]
    # identify the ids to replace the sludgeN df
    dist_ids <- which(calc_df[, 4]>0)
    sludgeN[dist_ids, 4] <- calc_df[dist_ids, 4]
    
    write_annual_data(module_name = 'Fertilization_module',
                      subfolder_name = 'Biosolids_rates',
                      file = sludgeN,
                      year = year,
                      filename = 'Biosolids_total',
                      subfolder_nameX2 = 'Total_municipality')
  }
  rm(list=c('sludgeN', 'calc_df', 'dist_ids', 'muni_df', 'id', 'uaa_AM_df', 'city_sludgeN'))
}



## ----------------------- CROP SLUDGE N AREA DISTRIBUTION ----------------------- ##
##---------------------------------------------------------------------------------##
            
# distribution based on the relative proportion of a crop acreage within the total sum of the selected crops
# note: industry and horticulture are disregarded

sludgeN_crop_acreage_distribution <- function(year) {
  # calculates the percentual crop acreage comparatively to the total sum of crop acreage to be distributed
  # % of total crop acreage per municipality
  
  main_crops <- append(get_fodder_main.crops(), get_non.fodder_main.crops())
  calc_df <- create_main_csv()
  
  # aggregated into one data.fram crops to be distributed sludge N and sum its totals
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      calc_df[, j] <- crop_area[, 4]
    }
  }
  crop_cols <- seq(4, ncol(calc_df))
  sludgeN_crops_totals<- rowSums(calc_df[, crop_cols])
  # calculate crop acreage proportion comparatively to UAA
  calc_df[, crop_cols] <- sapply(calc_df[, crop_cols], function(x) x/sludgeN_crops_totals)
  return(calc_df)
  rm(list=c('main_crops', 'crops', 'crop_area', 'crop_cols', 'sludgeN_crops_totals'))
}


compute_sludgeN_crop_distribution <- function(year) {
  # calculates crop sludge N application by distributing sludge N at a municipality according 
  # to area fractions
  # unit: kg N ha crop-1 yr-1
  
  # crop distribution fraction
  crop_dist_frac <- sludgeN_crop_acreage_distribution(year)
  # total sludge N and convert it to kg N yr-1
  sludgeN_muni <- get_module_subfolder_output(module = 'Fertilization_module', 
                                              submodule = 'Biosolids_rates', 
                                              submoduleX2 = 'Total_municipality', 
                                              file_pattern = 'Biosolids_total', 
                                              submoduleX3 = year)
  sludgeN_muni[, as.character(year)] <-  sludgeN_muni[, as.character(year)] * 1000
  
  # allocate sludge N based on crop distribution fraction
  main_crops <- append(get_fodder_main.crops(), get_non.fodder_main.crops())
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      crop_df <- create_main_csv()
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      crop_df[, j] <- crop_dist_frac[, j] * sludgeN_muni[, as.character(year)] / crop_area[, 4]
      crop_df <- data_cleaning(crop_df)
      
      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Biosolids_rates',
                        file = crop_df,
                        year = year,
                        filename = j,
                        subfolder_nameX2 = i)
    }
  }
  rm(list=c('crop_dist_frac', 'sludgeN_muni', 'main_crops', 'crops', 'crop_df', 'crop_area'))
}


## ----------------------- CROP SLUDGE N FRV TO FINALISE FERTILISER MECHANISM ----------------------- ##
##----------------------------------------------------------------------------------------------------##

compute_biosolids_crop_FRV <- function(year) {
  # computes crop sludgeN_FRV that must be discounted in the crop N requirements mechanism
  # unit: kg N_FRV yr-1
  
  sludge_FRV <- get_FRV('sludge')
  
  main_crops <- append(get_fodder_main.crops(), get_non.fodder_main.crops())
  for (i in main_crops) {
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      crop_sludgeN_rate <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                  submodule = 'Biosolids_rates', 
                                                  submoduleX2 = i,
                                                  file_pattern = j, 
                                                  submoduleX3 = year)
      crop_area <- get_crop_areas(year = year, main_crop = i, crop = j)
      
      # compute crop sludge N FRV
      crop_sludgeN_rate[, 4] <- round(
        crop_sludgeN_rate[, 4] * crop_area[, 4] * sludge_FRV, 2)

      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Biosolids_crop_distribution',
                        file = crop_sludgeN_rate,
                        year = year,
                        filename = j,
                        subfolder_nameX2 = i)
    }
  }
}



