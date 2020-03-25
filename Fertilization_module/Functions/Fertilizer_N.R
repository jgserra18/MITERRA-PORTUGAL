source('./Fertilization_module/Functions/Fertilizer_distribution_mechanism.R')


compute_total_unfit_fertiliser_N <- function(year) {
  # calc_df regards the dataframe where the totals of the maincrops are going to be summed, giving total fert N
  # maincrop_df regards the dataframe where the totals of crops are going to be summmed, giving total fert N per main crop class
  # computes the totals for
  # 1 - main crops
  # 2 - total unfit fertiliser N
  # unit: kg N yr-1
  
  calc_df <- create_main_csv()
  
  main_crops <- get_all_maincrop_names()
  for (i in main_crops) {
    maincrop_df <- create_main_csv()
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      print(j)
      # aggregate crops into the main crop dataset
      crop_fertN <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                submodule = 'Fertiliser_N_unadjusted', 
                                                submoduleX2 = i,
                                                file_pattern = j, 
                                                submoduleX3 = year)
      maincrop_df[, j] <- crop_fertN[, as.character(year)]
    }
    # sum the totals of crops for each maincrop
    ifelse(ncol(maincrop_df)>4,
           maincrop_df[, 'total'] <- rowSums(maincrop_df[, seq(4, ncol(maincrop_df))]),
           maincrop_df[, 'total'] <- maincrop_df[, 4])
    write_annual_data(module_name = 'Fertilization_module',
                      subfolder_name = 'Fertiliser_N_unadjusted',
                      file = maincrop_df,
                      year = year,
                      filename = paste0(i, '_total'),
                      subfolder_nameX2 = 'Municipality_totals')

    calc_df[, i] <- maincrop_df[, 'total']
  }
  # sum the totals of maincrops giving total fert N in municipality
  calc_df[, 'total_municipality'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  write_annual_data(module_name = 'Fertilization_module',
                    subfolder_name = 'Fertiliser_N_unadjusted',
                    file = calc_df,
                    year = year,
                    filename = 'Municipality_total',
                    subfolder_nameX2 = 'Municipality_totals')
}

get_module_subfolder_output()
compute_FAN <- function(year) {
  # computes the total adjustment factor FAN
  # to adjust the unfit fertiliser N according to the historical fertiliser N data
  # unit: dimensionless
  
  # get mainland unfit fertiliser N (in ktonnes N yr-1)
  unfit_fertN <- get_module_subfolder_output(module = 'Fertilization_module', 
                                            submodule = 'Fertiliser_N_unadjusted', 
                                            submoduleX2 = 'Municipality_totals',
                                            file_pattern = 'Municipality_total', 
                                            submoduleX3 = year)
  total_unfit_fertN <- sum(unfit_fertN[, ncol(unfit_fertN)])/1000000
  hist_ferN <- get_historical_fertilizerN(year, 'fert_ktN')
  
  FAN <- hist_ferN/total_unfit_fertN
  return(FAN)
  rm(list=c('unfit_fertN', 'total_unfit_fertN', 'hist_fertN'))
}


compute_adjusted_crop_fertilization_rates <- function(year, main_crop, crop, fertN_df) {
  
  
  crop_area <- get_crop_areas(year, main_crop = main_crop, crop = crop)
  fertN_df[, as.character(year)] <- round( fertN_df[, as.character(year)] / crop_area[, 4], 2)
  fertN_df <- data_cleaning(fertN_df)
  write_annual_data(module_name = 'Fertilization_module',
                    subfolder_name = 'Fertilization_FAN_rates',
                    file = fertN_df,
                    year = year,
                    filename = crop,
                    subfolder_nameX2 = main_crop)
  rm(list=c('fertN_df', 'crop_area'))
}


compute_adjusted_crop_fertiliser_N <- function(year) {
  # adjusts the unfit crop fertiliser N based on FAN
  # and calculates corrected crop fertiliser rates
  # unit: kg N yr-1
  
  calc_df <- create_main_csv()
  FAN <- compute_FAN(year)
  
  main_crops <- get_all_maincrop_names()
  for (i in main_crops) {
    maincrop_df <- create_main_csv()
    crops <- get_fertilization_crops(main_crop = i)
    for (j in crops) {
      # aggregate crops into the main crop dataset ----------------------------------------
      crop_fertN <- get_module_subfolder_output(module = 'Fertilization_module', 
                                                submodule = 'Fertiliser_N_unadjusted', 
                                                submoduleX2 = i,
                                                file_pattern = j, 
                                                submoduleX3 = year)
      crop_fertN[, as.character(year)] <- round( crop_fertN[, as.character(year)] * FAN , 2)
      write_annual_data(module_name = 'Fertilization_module',
                        subfolder_name = 'Fertiliser_FAN',
                        file = crop_fertN,
                        year = year,
                        filename = j,
                        subfolder_nameX2 = i)
      
      maincrop_df[, j] <- crop_fertN[, as.character(year)]
      
      # compute crop fertilization rate ----------------------------------------------------
      compute_adjusted_crop_fertilization_rates(year = year, main_crop = i, crop = j, fertN_df = crop_fertN)
    }
    # sum the totals of crops for each maincrop
    ifelse(ncol(maincrop_df)>4,
           maincrop_df[, 'total'] <- rowSums(maincrop_df[, seq(4, ncol(maincrop_df))]),
           maincrop_df[, 'total'] <- maincrop_df[, 4])
    write_annual_data(module_name = 'Fertilization_module',
                      subfolder_name = 'Fertiliser_FAN',
                      file = maincrop_df,
                      year = year,
                      filename = paste0(i, '_total'),
                      subfolder_nameX2 = 'Municipality_totals')
    
    calc_df[, i] <- maincrop_df[, 'total']
  }
  # sum the totals of maincrops giving total fert N in municipality
  calc_df[, 'total_municipality'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  write_annual_data(module_name = 'Fertilization_module',
                    subfolder_name = 'Fertiliser_FAN',
                    file = calc_df,
                    year = year,
                    filename = 'Municipality_total',
                    subfolder_nameX2 = 'Municipality_totals')
}

