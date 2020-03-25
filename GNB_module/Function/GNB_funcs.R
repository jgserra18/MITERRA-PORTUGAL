source('./Main_functions.R')


## OUTPUT FOLDERS WILL BE
  # GNB_INPUT (INPUTS IN TERMS OF KG N UAA-1)
  # GNB_OUTPUT (OUTPUTS IN TERMS OF KG N UAA-1)
  # IRRIG_GNB_INPUTS
  # IRRIG_GNB_OUTPUTS



compile_GNB_input <- function(year, uaa) {
  # compiles all the GNB inputs into one common dataset
  # whether in kg N yr-1 or kg N (UAA)-1 yr-1
  
  
  # load N-inputs for a specific year (in kg N yr-1) -----------------------------
  atmN <- get_module_subfolder_output(module = 'AtmN_module', 
                                      submodule = 'AtmN_municipality', 
                                      submoduleX2 = year,
                                      file_pattern = 'idw_atmN')
  atmN[, 4] <- round ( atmN[, 4] * load_uaa(year), 2)
  
  BNF <- get_module_subfolder_output(module = 'BNF_module', 
                                     submodule = 'Total_BNF', 
                                     submoduleX2 = year, 
                                     file_pattern = 'Total')
  sludgeN <- get_module_subfolder_output(module = 'Fertilization_module', 
                                         submodule = 'Biosolids_rates', 
                                         submoduleX2 = 'Total_municipality', 
                                         submoduleX3 = year,
                                         file_pattern = 'Biosolids_total')
  fertN <- get_module_subfolder_output(module = 'Fertilization_module', 
                                       submodule = 'Fertiliser_FAN', 
                                       submoduleX2 = 'Municipality_totals', 
                                       submoduleX3 = year,
                                       file_pattern = 'Municipality_total')
  fertN <- fertN[, c(1,2,3, ncol(fertN))]
  gross_manureN <- get_module_subfolder_output(module = 'MMS_module', 
                                               submodule = 'Total_Nexcretion', 
                                               submoduleX2 = year, 
                                               file_pattern = 'Total_Nexcreted')
  gross_manureN <- gross_manureN[, c(1,2,3, ncol(gross_manureN))]
  # aggregate these into one dataframe ----------------------------------------------
  input_df <- create_main_csv()
  input_df <- plyr::join_all(list(atmN, BNF, sludgeN, fertN, gross_manureN), by=c('Muni_ID', 'ID', 'Muni'), type='left')
  names(input_df)[4:ncol(input_df)] <- c('AtmN', 'BNF', 'SludgeN', 'FertN', 'Gross_manureN')
  
  if(missing(uaa)==TRUE) {
    return(input_df)
  }
  else {
    input_cols <- seq(4, ncol(input_df))
    input_df[, input_cols] <- sapply(input_cols, function(x) round(input_df[, x]/load_uaa(year), 2))
    input_df <- data_cleaning(input_df)
    return(input_df)
  }
}


compile_GNB_output <- function(year, uaa) {
  # compiles all the GNB outputs into one common dataset
  # whether in kg N yr-1 or kg N (UAA)-1 yr-1
  
  crop_offtake <- get_module_subfolder_output(module = 'CropProduction_module', 
                                              submodule = 'Crop_Nremoval', 
                                              submoduleX2 = 'MainCrop_Totals', 
                                              submoduleX3 = year,
                                              file_pattern = 'Total_municipality')
  crop_offtake <- crop_offtake[, c(1,2,3, ncol(crop_offtake))]
  
  res_burnt <- get_module_subfolder_output(module = 'CropProduction_module', 
                                              submodule = 'Residues_burnt', 
                                              submoduleX2 = 'MainCrop_totals', 
                                              submoduleX3 = year,
                                              file_pattern = 'Total_municipality')
  res_burnt <- res_burnt[, c(1,2,3, ncol(res_burnt))]
  
  res_removed <- get_module_subfolder_output(module = 'CropProduction_module', 
                                             submodule = 'Residues_removed', 
                                             submoduleX2 = 'MainCrop_totals', 
                                             submoduleX3 = year,
                                             file_pattern = 'Total_municipality')
  res_removed <- res_removed[, c(1,2,3, ncol(res_removed))]
  
  fodder_offtake <- get_module_subfolder_output(module = 'CropProduction_module', 
                                                submodule = 'Roughage_N', 
                                                submoduleX2 = year, 
                                                file_pattern = 'Total_roughageN')
  fodder_offtake <- fodder_offtake[, c(1,2,3, ncol(fodder_offtake))]
  # aggregate these into one dataframe ----------------------------------------------
  output_df <- create_main_csv()
  output_df <- plyr::join_all(list(crop_offtake, res_burnt, res_removed, fodder_offtake), by=c('Muni_ID', 'ID', 'Muni'), type='left')
  names(output_df)[4:ncol(output_df)] <- c('Crop_offake', 'Res_burnt', 'Res_removed', 'Fodder_offtake')
  
  if(missing(uaa)==TRUE) {
    return(output_df)
  }
  else {
    out_cols <- seq(4, ncol(output_df))
    output_df[, out_cols] <- sapply(out_cols, function(x) round(output_df[, x]/load_uaa(year), 2))
    output_df <- data_cleaning(output_df)
    return(output_df)
  }
}


compile_GNB_activityData <- function(year, uaa) {
  # call the compiling functions for GNB inputs and outputs
  # calculate the sum of N inputs and outputs, respectively
  # and export those data to the GNB module output
  # unit: kg N yr-1 or kg N (UAA)-1 yr-1
  
  inputs <- compile_GNB_input(year, uaa)
  outputs <- compile_GNB_output(year, uaa)
  
  # modify filenames based on uaa option ----------------------------------
  if (missing(uaa)==TRUE) {
    inp_name <- 'N_Inputs_df'
    out_name <- 'N_Outputs_df'
  } else {
    inp_name <- 'NUAA_Inputs_df'
    out_name <- 'NUAA_Outputs_df'
  }
  
  inputs[, 'total_Ninputs'] <- rowSums(inputs[, seq(4, ncol(inputs))])
  write_annual_data(module_name = 'GNB_module', 
                    subfolder_name = 'GNB', 
                    year = year, 
                    subfolder_nameX2 = 'GNB_I-O', 
                    file = inputs, 
                    filename = inp_name)
  
  outputs[, 'total_Noutputs'] <- rowSums(outputs[, seq(4, ncol(outputs))])
  write_annual_data(module_name = 'GNB_module', 
                    subfolder_name = 'GNB', 
                    year = year, 
                    subfolder_nameX2 = 'GNB_I-O', 
                    file = outputs, 
                    filename = out_name)
  rm(list=c('inputs', 'outputs'))
}


compute_GNB <- function(year, uaa) {
  # calculates the GNB
  # GNB = N input - N output
  # unit: kg N yr-1 or kg N (UAA)-1 yr-1
  
  # modify filenames based on uaa option ----------------------------------
  if (missing(uaa)==TRUE) {
    inp_name <- 'N_Inputs_df'
    out_name <- 'N_Outputs_df'
    gnb_name <- 'N_GNB_df'
  } else {
    inp_name <- 'NUAA_Inputs_df'
    out_name <- 'NUAA_Outputs_df'
    gnb_name <- 'NUAA_GNB_df'
  }
  
  # call N-inputs and N-outputs and select only the respective totals -----
  inputs <- get_module_subfolder_output(module = 'GNB_module', 
                                        submodule = 'GNB', 
                                        submoduleX2 = 'GNB_I-O', 
                                        submoduleX3 = year,
                                        file_pattern = inp_name)
  inputs <- inputs[, c(1,2,3, ncol(inputs))]
  
  outputs <- get_module_subfolder_output(module = 'GNB_module', 
                                         submodule = 'GNB', 
                                         submoduleX2 = 'GNB_I-O', 
                                         submoduleX3 = year,
                                         file_pattern = out_name)
  outputs <- outputs[, c(1,2,3, ncol(outputs))]
  
  # compute the GNB --------------------------------------------------------
  gnb <- inputs
  colnames(gnb)[4] <- 'GNB'
  gnb[, 4] <- round ( inputs[, 4] - outputs[, 4], 2)
  write_annual_data(module_name = 'GNB_module', 
                    subfolder_name = 'GNB', 
                    year = year, 
                    subfolder_nameX2 = 'GNB', 
                    file = gnb, 
                    filename = gnb_name)
}

