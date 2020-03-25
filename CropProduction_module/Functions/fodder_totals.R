source('./CropProduction_module/Functions/fodder_NonDairy_functions.R')
source('./Main_functions.R')


compute_total_roughageN <- function(year) {
  # calculates the total roughage N for the specific animal classes below
  # this is the GNB input
  # unit: kg N yr-1
  
  animal_classes <- c('Dairy', 'Bovine', 'Equides', 'Goats', 'Sheep')
  calc_df <- create_main_csv()
  
  for (i in animal_classes) {
    # aggregate the total roughage N of the specific animal classes into one dataset 
    animal_roughage_N <- get_module_subfolder_output(module = 'CropProduction_module', 
                                                     submodule = 'Roughage_N', 
                                                     submoduleX2 = year,
                                                     file_pattern = i)
    calc_df[, i] <- animal_roughage_N[, 4]
  }
  # sum the total roughage N
  calc_df[, 'total'] <- round( rowSums(calc_df[, seq(4, ncol(calc_df))]) , 2)
  write_annual_data(module_name =  'CropProduction_module', 
                    subfolder_name = 'Roughage_N', 
                    file = calc_df, 
                    filename = 'Total_roughageN', 
                    year = year)
  rm(list=c('animal_classes', 'calc_df', 'animal_roughage_N'))
}




