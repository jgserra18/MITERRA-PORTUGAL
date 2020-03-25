source('./MMS_module/Functions/Storage_MMS.R')
source('./MMS_module/Functions/Grazing_MMS.R')
source('./MMS_module/Functions/Housing_MMS.R')
source('./MMS_module/Functions/Spreading_MMS.R')
source('./MMS_module/Functions/Nex_computation.R')


aggregate_totals <- function(year, subfolder, subfolderX2, manure_type, animal_class_list) {
  ## aggregates all gaseous losses from manure management systems
  ## OR
  ## aggregates the available N in gross manure spreading (i.e., wo/ spreading Nh3 emissions)
  ## or
  ## aggregates N excreted onto pastures
  ## Unit: kg N yr-1
  
  calc_df <- create_main_csv()
  
  # this allows the specification into ruminants and non-ruminants to distribute the manure
  ifelse(missing(animal_class_list)==TRUE,
    animal_class <- get_animal_classes(),
    animal_class <- animal_class_list)
  
  for (i in animal_class) {
  
    select_file <- get_MMS_subfolder_output_file(subfolder = subfolder, 
                                              subfolderX2 = subfolderX2, 
                                              year = year, 
                                              file_pattern = file_pattern(subfolderX2, i, manure_type))
    select_file <- data_cleaning(select_file)
    
    # calculate total N excreted onto pastures from an animal class
    calc_df[, i] <- round(rowSums(select_file[, seq(4,ncol(select_file))]), 0)
    colnames(calc_df)[ncol(calc_df)] <- i
  }
  calc_df$TOTAL <- round(rowSums(calc_df[, seq(4, ncol(calc_df))]), 0)
  return(calc_df)
}

