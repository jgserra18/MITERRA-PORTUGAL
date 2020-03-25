source('./MMS_module/Functions/Nex_computation.R')
source('./MMS_module/Functions/Prepare_MMS_EFs.R')


## ----------------------- COMPUTE N-NH3 EMISSIONS FROM YARDS  ----------------------- ##
## ------------------------------------------------------------------------------------##


compute_NH3_yards <- function(year) {
  # computes NH3 emissions from yards for each animal class and respective subclasses
  # unit: kg N-NH3 yr-1
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    yard_TAN <- compute_TAN_flows_distribution(year = year, 
                                               dist_system = 'Yards', 
                                               animal_class = i)
    yard_NH3 <- yard_TAN
    animal_subclass <- colnames(yard_TAN)[-c(1,2,3)]  
    
    for (j in animal_subclass) {
      nh3_ef <- select_EFs_NH3_MMS(animal_class = i, animal_subclass = j, pathway = 'Yard', manure_type = 'solid')
      yard_NH3[, j] <- yard_TAN[, j] * nh3_ef$Yard
    }
    write_annual_data(subfolder = 'Yards', file = yard_NH3, filename = i, year = year, subfolder_nameX2 = 'NH3')
  }
}


## ----------------------- COMPUTE N-NH3 EMISSIONS FROM HOUSING (SOLID, SLURRY, TOTAL)  ----------------------- ##
## -------------------------------------------------------------------------------------------------------------##

compute_NH3_housing_mms <- function(year) {
  # computes N-NH3 emissions from animal housing for both solid and slurry systems
  # unit: kg N-NH3 yr-1
  
  animal_class <- get_animal_classes()
  manure_mms <- c('solid', 'slurry')

  for (i in animal_class) {
    
    for (a in manure_mms) {
      animal_class_mms <- paste0(i, '_', a) # e.g., Bovine_solid
      housing_TAN <- compute_TAN_flows_distribution(year = year, 
                                                    dist_system = 'Housing', 
                                                    animal_class = animal_class_mms)
      housing_NH3 <- housing_TAN
      animal_subclass <- colnames(housing_TAN)[-c(1,2,3)]  
      
      for (j in animal_subclass) {
        ef_nh3_mms <- select_EFs_NH3_MMS(animal_class = i, 
                                         animal_subclass = j, 
                                         pathway = 'Housing', 
                                         manure_type = a)
        housing_NH3[, j] <- housing_NH3[, j] * ef_nh3_mms$Housing
      }
     write_annual_data(subfolder = 'Housing', file = housing_NH3, filename = animal_class_mms, year = year, subfolder_nameX2 = 'NH3')
    }
  }
}

compute_NH3_housing_total <- function(year) {
  # computes TOTAL N-NH3 emissions from animal housing
  # TOTAL = SLURRY + SOLID
  # unit: kg N-NH3 yr-1
  
  animal_class <- get_animal_classes()

    for (i in animal_class) {
    # data dump 
    nh3_solid <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                              subfolderX2 = 'NH3', 
                                              year = year, 
                                              file_pattern = paste0(i, '_solid'))
    nh3_slurry <- get_MMS_subfolder_output_file(subfolder = 'Housing', 
                                                subfolderX2 = 'NH3', 
                                                year = year, 
                                                file_pattern = paste0(i, '_slurry'))
    
    animal_subclass_cols <- colnames(nh3_solid)[-c(1,2,3)]  
    nh3_housing_total <- nh3_slurry
    nh3_housing_total[, animal_subclass_cols] <- mapply("+", nh3_slurry[, animal_subclass_cols], nh3_solid[, animal_subclass_cols])
    
    write_annual_data(subfolder = 'Housing', file = nh3_housing_total, filename = paste0(i, '_total'), year = year, subfolder_nameX2 = 'NH3')
  }
}


## ----------------------- COMPUTE N/TAN leaving animal housing systems (SOLID ONLY) ----------------------- ##
## ----------------------------------------------------------------------------------------------------------##

## NOTE1 : THIS IS USED TO CALCULATE THE AMOUNT OF N/TAN ENTERING STORAGE IN SOLID SYSTEMS
## ALTHOUGH THE FUNCTIONS ARE IMPLEMENTED HERE, PLEASE SEE THEIR APPLITATION IN STORAGE_MMS.R
## NOTE2 : GO TO STORAGE_MMS.R

general_compute_animal_bedding_systems <- function(year, livestock_class, livestock_subclass, param) {
  # computes the total mass of straw OR N in bedding  for a specified livestock subclass
  # param is either bedding_mass or bedding_N
  # unit: kg straw yr-1
  
  animal_pop <- get_animal_pop(year, animal_class = livestock_class)
  # load animal distribution on solid systems
  animal_solid <- select_animal_MMS(year = 2009, animal_subclass = livestock_subclass, MMS = 'solid')
  # load straw amount per bedding
  animal_bedding <- get_animal_bedding(livestock_subclass = livestock_subclass, param = param)
  
  # compute total amount of straw per animal subclass
  animal_pop[, livestock_subclass] <- round(animal_pop[, livestock_subclass] * animal_solid * animal_bedding, 0)
  
  # subset only livestock subclass column for the calculations
  subset_livestock_subclass <- animal_pop[, livestock_subclass]
  
  return(subset_livestock_subclass)
}

compute_animal_bedding_amount <- function(year, livestock_class, livestock_subclass) {
  # computes the total mass of straw in bedding systms for a specified livestock subclass
  # unit: kg straw yr-1
  
  general_compute_animal_bedding_systems(year, livestock_class, livestock_subclass, param = 'bedding_mass')
}

compute_animal_bedding_N <- function(year, livestock_class, livestock_subclass) {
  # computes the total mass of straw in bedding systms for a specified livestock subclass
  # unit: kg N bedding yr-1
  
  general_compute_animal_bedding_systems(year, livestock_class, livestock_subclass, param = 'N_bedding')
}




