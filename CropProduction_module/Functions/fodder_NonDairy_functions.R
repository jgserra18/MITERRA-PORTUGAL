source('./MMS_module/Functions/Nex_computation.R')
source('./Main_functions.R')

## 1 - Calculate N retention
## 2 - Calculate N feeding intake
## N feeding intake comes from concentrate feeding for pigs, poultry and rabbits
## N feeding intake comes only from roughage for horses, sheeps and goats

## for non-dairy cattle
# 1 - calculate REM (IPCC 2006 Equation 10.14)
# 2 - Calculate NE_ma (dietary net energy concentration of diet) (IPCC 2006 Table 10.8)
# 3 - Calculate DMI (IPCC 2006 Equation 10.18a)


## ---------------- ANIMAL PARAMS GETTERS  ---------------- ##
## ---------------------------------------------------------##

get_livestock_params_folder <- function(param) {
  
  filepath <- list.files(select_livestock_subfolder('Animal_params'), pattern=param, full.names = TRUE)
  param_df <- read.csv(filepath)
  return(param_df)
}


get_nondairy_param <- function(param, year) {
  # gets nondairy params to calculate DMI
  # unit: avg mass (kg head-1 yr-1) or feed digestibility (%)
  
  non_dairy <- get_livestock_params_folder('Nondairy')
  
  yr_id <- which(non_dairy[, 1]==year)
  ifelse(param=='avg_mass',
         col <- 2,
         col <- 3)
  return(non_dairy[yr_id, col])
  rm(list=c('filepath', 'yr_id', 'col'))
}


get_animal_NUE <- function(animal_class) {
  
  nue_df <- get_livestock_params_folder('NUE')
  animal_id <- which(nue_df[, 1]==animal_class)
  return(nue_df[animal_id, 2])
}


## ---------------- ANIMAL N FEEDING INTAKE ---------------- ##
## ----------------------------------------------------------##

compute_main_animal_pop <- function(year, animal_class) {
  # computes the total population for the specified animal class
  # Dairy cows are not calculated separately
  # unit: head yr-1
  
  main_pop_df <- create_main_csv()
  
  animal_pop <- get_animal_pop(year = year, animal_class = animal_class)
  # if bovine, then do not account for dairy cows (id_col)
  ifelse(animal_class=='Bovine',
         start_id <- 5,
         start_id <- 4)
  main_pop_df[, as.character(year)] <- rowSums(animal_pop[, seq(start_id, ncol(animal_pop))])
  
  return(main_pop_df)
  rm(list=c('animal_pop', 'start_id'))
}


compute_animal_tot_Nexc <- function(year, animal_class) {
  # computes the total N excreted per animal class
  # Dairy cows are not calculated separately
  # unit: kg N yr-1
  
  main_pop_df <- create_main_csv()
  animal_Nex <- get_module_subfolder_output(module = 'MMS', 
                                            submodule = 'Total_Nexcretion', 
                                            submoduleX2 = as.character(year), 
                                            file_pattern = animal_class)
  # if bovine, then do not account for dairy cows (id_col)
  ifelse(animal_class=='Bovine',
         start_id <- 5,
         start_id <- 4)
  main_pop_df[, as.character(year)] <- rowSums(animal_Nex[, seq(start_id, ncol(animal_Nex))])
  
  return(main_pop_df)
  rm(list=c('animal_Nex', 'start_id'))
}


compute_animal_tot_Nret <- function(year, animal_class) {
  # computes animal N retention based on Oenema et al 2014
  # N intake = N retention + N exc, where N in = N ret / NUE
  # Thus
  # N ret = NUE * N exc / (1 - NUE)
  # unit: kg N yr-1
  
  animal_TNex <- compute_animal_tot_Nexc(year, animal_class)
  animal_NUE <- get_animal_NUE(animal_class)
  N_retention <- animal_TNex
  N_retention[, as.character(year)] <- round(
    animal_TNex[, as.character(year)] * animal_NUE / (1 - animal_NUE), 2)
  
  return(N_retention)
  rm(list=c('animal_TNex', 'animal_NUE'))
}

compute_animal_tot_Nintake <- function(year, animal_class) {
  # compute total animal N intake based on Oenema et al 2014
  # N intake = N retention + N excretion
  # unit: kg N yr-1
  
  N_retention <- compute_animal_tot_Nret(year, animal_class)
  N_exc <- compute_animal_tot_Nexc(year, animal_class)
  N_intake <- N_retention
  
  N_intake[, as.character(year)] <- round( N_retention[, as.character(year)]  + N_exc[, as.character(year)] , 2)
  return(N_intake)
  rm(list=c('N_retention', 'N_exc'))
}



## ---------------- NON-DAIRRY CATTLE DMI CALCULATIONS ---------------- ##
## ---------------------------------------------------------------------##

compute_nondairy_REM <- function() {
  # step 1 for nondairy cattle DMI
  # REM is the ratio of net energy available in a diet for maintenance to digestible energy consumed
  # IPCC (2006) Equation 10.14
  # REM = 1.123 - (4.092/1000 * DE) + [1.126/10^5 * DE^2]-25.4/DE
  # unit: dimensionless
  
  feed_DE <- get_nondairy_param('DE_frac', 2000) # constant var
  REM <- round(
    1.123 - (4.092*10^(-3)*feed_DE) + (1.126*10^(-5)*feed_DE^2) - 25.4/feed_DE , 2)
  return(REM)
}

compute_nondairy_NE_ma <- function() {
  # step 2 for nondairy cattle DMI
  # NE_ma is the estimated dietary net energy concentration of died
  # IPCC (2006) Table 10.8
  # unit: MJ kg DM-1 
  
  REM <- compute_nondairy_REM()
  feed_DE <- get_nondairy_param('DE_frac', 2000)
  
  NE_ma <- REM * 18.45 * feed_DE/100
  return(NE_ma)
  rm(list=c('REM', 'feed_DE'))
}

compute_nondairy_head_DMI <- function(year) {
  # calculation of nondairy for mature and growing population
  # IPCC (2006) Equation 10.17
  # first DMI is calculated on a daily basis, given in kg DM day-1 hd-1
  # then the annual DMI is calculated for each nondairy animal
  # unit: kg DM yr-1 hd-1
  
  BW <- feed_DE <- get_nondairy_param('avg_mass', year)
  NE_ma <- compute_nondairy_NE_ma()
  DMI <- round(
    BW^(0.75) * ( (0.2444 * NE_ma - 0.0111 * NE_ma^(2) - 0.472) / NE_ma), 2)
  DMI <- round (DMI * 365, 2)
  
  return(DMI)
  rm(list=c('BW', 'NE_ma'))
}


compute_nondairy_DMI <- function(year) {
  # computes the DMI of the total bovinne population
  # Total DMI = DMI * Bovine_pop
  # unit: kg DM yr-1
  
  nondairy_pop <- compute_main_animal_pop(year, 'Bovine')
  nondairy_DMI <- compute_nondairy_head_DMI(year)
  
  nondairy_DMI_pop <- nondairy_pop
  # compute total Bovine DMI
  nondairy_DMI_pop[, as.character(year)] <- round(
    nondairy_DMI_pop[, as.character(year)] * nondairy_DMI, 2)
  return(nondairy_DMI_pop)
}

compute_nondairy_concentrate_N <- function(year, concentrate_FRAC) {
  # computes nondairy N concentrate feed intake
  # N_conc = (Total DMI - (total DMI /1000 * 0.1) * 2%N
  # unit: kg N yr-1
  
  # set condition for concentrate fraction
  if (missing(concentrate_FRAC)==TRUE) {
    concentrate_FRAC <- 0.1
  }
  
  # set concentrate feeding N concent (eq wheat 2%N)
  concentrate_N <- 20 # 2 kg N tonnes dm-1
  
  # compute total DMI (kg DM yr-1) -----------------------------------------------------------------
  nondairy_DMI <- compute_nondairy_DMI(year)
  # compute total DMI and N intake for concentrate feeding -----------------------------------------
  nondairy_DMI[, 4] <- round( 
    (nondairy_DMI[, 4]/1000 * concentrate_FRAC ) * concentrate_N, 2)
  
  write_annual_data(module_name =  'CropProduction_module', 
                    subfolder_name = 'Concentrate_N', 
                    file = nondairy_DMI, 
                    filename = 'Bovine', 
                    year = year)
  rm(list=c('concentrate_FRAC', 'concentrate_N', 'nondairy_DMI'))
}


compute_nondairy_roughage_N <- function(year, concentrate_FRAC) {
  # computes the N in roughage for bovine population (except dairy)
  # N roughage = N intake - N conc
  # unit: kg N yr-1
  
 nondairy_Nintake <- compute_animal_tot_Nintake(year, 'Bovine')
 nondairy_Nconc <- get_module_subfolder_output(module = 'CropProduction_module', 
                                               submodule = 'Concentrate_N', 
                                               submoduleX2 = year,
                                               file_pattern = 'Bovine')
 
 nondairy_Nroughage <- nondairy_Nconc
 # compute N in roughage --------------------------------------------------------------------------
 nondairy_Nroughage[, as.character(year)] <- round(
   nondairy_Nintake[, as.character(year)] - nondairy_Nconc[, as.character(year)], 2)
 
 write_annual_data(module_name =  'CropProduction_module', 
                   subfolder_name = 'Roughage_N', 
                   file = nondairy_Nroughage, 
                   filename = 'Bovine', 
                   year = year)
 rm(list=c('nondairy_Nintake', 'nondairy_Nconc', 'nondairy_Nroughage'))
}



compute_totals_concentrate_N <- function(year, concentrate_FRAC) {
  # computes N in concentrate feeding for the allocated animal classes
  # unit: kg N yr-1
  
  concentrate_animals <- c('Poultry', 'Swine', 'Rabbits', 'Bovine')
  for (i in concentrate_animals) {
    
    if (i=='Poultry' | i=='Swine' | i=='Rabbits') {
      
      N_intake <- compute_animal_tot_Nintake(2009, i)
      write_annual_data(module_name =  'CropProduction_module', 
                        subfolder_name = 'Concentrate_N', 
                        file = N_intake, 
                        filename = i, 
                        year = year)
    }
    else {
      N_intake <- compute_nondairy_concentrate_N(year, concentrate_FRAC)
      write_annual_data(module_name =  'CropProduction_module', 
                        subfolder_name = 'Concentrate_N', 
                        file = N_intake, 
                        filename = i, 
                        year = year)
    }
  }
  rm(list='N_intake')
}

compute_totals_roughage_N <- function(year, concentrate_FRAC) {
  # computes the total roughage N for the allocate animal classes
  # unit: kg N yr-1
  
  roughage_animals <- c('Bovine', 'Equides', 'Goats', 'Sheep')
  for (i in roughage_animals) {
    
    if (i=='Equides' | i=='Goats' | i=='Sheep') {
      
      N_intake <- compute_animal_tot_Nintake(2009, i)
      write_annual_data(module_name =  'CropProduction_module', 
                        subfolder_name = 'Roughage_N', 
                        file = N_intake, 
                        filename = i, 
                        year = year)
    }
    else {
      N_intake <- compute_nondairy_roughage_N(year, concentrate_FRAC)
      write_annual_data(module_name =  'CropProduction_module', 
                        subfolder_name = 'Roughage_N', 
                        file = N_intake, 
                        filename = i, 
                        year = year)
    }
  }
  rm(list='N_intake')
}
