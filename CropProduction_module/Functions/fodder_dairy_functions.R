source('./CropProduction_module/Functions/fodder_NonDairy_functions.R')
source('./Main_functions.R')

# timeperiod = 271 days
# BW dairy cow = 650 kg

# Volden 2011 -------------------------------------------------------------
  # IC = Intake capacity
  # FV_intake = total feed intake expressed in fill units



# ASSUMPTION:
  # To calculate DMI, IC = FV_intake (CHapter 10.1)


# Roughage -------------------------
# energy content  feed (FV_roughage/FSU) = 0.81 SFU (kg DM)-1
# FSU = 7.9 MJ (kg DM)-1
# organic matter digestibility  = 70 %
# NFD content of feed = 33 g (kg DM)-1
# sugar + starch content  = 12.5 g (kg DM)-1



# Concentreate -------------------------
# energy content of concentra feed (FV_concentrate/FSU) = 1.22 SFU (kg DM)-1
# FSU = 10.9 MJ (kg DM)-1
# organic matter digestibility of concentrate = 90 %
# NFD content of concentrate feed = 9 g (kg DM)-1
# sugar + starch content of concentrate = 69.2 g (kg DM)-1
# N content = 20 g (kg DM)-1

# Feed intake (FV) concentrate = 0.22 FV / kg DM


## ----------------------- FILL UNITS ----------------------- ##
## -----------------------------------------------------------##


compute_roughage_fill_value_FE <- function() {
  # Roughage fill value
  # Chapter 6.2, Eq. 10, Volden (2011)
  # unit: kg DM head-1 yr-1
    
  OMD <- 70 #OM digestibility
  NDF <- 33 #NDF
  
  eq_numerator <- round( 0.86 - OMD * 0.005, 2)
  eq_denominator <- round( 0.94 + 0.56 * exp( -0.000029 * (NDF/10)^2.9), 2)
  FV <- round( eq_numerator / eq_denominator, 2)
  return(FV)
}




## ----------------------- MAX INTAKE CAPACITY ----------------------- ##
## --------------------------------------------------------------------##

get_dairy_milk_prod <- function(year) {
  # gets dairy milk productivity for a given year
  # and subsets it
  # unit: kg milk head-1 yr-1
  
  dairy_MP <- get_module_subfolder_output(module = 'MMS_module', 
                                          submodule = 'Dairy_productivity', 
                                          submoduleX2 = year, 
                                          file_pattern = 'dairy_prod')
  dairy_MP <- subset(dairy_MP, select = c('nuts2_ID', 'dairy_productivity'))
  return(dairy_MP)
}


compute_dairy_ECM <- function(year) {
  # ACTIVITY DATA FOE VOLDEN 2011 EQUATION 10.2
  # computes the energy corrected milk (ie assumed statistical data)
  # kg milk hd-1 day-1
  
  DIM <- 271 #days
  dairy_MP <- get_dairy_milk_prod(year)
  dairy_MP[, 2] <- round( dairy_MP[, 2] / 271, 2)
  return(dairy_MP)
  rm(list='DI')
}


compute_IntakeCapacity_IC <- function(year) {
  # Intake capacity of lactating dairy cows
  # Volden 2011 Equation 10.2
  
  # loadm country-specific params
  DIM <- 271 #days in milk 
  BW <- 650
  ECM <- compute_dairy_ECM(year)
  
  # load params from VOlden 2011 Table 10.5
  a <- 2.82
  b <- 0.134
  c <- -0.0006
  d <- 0.55
  e <- 0.091
  f <- 575
  g <- 0.006
   
  # IC computation -------------------------------
  ECM[, 'IC'] <- round(
    a * DIM^(b) * e^(c * DIM) - DIM^(-d) + e * ECM[, 2] + (BW - f) * g, 2)
  
  return(ECM)
  rm(list=c('DIM', 'BW', 'ECM', 'a', 'b', 'c', 'd', 'e', 'f', 'g'))
}


## ----------------------- MAX INTAKE CAPACITY ----------------------- ##
## --------------------------------------------------------------------##









## ----------------------- CONCENTRATE N FEED ----------------------- ##
## -------------------------------------------------------------------##
## -------------------------------- TEMPORARY FIX --------------------##

# default is for 2009
compute_dairy_hd_concentrateN <- function(year) {
  # receives as input the fraction of concentrate feed in terms of total N excretion
  # computes the N input from concentrate feed per dairy cow per year
  # Dairy_N = conc_frac * Nex
  # unit: kg N head-1 yr-1
  
  df <- compute_regional_dairy_productivity(2009)[, c(1,2)]
  df$concentrate_FRAC <- c(0.24, 0, 0.201, 0, 0.249)
  
  # disaggregate frac concentrate from the NUTS2 to the municipality
  dairy_conc_FRAC <- disagg_Nex(df)
  
  # calculate kg N of cocnentrate feed per dairy cow ---------------
  #  get dairy Nex rate
  dairy_nex <- compute_dairy_Nex(year)
  # compute kg N of concentrate feed 
  dairy_nex[, 4] <- round( dairy_nex[, 4] * dairy_conc_FRAC[, 4], 2)
  
  return(dairy_nex)
  rm(list=c('df', 'dairy_conc_FRAC'))
}

compute_dairy_concentrate_N <- function(year) {
  # calculates the total N intake from concentrate feeding from a dairy cow population for a given year
  # unit: kg N yr-1
  
  dairy_hd_concN <- compute_dairy_hd_concentrateN(year)
  dairy_pop <- get_animal_pop(year = year, animal_class = 'Bovine')[, seq(1,4)]
  
  dairy_conc_N <- dairy_hd_concN
  dairy_conc_N[, 4] <- round( dairy_conc_N[, 4] * dairy_pop[, 4], 2)
  
  write_annual_data(module_name =  'CropProduction_module', 
                    subfolder_name = 'Concentrate_N', 
                    file = dairy_conc_N, 
                    filename = 'Dairy', 
                    year = year)
  rm(list=c('dairy_hd_concN', 'dairy_pop', 'dairy_conc_N'))
}


## ----------------------- DAIRY N FEEDING INTAKE ----------------------- ##
## ----------------------- DAIRY ROUGHAGE N INTAKE----------------------- ##
## -----------------------------------------------------------------------##

compute_dairy_Nintake <- function(year) {
  # computes dairy cows' N intake based on Oenema et al 2014
  # analogous approx of the remaining livestock class
  # unit: kg N yr-1
  
  animal_Nexc <- get_module_subfolder_output(module = 'MMS', 
                                            submodule = 'Total_Nexcretion', 
                                            submoduleX2 = as.character(year), 
                                            file_pattern = 'Bovine')
  dairy_Nexc <- animal_Nexc[, seq(1,4)]
  dairy_NUE <- get_animal_NUE('Dairy')
  dairy_Nret <- dairy_Nexc
  
  # calculate dairy N retention ---------------------------------------------
  dairy_Nret[, 4] <- round( dairy_Nexc[, 4] * dairy_NUE / (1 - dairy_NUE), 2)
  # calculate dairy N intake
  dairy_Nret[, 4] <- round( dairy_Nret[, 4] + dairy_Nexc[, 4], 2)
  
  return(dairy_Nret)
  rm(list = c('animal_Nexc', 'dairy_Nexc', 'dairy_NUE'))
}


compute_dairy_roughage_N <- function(year) {
  # computes the dairy N intake from roughage
  # N roughage = N intake - N concentrate
  # unit: kg N yr-1
  
  dairy_Nintake <- compute_dairy_Nintake(year)
  dairy_conc_N <- get_module_subfolder_output(module = 'CropProduction_module', 
                                              submodule = 'Concentrate_N', 
                                              submoduleX2 = year,
                                              file_pattern = 'Dairy')
  dairy_roughageN <- dairy_Nintake
  dairy_roughageN[, 4] <- round( dairy_Nintake[,4] - dairy_conc_N[, 4], 2)
  write_annual_data(module_name =  'CropProduction_module', 
                    subfolder_name = 'Roughage_N', 
                    file = dairy_roughageN, 
                    filename = 'Dairy', 
                    year = year)
  rm(list=c('dairy_Nintake', 'dairy_conc_N', 'dairy_roughageN'))
}
