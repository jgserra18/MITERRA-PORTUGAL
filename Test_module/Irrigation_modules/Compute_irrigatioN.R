source('./Irrigation_module/Functions/Compute_1999_areas.R')
source('./Irrigation_module/Functions/no3_functions.R')
source('./Irrigation_module/Functions/compute_irrigatioN.R')

###### COMPUTE IRRIGATION N FOR EACH IRRIGATION SYSTEM AND EACH WATER SOURCE ######
###### COMPUTE TOTAL IRRIGATION N FOR EACH YEAR #####
compute_irrigatioN <- aggregate_N_irrigation_source(year = 1999, efficiency = FALSE, write = TRUE)

