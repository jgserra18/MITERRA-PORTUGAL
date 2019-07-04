#====================================================================================================#
#####################################  IPCC N2O Tier 1 ###############################################
#====================================================================================================#"
################ THIS IS USED TO CALCULATE AND LATER COMPILE DIRECT N2O EMISSIONS ####################

#load used submodules
source('./Main_functions.R')
source('./Gaseous_functions.R')
#load global gaseous function to compute tier 1 emissions
source('./Gaseous_N_losses/Emission function/Compute_tier1_emission.R')

#====================================================================================================#
#######################################  Calculations ################################################
org_fert99 <- select_inputs_cols(1999)
org_fert09 <- select_inputs_cols(2009)

#compute N-N2O emissions from manure applied to soil N 
#and write to activity data
n2o_manure_app99 <- compute_manure_app(1999, 'N2O')
n2o_manure_app09 <- compute_manure_app(2009, 'N2O')

#compute N-N2O emissions from sludge and fertiliser N applied to soil
#and write to activity data
n20_inputs99 <- compute_org_fert(1999, 'N2O')
n2o_inputs09 <- compute_org_fert(2009, 'N2O')