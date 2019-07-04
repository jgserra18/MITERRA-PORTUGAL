print_module_hierarchy <- function()
{
	print('The model simulates each sub-module independently. However:\n')
	print('The model is computed using the following sequence :\n')
	print('1 - Calculate the GNB.')
	print('2 - Calculate N-NH3 emissions.')
	print('3 - Calculate N-NOx emissions.')
	print('4 - Calculate N-N2O emissions.')
	print('5 - Calculate N-runoff losses.')
	print('Optional - Calculate the Nitrogen Surplus (GNB-NH3 emissions')
	print('6 - Calculate the Soil Surface Nitrogen Balance (GNB-gaseous N losses - Runoff).')
	print('7 - Calculate N-leaching.')
	print('8 - Calculate denitrification to di-nitrogen.')
}

## RUN IRRIGATION MODULE ---------------------------------
irrigation_submodule <- './Test_module/Irrigation_modules/'
#sapply(list.files(irrigation_submodule, full.names=T), source)

  # 1 - Compute irrigation volumes for 2009
  source('./Test_module/Irrigation_modules/Compute_irrigation_volumes.R')
  # 2 - Compute irrigation data for 1999
  source('./Test_module/Irrigation_modules/Compute_irrigation_1999.R')
  # 3 - Compute N-input from irrigation for both years
  source('./Test_module/Irrigation_modules/Compute_irrigatioN.R')

## RUN GNB ACTVITY DATA ----------------------------------
source('./Test_module/RUN_GNB_activity_data.R')

## RUN GNB -----------------------------------------------
source('./Test_module/RUN_GNB.R')

## RUn N in manure application ---------------------------
source('./Test_module/Calculation_modules/RUN_manure_application+other_usage.R')

## RUN GASEOUS emissions ---------------------------------
  # 1 - NH3 emissions
  source('./Test_module/RUN_NH3.R')
  # 2 - NOx emissions
  source('./Test_module/Calculation_modules/RUN_nox_calculation.R') 
  source('./Test_module/RUN_NOx.R')
  # 3 - N2O emissions
    #3.1 - Run grazing N2O emissions
    source('./Test_module/Calculation_modules/RUN_n2o_grazing.R')
    # 3.2 - Run irrigation N2O emissions
    source('./Test_module/Irrigation_modules/Compute_N2O_irrigatioN.R')  
    # 3.3 - Run all n2O emissions
    source('./Test_module/Run_N2O_tier1.R')

## RUN runoff module -------------------------------------
  # 1 - Run irrigation N runoff
  source('./Test_module/Irrigation_modules/Compute_irrigatioN_losses.R')
  # 2 - Run general runoff losses
  source('./Test_module/RUN_runoff.R')

## Run other N balance approaches ------------------------
source('./Test_module/RUN_N_balances.R')

## Run N-leaching module ---------------------------------
  # 1 - Run total N-leaching
  source('./Test_module/RUN_leaching.R')
  # 2 - Run disaggregated pathways of N-leaching
  source('./Test_module/RUN_leaching_sources.R')

# Run NO3 concentration to GW ----------------------------
source('./Test_module/RUN_nloading.R')

# Run indirect N2O emissions -----------------------------
source('./Test_module/RUN_indirect_N2O_emissions.R')
