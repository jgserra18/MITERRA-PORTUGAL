source('./MMS_module/Functions/Totals_aggregation_MMS.R')
source('./Fertilization_module/Functions/Crop_N_requirements.R')
source('./Fertilization_module/Functions/Fertilizer_distribution_mechanism.R')
source('./Fertilization_module/Functions/Manure_distribution.R')
source('./MMS_module/Functions/Nex_computation.R')
source('./Fertilization_module/Functions/Biosolids_distribution.R')
source('./Fertilization_module/Functions/Fertilizer_N.R')


## ----------------------- CROP N R REQUIREMENTS ----------------------- ##
## ----------------------------------------------------------------------##

# compute fertilizer rates for a given year for each crop at the agrarian region level
# unit: kg N yr-1
compute_fertilizer_rates(year = 2009)

# compute N requirements based on the recommended fertilizer rates at the agrarian region
# this is done for each crop for a given year
# unit : kg N yr-1
compute_crop_N_requirements(year = 2009)


## ----------------------- MANURE CROP DISTRIBUTION----------------------- ##
## ------------------------------------------------------------------------##

# manure is distributed over the main corp categories, barring horticulture and industry crops
# Manure distribution assumptions
# 25 % of pig manure AND 100% ruminant manure --> fodder crops + intensive pasture
# 100% of poultry, equides, rabbits and 75% pig manure --> non-fodder crops
# unit: kg N yr-1
## IMPORTANT: THIS CALCULATES SEPARATELY THE MANURE N_FRV for slurry AND solid manure
manure_crop_distribution_mechanism(year = 2009)

# calculates the sum of manure N_FRV of slurry and solid
# unit: kg N yr-1
compute_sum_manure_crop_distribution(year = 2009)


## ----------------------- BIOSOLIDS----------------------- ##
## ---------------------------------------------------------##

# This function has two main purposes:
  # 1 - Calibre sludge N applied in agriculture at the municipality level 
  # based on total sludge produced and historical mainland values
  
  # 2 - Distribute Lisbon's municipality sludge N applied in agriculture 
  # to the remaining AML municipalities
# unit: % of total crop acreage 
# unit: kg N yr-1
sludgeN_Lisbon_distribution(year = 2009)

# Distribute sludge N for different crops according to crop municipality fraction
# unit: % of total crop acreage 
# unit: kg N yr-1
sludgeN_crop_acreage_distribution(year = 2009)
compute_sludgeN_crop_distribution(year = 2009)

# compute crop sludge N FRV for the fertilizer distribution mechanism
# unit: kg N FRV yr-1
compute_biosolids_crop_FRV(year = 2009)


## ----------------------- FERTILISER N MECHANISM---------------------------------------------------------------- ##
## ---------------------------------------------------------------------------------------------------------------##


## ----------------------- UNFIT FERTILISER N----------------------- ##
## ------------------------------------------------------------------##

# this function employs several sub-functional routines
# 1 - MANURE: 
  # 1.1 - calculates the difference of crop N requirements and crop manure N FRV (kg N yr-1)
  # 1.2 - calculates manure surplus following spreading (kg N yr-1)
  # 1.3 - calculates crop manure application rates (kg N ha_crop-1 yr-1)
# 2 - BIOSOLID
  # 2.1 - calculates the difference of the remaining crop N demand (1.1) and crop biosolid N FRV (kg N yr-1)
  # 2.2 Sludge surplus is not calculated currently  
# 3 - Fertiliser
  # 3.1 - Calculates crop fertiliser N (from 2.1)
  # 3.2 - Calculates crop fertiliser N rate (kg N ha_crop-1 yr-1)
compute_mechanism_crop_fertiliserN(year = 2009) 

# calculates fertiliser N from crops not organically fertilized (i.e., horticulture, industry)
# also updates the real fertilization rates
# unit: kg N yr-1
compute_vegetable_fertiliserN(year = 2009) 


## calculate totals of unfit fertiliser N for each maincrop and municipality
## unit: kg N yr-1
compute_total_unfit_fertiliser_N(year = 2009)

## ----------------------- FAN FERTILISER N----------------------- ##
## ----------------------------------------------------------------##

# calibrate crop fertiliser N and fertiliser N totals through FAN
# FAN = Historical fert N / estimated fert N

# adjust crop fertilization N rates based on FAN
# unit: kg N ha_crop-1 yr-1
# AND !!!!
# adjusts fertiliser N vaariants
# unit: kg N yr-1
compute_adjusted_crop_fertiliser_N(year = 2009)
