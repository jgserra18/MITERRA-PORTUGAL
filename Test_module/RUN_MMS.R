source('./MMS_module/Functions/Storage_MMS.R')
source('./MMS_module/Functions/Grazing_MMS.R')
source('./MMS_module/Functions/Housing_MMS.R')
source('./MMS_module/Functions/Spreading_MMS.R')
source('./MMS_module/Functions/Nex_computation.R')
source('./MMS_module/Functions/Totals_aggregation_MMS.R')
source('./Main_functions.R')


## ----------------------- DAIRY COW N EXCRETION COEFFICIENT ----------------------- ##
## ----------------------------------------------------------------------------------##

## Compute dairy cow Nex based on milk productivity and its conversion based on the standard milk production (7000 kg cow-1) and Nex of 115 kg N cow-1
## unit: kg N head-1 yr-1
compute_dairy_Nex(year = 2009)


## ----------------------- N EXCRETION PER DISTRIBUTION SYSTEM ----------------------- ##
## ------------------------------------------------------------------------------------##

## Computes TOTAL N excretion for each animal class and respective subclasses
## unit: kg N yr-1
compute_Nex_subclass(year = 2009)

## Computes N excreted onto pastures for each animal class and respective subclasses
## unit: kg N yr-1
compute_N_excretion_grazing(year = 2009)
  # sums total animal class N excretion onto pastures
  sum_graz <- aggregate_totals(year = 2009, subfolder = 'Grazing', subfolderX2 = 'N_excretion')


## Computes N excreted in yards for each animal class and respective subclasses
## unit: kg N yr-1
compute_N_excretion_distribution(year = 2009, dist_system = 'Yards')

## Computes N excreted in housing for each animal class and respective subclasses
## this function automatically calculates N excreted in solid and liquid systems as well as its total
## unit: kg N yr-1
compute_N_excretion_distribution(year = 2009, dist_system = 'Housing')


## ----------------------- N-NH3 EMISSIONS IN HOUSING + YARDS + GRAZING ----------------------- ##
## ---------------------------------------------------------------------------------------------##

## Computes NH3 emissions from N excreted onto pastures
## unit: kg N-NH3 yr-1
compute_NH3_grazing(year = 2009)

## Computes NH3 emissions from N excreted in yards
## unit: kg N-NH3 yr-1
compute_NH3_yards(year = 2009)

## Computes NH3 emissions from N excreted in housing for solid and liquid systems and their total
## unit: kg N-NH3 yr-1
compute_NH3_housing_mms(year = 2009)
compute_NH3_housing_total(year = 2009)


## ----------------------- N-NH3 STORAGE MANURE ----------------------- ##
## ---------------------------------------------------------------------##

## Computes TAN and N entering solid manure storage, corrected to account for animal bedding systems
compute_solid_TAN_entering_storage(year = 2009, manure_spreading = FALSE)
compute_solid_N_entering_storage(year = 2009, manure_spreading = FALSE)

## Computes TAN and N entering ?slurry manure storage
## This is not the "activity data" for slurry storage gaseous emissions
compute_slurry_TAN_entering_storage(year = 2009, manure_spreading = F)
compute_slurry_N_entering_storage(year = 2009, manure_spreading = F)

## Compute N-NH3 losses from manure storage
## Method: EMEP (2016) Tier 2 EF
## Unit: kg N-NH3 yr-1
compute_storage_gaseous_emissions(year = 2009, gaseous_source = 'NH3')
compute_totals(year = 2009, subfolder = 'Storage', subfolderX2 = 'NH3')


## ----------------------- N-N2O EMISSIONS IN MMS ----------------------- ##
## -----------------------------------------------------------------------##

## Computes N2O emissions from N excreted onto pastures
## Method: IPCC (2006) Tier1 emission factors
## unit: kg N-N2O yr-1
compute_N2O_grazing(year = 2009)

## Computes N2O emissions from storage
## Method: EMEP (2016) Tier 2 EF
## Unit: kg N-N2O yr-1
compute_storage_gaseous_emissions(year = 2009, gaseous_source = 'N2O')
compute_totals(year = 2009, subfolder = 'Storage', subfolderX2 = 'N2O')


## ----------------------- N-NOx EMISSIONS IN MMS ----------------------- ##
## -----------------------------------------------------------------------##

## Computes NOx emissions from N excreted onto pastures
## Method: EMEP (2016) tier1 emission factors
## unit: kg N-NOx yr-1
compute_NOx_grazing(year = 2009)

## Computes NOx emissions from manure storage
## Method: EMEP (2016) tier2 emission factors
## unit: kg N-NOx yr-1
compute_storage_gaseous_emissions(year = 2009, gaseous_source = 'NOx')
compute_totals(year = 2009, subfolder = 'Storage', subfolderX2 = 'NOx')


## Computes N2 emissions from manure storage
## Method: EMEP (2016) tier2 emission factors
## unit: kg N-N2 yr-1
compute_storage_gaseous_emissions(year = 2009, gaseous_source = 'N2')



## ----------------------- N-NH3 MANURE SPREADING ----------------------- ##
## -----------------------------------------------------------------------##

## Computes N-NH3 emissions from manure spreading
## Alrady accounts for manure application rates at the municipality scale (Statistics Portugal, 2009)
## Unit: kg N-NH3 yr-1
compute_NH3_manure_spreading(year = 2009, manure_type = 'slurry')
compute_NH3_manure_spreading(year = 2009, manure_type = 'solid')
compute_totals(year = 2009, subfolder = 'Spreading', subfolderX2 = 'NH3')

compute_totals(year = 2009, subfolder = 'Spreading', subfolderX2 = 'Gross_soil_manure_N')


## ----------------------- TOTALS AGGREGATION ------------------------- ##
## ---------------------------------------------------------------------##
##### EXAMPLE #####
aggregate_totals(year = 2009, 
                 subfolder = 'Spreading',
                 subfolderX2 = 'Gross_soil_manure_N',
                 manure_type = 'solid')
