source('./BNF_module/Functions/BNF_functions.R')

# Method employed: Baddeley et al., 2014

## ----------------------- FORAGE LEGUMES BNF CALCULATION ----------------------- ##
## -------------------------------------------------------------------------------##

# calculate the N fixed by forage legumes in intensive and extensive grasslands
# unit: kg N yr-1
compute_forage_BNF(year = 2009)

# calculate the sum of forage legumes BNF
# unit: kg N yr-1
compute_total_forage_Nfixation(year = 2009)


## ----------------------- GRAINS LEGUMES BNF CALCULATION ----------------------- ##
## -------------------------------------------------------------------------------##
 
# compute AB N, BG N, rhizo N, total N and N fixed by all pulse  crops
# unit: kg N yr-1
compute_grain_Nfixation(year = 2009)

# computes total grain legumes N fixation 
# unit: kg N yr-1
compute_total_grain_Nfixation(year = 2009)

## ----------------------- TOTAL BNF CALCULATION ----------------------- ##
## ----------------------------------------------------------------------##

# computes the sum of grain and forage N 
compute_total_BNF(year = 2009)

## ----------------------- LEGUMES N FIXED PER CROP ACREAGE----------------------- ##
## --------------------------------------------------------------------------------##
compute_acreage_Nfixation(year = 2009)