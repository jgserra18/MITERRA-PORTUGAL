source('./CropProduction_module/Functions/crop_Nremoval_functions.R')
source('./CropProduction_module/Functions/Crop_acreage_distribution.R')

## ----------------------- CROP N OFFAKE---------------------- ##
## ------------------------------------------------------------##

# cROP AREAS AND YIELDS WERE COLLATED FROM STATISTICS PORTUGAL
# YIELDS ARE AVAILABLE AT THE AGRARIAN REGION LEVEL, FURTHER DISAGGREGATED INTO THEIR RESPECTIVE MUNICIPALITIES
# N CONTENT WERE COLLATED FROM RESOLUTION NO 1209

# VERY SIMPLE PROCEDURE
  # N OFFAKE = CROP_AREA * CROP_YIELD * N CONTENT


# COMPUTES THE TOTAL N OFFAKE FOR EACH INDIVIDUAL CROP
compute_total_N_offake(year = 2009)

# COMPUTES THE TOTAL N OFFTAKE AGGREGATED FOR EACH MAINCROP
compute_total_N_offake(year = 2009)

# COMPUTES THE TOTAL N IN CROP UPTAKE PER CROP AREA
# UNIT: KG N (CROP AREA)-1 YR-1
compute_crop_Nremoval_ha(year = 2009)