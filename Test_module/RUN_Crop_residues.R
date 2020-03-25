source('./CropProduction_module/Functions/crop_Residues_functions.R')
source('./CropProduction_module/Functions/Crop_acreage_distribution.R')


# METHODS DESCRIBED IN SERRA ET AL 2019 ---------------------------------------------

# CROP RESIDUES ARE DIVIDED INTO THREE DIFFERENT SUB MODULES
  # 1 - CROP RESIDUES LEFT ON THE FIELD (ipcc 2006)
  # 2 - CROP RESIDUES REMOVED FROM THE FIELD (ipcc 2006)
  # 3 - CROP RESIDUES BURNT IN SITUE (Dias 2002)

# TOTAL N IN CROP RESIDUES WERE ESTIMATED BASED ON IPCC (2006)
#  THIS APPLIES TO THOSE LEFT OR REMOVED FROM THE FIELD
#  THESE WERE ESTIMATED BASED ON THE REMOVED_FRAC OR ITS COMPLEMENTARY FRACTION (APA 2018)

# TOTAL N IN CROP RESIDUES BURNT IN SITU WERE ESTIMATED FOLLOWING DIAS (2002)
# BASED ON TREE DENSITIES, RESIDUES BIOMAASS FROM PRUNING PRACTICES AND AREA FRACTION BURNT


## ----------------------- CROP RESIDUES LEFT/REMOVED ON THE FIELD----------------------- ##
## ---------------------------------------------------------------------------------------##

# FUNCTION AUTOMATICALLY COMPUTES THE FRACTION OF A CROP RESIDUE LEFT AND REMOVED
# THIS IS DONE FOR EACH CROP
compute_crop_residuesN(year = 2009)

# COMPUTES THE TOTAL N FOR EITHER RESIDUES LEFT OR REMOVED FROM THE FIELD
# only municipality totals
compute_totals_residues_N(year = 2009, residue_practice = 'removed')
compute_totals_residues_N(year = 2009, residue_practice = 'left')


## ----------------------- CROP RESIDUES BURNT IN SITU----------------------- ##
## ---------------------------------------------------------------------------##

## COMPUTE TOTAL N IN BURNT RESIDUES FOR ALL PERMANENT CROPS AND CEREALS AND PASTURES
compute_crop_residues_burnt_N(year = 2009)

## COMPUTE MUNICIPALITY TOTALS
compute_totals_residue_burnt_N(year = 2009)

## ----------------------- RESIDUE ACREAGE DISTRIBUTION----------------------- ##
## ----------------------------------------------------------------------------##

## COMPUTE THE TOTAL N PER CROP AREA OF RESIDUES BURNT
## UNIT: KG N (CROP AREA)-1 YR-1
compute_crop_Nres_burnt_ha(year = 2009)

## COMPUTE THE TOTAL N PER CROP AREA OF RESIDUES REMOVED
## UNIT: KG N (CROP AREA)-1 YR-1
compute_crop_Nres_removed_ha(year = 2009)