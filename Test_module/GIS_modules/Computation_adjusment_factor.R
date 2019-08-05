source('./GIS_module/Function/UAA_adjusment_factor.R')

## ADJUSTMENT FACTORS FOR LEACHING ----------------------------------------
## COMPUTE ADJUSMENT FACTORS AND RASTERIZE THESE AT THE MUNI SCALE
adj99 <- compute_adjustment_factor(1999, T)
adj09 <- compute_adjustment_factor(2009, T)

ras_adj99 <- rasterize_adjs_factor(1999)
ras_adj09 <- rasterize_adjs_factor(2009)

## ADJUSTMENT FACTORS FOR LU MODELLING ----------------------------------------
source('./GIS_module/Function/LU_irrigation_allocation.R')
adj99 <- compute_LU_class_adj_factor(1999)
adj09 <- compute_LU_class_adj_factor(2009)

mosaic99 <- create_LU_adj_factor_mosaic(1999)
mosaic09 <- create_LU_adj_factor_mosaic(2009)