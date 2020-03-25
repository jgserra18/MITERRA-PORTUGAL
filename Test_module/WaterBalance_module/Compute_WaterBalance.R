source('./WaterBalance_module/Functions/Water_Balance_funcs.R')

## compute the weighted average of crop ET0_kc coefficients
compute_avg_wght_crop_kc()

# computes seasonal ET0 for the summer/spring, autumn and winter
# unit : mm/yr
loop_seasonal_ET0()

# computes seasonal precipitation 
# unit: mm(yr
loop_seasonal_prec_df()
loop_idw_seasonal()

# compute for each LU class the associated LU kc coefficient based on the allocated crop areas and respective proportions
# this is done on a municipality basis and as data frame
compute_LU_crop_kc(year = 1999)
compute_LU_crop_kc(year = 2009)

# spatial allocate LU kc to GIS_LU
spatial_allocation_LU_kc(year = 1999)
spatial_allocation_LU_kc(year = 2009)

# create country mosaic of LU kc
create_mosaic_LU_kc(year = 2009)
create_mosaic_LU_kc(year=1999)

# compute summer ET_kc
# unit : mm/yr
compute_summer_ETkc_LU(year = 1999)
compute_summer_ETkc_LU(year = 2009)

# export gross irrigation (m3/ha/yr) to WB_submodule
export_gross_irrig_vol(year = 1999)
export_gross_irrig_vol(year = 2009)

# compute seasonal water surplus as well as water runoff
# unit: m3/ha/yr
loop_seasonal_water_surplus()

# create WS mosaic
construct_WaterBalance_mosaic_annual(year = 2009)
construct_WaterBalance_mosaic_annual(year = 1999)
