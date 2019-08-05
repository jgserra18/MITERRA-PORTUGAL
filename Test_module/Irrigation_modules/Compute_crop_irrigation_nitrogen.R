source('./Irrigation_module/Functions/Compute_crop_irrigatioN.R')

#computes total irrigation areas for each crop for the year 2009
compute_tot_irrig_areas09()

#computes proportion of each crop comparatively to total irrigated areas for the year 2009
#note: used to extrapolate 1999 areas
compute_prop_crop09()

#xtrapolate 1999 crop areas based on 2009 data
extrapolate_1999_crop_areas()

#compute irrigation systems per crop for the year 1999 based on 2009 data
populate_output_irrig_areas99()

#compute corrected irrigated areas based on 1999 total crop areas
correct_output_irrig_areas99()
## ---------------------------------------------------------------------------------- 

## loops through both individual and total crop irrigatioN for both years
## output is dataframe files for either individual crops or aggregated into main crop categories

## ---------------------------------------------------------------------------------- 
loop_through_crop_irrigatioN()
