source('./GIS_module/Function/Irrigation/Runoff_N_module.R')

## load runoff assumptions dataframe
runoff_assumptions_df()

## allocate the available irrigation N, which acts as activity data for runoff N losses from irrigation
## to different LU classes, besides heterogeneous which was computed separately
## note that localized irrigation systems (drip, microsprinkler) and rice have no runoff losses 
## so these were not converted to a GIS-format
## output: writes the raster for the subsequent main irrigation systems to runoff activity data subfolder
## unit: kg N/LU ha
spatial_allocation_runoffN_LU(year = 2009, all = FALSE)

## similar function but applied to heterogeneous LU class
spatial_allocation_runoffN_heterogeneous(year = 2009, all = FALSE)

## create a raster mosaic for each main irrigation system
## merging heterogeneous and the remaining LU classes
## unit: kg N/LU ha
mosaic_runoff_main_irrig_systems(year = 2009)

## calculate irrigation-specific runoff fractions based on soil texture and the underlying assumptions
## optional as this is called in the main function
call_runoff_fractions_irrig_systems(year = 2009, main_irrig_system = 'sprinkler')

## gets the raster mosaic for runoff N losses (kg N/LU ha) for the specified main irrig system
compute_irrig_runoff_nha_mosaic(year = 2009, main_irrig_system = 'sprinkler', write = FALSE)

## loops compute_irrig_runoff_nha_mosaic() to every main irrigation system with runoff losses
loop_runoff_nha(year = 2009)

## computes total runoff losses (tot_rf_irrig)
## #tot_rf_irrig = rf_irrig_sprinkler + rf_irrig_furrow + rf_irrig_other_grav
## unit: kg N/LU ha
compute_total_LU_runoff_nha(year = 2009, write = FALSE)

## compute total N runoff losses at the municipality scale
## units: both in kg N and kg N/UAA ha
compute_runoff_nha_muni(2009, write = T)
