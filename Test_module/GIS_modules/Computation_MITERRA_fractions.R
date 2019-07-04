source('./GIS_module/Function/MITERRA_fractions.R')

## Pre-Process GIS rasters to a suitable spatial resolution/extent
#note: untested but it should work
preProcessing_rasters()

## Compute all MITERRA fractions without writing anything
#id 1 - Rf_min, id 2- Rf, i3 _ Lf_min, id 4 - Lf
mit_fracs99 <- compute_all_fractions(1999)
mit_fracs09 <- compute_all_fractions(2009)

## Writes all MITERRA fractions
# does not work due to something related to the ifelse, DOES NOT MATTER
write_runoff_leaching_fractions()
