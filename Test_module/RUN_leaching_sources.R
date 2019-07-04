source('./Leaching_module/Function/Compute_leaching.R')
source('./Leaching_module/Function/compute_correct_surface_water_areas.R')

## COMPUTE AREAS OF SURFACE WATERS INTERSECTED TO EACH GW
#surface_areas <- compute_area_export()

## CORRECT AREAS OF SURFACE WATERS BASED ON THE CORRECTED UAA PER CAA
#EXAMPLE ;; OPTIONAL
#1 - large, 2 - small
#corr_areas99 <- compute_correct_surface_areas(1999, 1)

## MAIN DF WITH CORRECT SURFACE AREAS
tier <- c('tier2_irrig', 'tier2_ssnb')

for (i in tier)
{
  # LOAD SURFACE WATERBODIES CORRECTED AREAS
  df99 <- aggregate_surface_areas(1999, tier_ssnb)
  df09 <- aggregate_surface_areas(2009, tier_ssnb)
  ## MAIN DF WITH LEACHING
  main_df99 <- aggregate_leaching_surface(1999, tier_ssnb = tier)
  main_df09 <- aggregate_leaching_surface(2009, tier_ssnb = tier)
  # FINAL DF WITH LEACHING PER SOURCE
  leaching_db99 <- compute_gw_leaching(1999, tier_ssnb = tier)
  leaching_db09 <- compute_gw_leaching(2009, tier_ssnb = tier)
  # COMPUTE LEACHING FRACTIONS PER SOURCE
  Lfrac99 <- compute_leaching_frac(1999)
  Lfrac09 <- compute_leaching_frac(2009)
  
  ## EXPORT DATA -------------------------------------------------------------
  # export leaching fractions per source (in %)
  write_leaching_output(Lfrac99, 'lfraction', 1999, 'tier2')
  write_leaching_output(Lfrac09, 'lfraction', 2009, 'tier2')
  # export 
  write_leaching_output(leaching_db99, 'source_leaching_irrig', 1999, 'tier2')
  write_leaching_output(leaching_db09, 'source_leaching_irrig', 2009, 'tier2')
}

populate_gw_leaching_dataset_surface()
