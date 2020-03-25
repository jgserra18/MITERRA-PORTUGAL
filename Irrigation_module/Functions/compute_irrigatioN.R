source('./Irrigation_module/Functions/Compute_1999_areas.R')
source('./Irrigation_module/Functions/no3_functions.R')
source('./Irrigation_module/Functions/Compute_crop_irrigatioN.R')



#disaggregates the water source according to the user specification and calculates the water volume of each irrig sys  accordingly
#d <- disaggregate_irrig_sys_per_water_source(2009, TRUE, 'wells')
disaggregate_irrig_sys_per_water_source <- function(year, efficiency, water_source)
{
  water_source_df <- specify_water_source(water_source)
  irrig_sys_vol <- get_irrig_sys_volumes(year, efficiency)
  
  for (i in 4:ncol(irrig_sys_vol))
  {
    irrig_sys_vol[, i] <- irrig_sys_vol[, i]*water_source_df
  }
  return(irrig_sys_vol)
}

#general function to compute the irrigation N for every irrigation system but for a specific water source, e.g. wells, superficial, spring
compute_N_irrigation_source <- function(year, efficiency, water_source)
{
  kg_N_NO3 <- conversion_no3(year, water_source) #kg N/m3
  main_df <- disaggregate_irrig_sys_per_water_source(year, efficiency, water_source) #m3
  
  for (i in 4:ncol(main_df))
  {
    main_df[, i] <- round(main_df[, i] * kg_N_NO3, 2)
  }
  return(main_df)
}

#loops compute_N_irrigation_source and writes these to irrigation_module output
#if write==FALSE it returns the dataset containing irrigation N from each irrigation system, the total SUM and the kg N/UAA ha
aggregate_N_irrigation_source <- function(efficiency, write, year)
{
  ifelse(efficiency==TRUE, e <- 'w_eff_', e <- 'wo_eff_')
  
  ifelse(missing(year)==TRUE,
         year <- c(1999, 2009),
         year <- year)
  
  water_source <- c('spring', 'superficial', 'well')
  
  for (i in year)
  {
    df <- get_irrig_template()
    df$SUM <- 0
    uaa <- load_uaa(i)
    
    for (j in water_source)
    {
      ####################### COMPUTE AND WRITE IRRIGATION N FOR EACH IRRIGATION SYSTEM SEPARATELY FOR EACH SOURCE #################
      compute_irrigatioN <- compute_N_irrigation_source(i, efficiency, j)
      compute_irrigatioN$SUM <- rowSums(compute_irrigatioN[, 4:(ncol(compute_irrigatioN)-1)])
      compute_irrigatioN$nha <- compute_irrigatioN$SUM/uaa
  
      if (write==TRUE)
      {
        print(paste0('Writing ', j, ' data for ', i))
        write_irrig_output(paste0('Irrigation_N/', i), compute_irrigatioN, paste0(e, 'irrigatioN_', j, i))
        print('Finished!')
      }
      ###################################################### END #################################################################
      
      ########################################## COMPUTE AND WRITE TOTAL IRRIGATION N ############################################
      for (z in 4:(ncol(df)-1))
      {
        df[, z] <- df[, z] + compute_irrigatioN[, z]
      }
    }
      df$SUM <- rowSums(df[, 4:(ncol(df)-1)])
      df$nha <- df$SUM/uaa
      
      if (write==TRUE)
      {
        print(paste0('Writing total data for ', i))
        write_irrig_output(paste0('Irrigation_N/', i), df, paste0(e, 'total_irrigatioN', i))
        print('Finished!')
      }
      return(df)
      ###################################################### END #################################################################
  }
}

#computes avg efficiency at the municipality scale
#d - compute_avg_efficiency_muni(2009)
compute_avg_efficiency_muni <- function(year)
{
  warning(options=-1)
  wo_eff <- aggregate_N_irrigation_source(FALSE, FALSE, year)
  w_eff <- aggregate_N_irrigation_source(TRUE, FALSE, year)
  
  main_df <- create_main_csv()
  avg_eff <- round(w_eff[, ncol(w_eff)]/wo_eff[, ncol(wo_eff)], 2)
  main_df <- cbind(main_df, avg_eff)
  main_df <- data_cleaning(main_df)
  colnames(main_df)[4] <- 'avg_efficiency'
  
  return(main_df)
}


