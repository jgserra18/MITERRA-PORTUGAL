source('./Main_functions.R')
source('./Irrigation_module/Functions/compute_irrigatioN.R')
#source('./Runoff_module/Function/Compute_runoff_application.R')


#calculate N losses through inefficiency
#calculate average runoff fractions 
#apply runoff fractions to the difference; the remainder is leaching

#computes irrigation N inefficiencies for each irrig system
#d <- compute_irrigation_sys_inefficiency(2009, FALSE)
compute_irrigation_sys_inefficiency <- function(year, write)
{
  ifelse(missing(year)==TRUE, year <- c(1999, 2009), year <- year)

  for (i in year)
  {
    main_df <- create_main_csv()
    df_gIrrig <- aggregate_N_irrigation_source(FALSE, FALSE, i) #gross irrig (without efficiency)
    df_nIrrig <- aggregate_N_irrigation_source(TRUE, FALSE, i) #net irrig with efficiency
    
    nc <- ncol(df_gIrrig)-2
    
    compute_Nlosses <- df_gIrrig[, 4:nc]-df_nIrrig[, 4:nc] #from irrigation inefficiency
    computation_df <- cbind(main_df, compute_Nlosses)
    computation_df$SUM <- rowSums(computation_df[, 4:nc])
    computation_df$nha <- computation_df$SUM/load_uaa(i)
    
    if (write==TRUE)
    {
      print(paste0('Writing irrigation N inefficiencies data for ', i))
      write_irrig_output(paste0('Irrigation_inefficiencies/', i), computation_df, paste0('irrigatioN_inefficiencies', i))
      print('Finished!')
    }
    else {return(computation_df)}
    
    rm(computation_df)
  }
}

################################ AVERAGE RUNOFF FRACTION @ MUNICIPALITY  ###############################
#########################################################################################################

#loads runoff fractions at the CAA scale and then averages at the munic scale
get_runoff_fraction_muni <- function(year)
{
  options(warn = -1)
  #compute and create a dataaset with average runoff fraction at the muni scale
  rf_df <- load_rf_fraction(year)
  avg_rf_muni <- average_if(rf_df, 2)
  select_df <- avg_rf_muni[, c(3,6)]
  
  #properly merge with the standardized dataset for modelling
  muni <- create_main_csv()
  muni_rf <- merge(muni, select_df, 'Muni_ID', sort=FALSE)
  return(muni_rf)
}

################################ RUNOFF FROM IRRIGATION SYSTEMS FUNCTIONS ###############################
#########################################################################################################

#adds reduction factor based on soil type
soil_reduction_factor <- function(soil_df)
{
  siltclay_idx <- which(soil_df$predominant_soil == 'silt' | soil_df$predominant_soil=='clay') #this can be done because the ID order is the same
  soil_df[siltclay_idx, 'reduction_factor'] <- 1
  soil_df[-siltclay_idx, 'reduction_factor'] <- 0.5
  
  return(soil_df)
}

#gets predominant soil at the municipality scale
get_predominant_soil <- function()
{
  folder <- select_maindata_pattern('EnvironmentalConditions')
  soil_subfolder <- file.path(folder, 'Soil')
  file <- list.files(soil_subfolder, pattern = 'predominant_soil')
  file_path <- file.path(soil_subfolder, file)
  file <- read.csv(file_path)
  
  file <- soil_reduction_factor(file)
  return(file)
}

#sets the runoff of drip and flood systems to 0
runoff_drip_other_grav <- function(df_irrig_sys)
{
  conditions <- c('drip', 'other_grav', 'microaspersion')
  
  for (i in conditions)
  {
    df_irrig_sys[, i] <- 0
  }
  return(df_irrig_sys)
}

#computes runoff of sprinkler systems for predominantly sandy soils and sets it to 0
compute_runoff_sandy <- function(df_irrig_sys, year)
{
  soil_df <- get_predominant_soil()
  rf_frac <- get_runoff_fraction_muni(year)
  
  conditions <- c('aspersion', 'cannon', 'pivot')
  sandy_idx <- which(soil_df$predominant_soil=='sandy')
  #conditional --------------------------------------------------------------------
  #if the soil isn't sandy, then use default rf fraction; otherwise set runoff to 0
  for (j in conditions)
  {
    df_irrig_sys[-sandy_idx, j] <- mapply(function(x,y) x*y/100, 
                                          df_irrig_sys[-sandy_idx, j],
                                          rf_frac[-sandy_idx, ncol(rf_frac)])
    for (i in sandy_idx)
    {
      df_irrig_sys[i, j] <- 0
    }
  }
  return(df_irrig_sys)
}

#if soil is silt/clay then rff is 10 and 20% ;; if soil is sandy then rf is arbitrarily set to 50%
compute_runoff_furrow <- function(df_irrig_sys, year)
{
  #condition ------------------------------------------------------
  ifelse(year==2009, rf <- 0.10, rf <- 0.20)
  soil_df <- get_predominant_soil()

  df_irrig_sys[, 'furrow'] <- mapply(function(x,y) x*rf*y,
                                     df_irrig_sys[, 'furrow'],
                                     soil_df[, 'reduction_factor'])
  return(df_irrig_sys)
}

#computes runoff N losses from irrigation systems
## YOU HAVE TO COMPUTE RUNOFF LOSSES USING RUNOFF FRAC
compute_runoff_irrig <- function(year, write)
{
  N_irrig <- aggregate_N_irrigation_source(FALSE, FALSE, year) #gross irrig
  irrig_runoff <- compute_runoff_furrow(N_irrig, year)
  irrig_runoff <- compute_runoff_sandy(irrig_runoff, year)
  irrig_runoff <- runoff_drip_other_grav(irrig_runoff)
  irrig_runoff$SUM <- rowSums(irrig_runoff[, c(4, ncol(irrig_runoff)-2)])
  irrig_runoff$nha <- round(irrig_runoff$SUM/load_uaa(year), 4)
  
  if (write==TRUE)
  {
    print(paste0('Writing irrigation N runoff data for ', year))
    write_irrig_output(paste0('Irrigation_runoff/', year), irrig_runoff, paste0('irrigatioN_runoff', year))
    print('Finished!')
  }
                                              
  return(irrig_runoff)
}

#adds the irrigation N losses through runoff (kg N/ha) to runoff_muni_source 
#this selects only the last column which contains irrigation runoff in kg N/ha 
#to be then cbind'd in the runoff_application
#d <- add_runoff_irrig_dataset(2009)
add_runoff_irrig_dataset <- function(year)
{
  irrig_rf <- compute_runoff_irrig(year, FALSE)
  irrig_rf <- irrig_rf[, ncol(irrig_rf)]
  irrig_rf <- as.data.frame(irrig_rf)
  colnames(irrig_rf)[1] <- 'irrigation'

  return(irrig_rf)
}
