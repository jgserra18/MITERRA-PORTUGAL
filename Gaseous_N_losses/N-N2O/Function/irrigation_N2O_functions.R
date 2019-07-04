source('./Irrigation_module/Functions/Global_irrigation_functions.R')
source('./Irrigation_module/Functions/compute_irrigatioN.R')
source('./Gaseous_functions.R')

#general EFs for each main irrigation system used in Cayuela et al 2017
get_irrig_ef <- function()
{
  ef_df <- data.frame(irrig_systems=c('rainfed', 'gravity', 'sprinkler', 'drip'), EF=c(0.27, 0.47, 0.91, 0.51))
  ef_df$EF <- ef_df$EF/100
  
  return(ef_df)
}

#input data is colnames of a dataset with irrigation systems' names
#it then returns the respective EF
irrig_ef_conditions <- function(colname_df)
{
  ef_df <- get_irrig_ef()
  
  #build a dataset with the EFs of each irrigation system except rainfed
  exceptions <- c('other_grav', 'aspersion', 'cannon', 'pivot', 'microaspersion', 'furrow', 'drip')
  main_irrig <- c('gravity', 'sprinkler', 'sprinkler', 'sprinkler', 'drip', 'gravity', 'drip')
  df <- data.frame(exception_irrig_sys=exceptions, irrig_systems=main_irrig)
  df <- merge(df, ef_df, 'irrig_systems')
  
  #identify colname_df (ie irrigation system name) and return EF
  if (missing(colname_df)==FALSE)
  {
    id <- which(df[, 2]==colname_df)
    ef <- df[id, 3]
    return(ef)
  }
  else 
  {
    return(df)
  }
}

#gets the total irrigation N (sum of each water source)
#used to calculate direct N2O emissions
get_irrigatioN_sys <- function(efficiency, year)
{
  #irrigatioN_df <- get_output_file('Irrigation_N', year, 'total') this was one disregarded as does not  easily alllow to account for irrigation efficiencies
  irrigatioN_df <- aggregate_N_irrigation_source(efficiency, FALSE, year)
  return(irrigatioN_df)
}
irrigatioN_df <- aggregate_N_irrigation_source(F, FALSE, 2009)
View(irrigatioN_df)
#computes direct N2O emissions from irrigation N
#in principle efficiency should be FALSE
#EXPORT TO N2O ACTIVITY DATA
compute_n2o_irrig_sys <- function(efficiency, year, export_activity, tier)
{
  irrigatioN_df <- aggregate_N_irrigation_source(efficiency, FALSE, year)
  uaa <- load_uaa(year)
  cols <- colnames(irrigatioN_df)
  stop_id <- ncol(irrigatioN_df)-2 #loop before reaching SUM and nha
  
  ######## COMPUTE N-N2O FROM EACH IRRIG SYS ACCORDING TO SPECIFIED TIER #########
  for (i in 4:stop_id)
  {
    ifelse(tier==2,
      irrigatioN_df[, i] <- irrigatioN_df[, i]*irrig_ef_conditions(cols[i]),
      irrigatioN_df[, i] <- irrigatioN_df[, i]*0.01)
  }
  
  irrigatioN_df$SUM <- rowSums(irrigatioN_df[, 4:stop_id])
  irrigatioN_df$nha <- irrigatioN_df$SUM/uaa
  colnames(irrigatioN_df)[ncol(irrigatioN_df)] <- 'irrig_nha'
  
  ######## WRITE #########
  if (export_activity==TRUE)
  {
    
    export_to_activity('N-N2O', 'Application', irrigatioN_df, paste0('tier', tier, '_irrigatioN', year_prefix(year)))
  }
  
  return(irrigatioN_df)
}


