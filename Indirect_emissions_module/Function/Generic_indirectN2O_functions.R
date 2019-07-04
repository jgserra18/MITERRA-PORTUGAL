source('./Main_functions.R')
source('./Runoff_module/Function/Compute_runoff_application.R')

#unit EF: kg N-N2O kg N applied-1
#EF5 represents the sum of Gw, rivers and estuaries
default_ipcc_ef <- function()
{
  ef_ipcc <- data.frame(source=c('atmospheric_deposition', 'groundwater', 'rivers', 'estuaries', 'ef5'), 
                         ef_ipcc=c(0.01, 0.015, 0.0075, 0.0025, 0.025))
  
  return(ef_ipcc)
}

#tian et al (2018) 
#https://www.sciencedirect.com/science/article/pii/S0269749118317998?via%3Dihub 
reviewed_ef <- function()
{
  ef_review <- data.frame(source=c('atmospheric_deposition', 'groundwater', 'rivers', 'estuaries', 'ef5'), 
                          ef_review=c(0.01, 0.006, 0.0026, 0.0026, 0.006+0.0026*2))
  return(ef_review)
}

get_efs <- function()
{
  ef_ipcc <- default_ipcc_ef()
  ef_review <- reviewed_ef()
  
  ef_df <- merge(ef_ipcc, ef_review, 'source', sort = FALSE)
  
  return(ef_df)
}

#gets a dataset with every runoff source (in kg N/ha), converted to kg N
get_runoff_N <- function(file_pattern, year)
{
  ifelse(missing(file_pattern)==TRUE, file_pattern <- 'source_muni', file_pattern <- file_pattern)
  file <- get_module_output('Runoff', file_pattern, year)
  file[, 4:ncol(file)] <- sapply(file[, 4:ncol(file)], function(x) 
                                                          round(x*load_uaa(year), 3))
  return(file)
}

#gets a dataset with every leaching per source (in kg N/ha)
get_leaching_N_source <- function(file_pattern, year)
{
  ifelse(missing(file_pattern)==TRUE, file_pattern <- 'source_leaching', file_pattern <- file_pattern)
  file <- get_module_output('Leaching', file_pattern, year)
  return(file)
}

#must sum all that
leaching_N_source_muni <- function(year)
{
  file <- get_leaching_N_source(year=year) #caa
  df <- create_main_csv()
  
  select_cols <- file[, c('Muni_ID', 'leaching_large', 'leaching_small', 'leaching_gw')]
  upscale_muni <- select_cols %>% group_by(Muni_ID) %>% summarise_all(sum)
  df <- merge(df, upscale_muni, 'Muni_ID', sort=FALSE)
  
  return(df)
}

#gives theoutput path of the indirect n2o emissions module
ind_n2o_module_path <- function()
{
  path <- select_module_output('Indirect')
}

write_indirect_n2o <- function(df, name, pattern, year)
{
  write_output('Indirect', df, paste0('/', year, '/', name), pattern)
}

#check this, particularly EF of runoff
#simplified function, may have to be adapted in the future
indirect_n2o_conditions <- function()
{
  condition_df <- data.frame(input=c('large', 'small', 'gw', 'runoff'), 
                             source=c('rivers', 'rivers', 'groundwater', 'groundwater'))
  efs <- get_efs()
  merged_df <- merge(condition_df, efs, 'source')
  
  return(merged_df)
}

#this funciton allows the specification of runoff or leaching N losses to each source (large, small, gw or runoff)
#it subsets the ef_df according to the tiered methodology, e.g. "ipcc" or "not_ipcc"
#it then computes indirect N2O emissions per source
#d <- compute_ind_n2o('leaching', 2009, 'not_ipcc')
compute_ind_n2o <- function(source, year, tier)
{
  #load data EFs
  ef <- indirect_n2o_conditions()
  #subset df according to source
  ifelse(source=='runoff', df <- get_runoff_N(year=year), df <- leaching_N_source_muni(year))
  #load colnames
  cols <- colnames(df)
  #subsets the efs according to the specified tier
  ifelse(tier=='ipcc',  ef <- ef[, -4], ef <- ef[, -3]) #
  #condition to test if colnames == to EF_source
  for (i in 1:nrow(ef))
  {
    condition <- grepl(ef[i, 2], cols)
    if (!all(condition==FALSE)) #if there is at least one TRUE
    {
      id <- which(condition==TRUE)
      efs <- ef[i, 3]
      df[, id] <- round(df[, id]*efs, 3)
    }
  }
    if (source=='runoff')
    {
      efs <- ef[2, 3]
      df[, 4:ncol(df)] <- sapply(df[, 4:ncol(df)], function(x) round(x*efs, 3))
      df <- data_cleaning(df)
    }
  df$SUM <- rowSums(df[, 4:ncol(df)])
  
  return(df)
}

#compiles and writes every runoff/leaching source to the output
compile_ind_n2o_all <- function(all)
{
  year <- c(1999, 2009)
  source <- c('runoff', 'leaching')
  tier <- c('ipcc', 'not_ipcc')
  
  for (a in year)
  {
    path <- dir.create(file.path(ind_n2o_module_path(), a), showWarnings = F)
    
    for (b in source)
    {
      for (c in tier)
      {
        compute_ind_n2o <- compute_ind_n2o(b, a, c)
        write_indirect_n2o(compute_ind_n2o, name = paste0(b, '_', c), year = a)
      }
    }
  }
}

aggregate_ind_n2o <- function(year, tier, write)
{
  source <- c('runoff', 'leaching')
  main_df <- create_main_csv()
  
  for (a in source)
  {
    df <- compute_ind_n2o(a, year, tier)
    nc <- ncol(df)-1 #minus SUM
    main_df <- cbind(main_df, df[, 4:nc])
  }
  main_df$SUM <- rowSums(main_df[, 4:ncol(main_df)])
  main_df$nha <- main_df$SUM/load_uaa(year)
  
  if (write==TRUE)
  {
    write_indirect_n2o(main_df, name = paste0('aggregated_dataset_', tier), year = year)
    
  }
  
  return(main_df)
}

#loops the function aggregate_ind_n2o and aggregates and subsequently writes this new aggregated dataset
populate_aggregated_ind_n2o <- function()
{
  year <- c(1999, 2009)
  tier <- c('ipcc', 'not_ipcc')
  
  for (a in tier)
  {
    for (b in year)
    {
      df <- aggregate_ind_n2o(b, a, T)
    }
  }
}

