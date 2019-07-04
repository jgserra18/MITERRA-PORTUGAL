source('./Main_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')
source('./Leaching_module/Function/Compute_leaching.R')
#source('./NLoading_module/Function/Compute_gw_loadings.R')
#source('./Leaching_module/Function/compute_correct_surface_water_areas.R')

# 1 - calculate leaching to large and small surfaces, and GW at the CAA scale
# 2- main_df - lsrge/surface spreadsheet; merge with corrected areas; merge with leaching_nha per CAA_ID, calculate leaching (in N) and
#sum to GW_ID;; do this for large and surface waters
#3 - Leaching to GW = Leaching_to_GW_CAA (already calculated)-Leaching_large-Leaching_small

#computes the area of each large and small surface area intersected to each GW
#these are exported as csv
compute_area_export <- function()
{
  file_names <- c('intersect_large09', 'intersect_large99', 'intersect_small09', 'intersect_small09')
  
  for (i in file_names)
  {
    df <- load_surface_shp(i)
    df$area <- rgeos::gArea(df, byid=TRUE)/10000
    df$area <- round(df$area, 3)
    
    df <- as.data.frame(df)
    export_surface_csv(df, i)
    rm(df)
  }
}

#load the csv files of large or small surfaces
#output is a list with csv of large and small surface bodies
load_surface_csv <- function(year)
{
  folder_path <- select_maindata_pattern('SurfaceWater')
  store_files <- store_folder_files(folder_path)
  files_yr <- disaggregate_year(store_files, year)
  
  file_large <- read.csv(identify_pattern(files_yr, 'large'))
  colnames(file_large)[ncol(file_large)] <- 'area_large'
  
  file_small <- read.csv(identify_pattern(files_yr, 'small'))
  colnames(file_small)[ncol(file_small)] <- 'area_small'
  
  return(list(file_large, file_small))
}

#selects CAA_ID and area
select_cols_surface_csv <- function(year, list_id)
{
  df <- load_surface_csv(year)[[list_id]]
  df <- df[, c(1, ncol(df))]
  
  return(df)
}

#loads the CAA areas to further correct
#with N-leaching_nha
#tier is either 'tier2_ssnb' or 'tier2_irrig') by default it is irrig
load_caa_correct_areas <- function(year, tier_ssnb)
{
  df <- load_lf_fraction(year)
  df <- df[, c(1,2,5,6,7)]
  
  ifelse(missing(tier_ssnb)==T, tier_ssnb <- 'tier2_irrig', tier_ssnb <- tier_ssnb)
  
  leaching_nha <- aggregate_to_maindf(year, TRUE, tier_ssnb)
  df <- merge(df, leaching_nha[, c('CAA_ID', 'leaching_nha')], 'CAA_ID')
  
  return(df)
}

#sums surface areas per CAA_ID for further correction
#note: this is simply used for calculations at the CAA scale
sumif_surface_areas_for_caa <- function(year, list_id)
{
  df_surface <- select_cols_surface_csv(year, list_id)
  colnames(df_surface)[1] <- 'CAA_ID'
  sum_df <- sumif(df_surface, 'CAA_ID', colnames(df_surface)[ncol(df_surface)])
  
  main_df <- cbind(unique(df_surface$CAA_ID), sum_df)
  colnames(main_df)[1] <- 'CAA_ID'
  
  return(main_df)
}

#computes the corrected areas 
compute_correct_surface_areas <- function(year, list_id, tier_ssnb)
{
  main_df <- load_caa_correct_areas(year, tier_ssnb)
  df_surface <- sumif_surface_areas_for_caa(year, list_id)
  
  main_df <- merge(main_df, df_surface, 'CAA_ID', all = TRUE)
   
  main_df$name <- floor(round(
                              main_df[, ncol(main_df)]*main_df[, 3]/main_df[, 4],
                              2)) #rounding down to 0
  
  ifelse(list_id==1, colnames(main_df)[ncol(main_df)] <- 'correct_large',
                    colnames(main_df)[ncol(main_df)] <- 'correct_small')
  
  return(main_df)
}

#computes leaching (kg N) for each type of surface water per CAA_ID
compute_leaching_surface <- function(year, list_id, tier_ssnb)
{
  main_df <- compute_correct_surface_areas(year, list_id, tier_ssnb)
  main_df$lch <- main_df[,'leaching_nha']*main_df[, ncol(main_df)]
  
  ifelse(list_id==1, colnames(main_df)[ncol(main_df)] <- 'leaching_large',
         colnames(main_df)[ncol(main_df)] <- 'leaching_small')
  
  return(main_df)
}

#aggregates the areas of large and small surface water bodies per CAA_ID
#d <- aggregate_surface_areas(2009, 'tier2_irrig')
aggregate_surface_areas <- function(year, tier_ssnb)
{
  list_id <- c(1,2)
  main_df <- load_caa_correct_areas(year, tier_ssnb)
  
  for (i in list_id)
  {
    surface_df <- compute_correct_surface_areas(year, i, tier_ssnb)
    main_df <- cbind(main_df, surface_df[, ncol(surface_df)])
    colnames(main_df)[ncol(main_df)] <- colnames(surface_df)[ncol(surface_df)]
    
    rm(surface_df)
  }
  return(main_df)
}

#aggregates to the previous main_df, total leaching, small leaching and large leaching
aggregate_leaching_surface <- function(year, tier_ssnb)
{
  list_id <- c(1,2)
  
  main_df <- aggregate_surface_areas(year)
  main_df$total_leaching <- round(main_df[, 'poly_corr']*main_df[, 'leaching_nha'], 3)
  
  for (i in list_id)
  {
    df_surface <- compute_leaching_surface(year, i, tier_ssnb)
    leaching_col <- df_surface[, ncol(df_surface)]
    
    main_df <- cbind(main_df, leaching_col)
    colnames(main_df)[ncol(main_df)] <- colnames(df_surface)[ncol(df_surface)]
    
    rm(df_surface)
  }
  return(main_df)
}

#algorithm to correct data inconsistencies concerning surface water intersection
area_leaching_correction_algorithm <- function(year, tier_ssnb)
{
  df <- aggregate_leaching_surface(year, tier_ssnb)
  df <- data_cleaning(df)
  nc <- ncol(df)
  
  for (i in 1:nrow(df))
  {
    if ((df[i, nc-2]-df[i, nc-1]-df[i, nc])<0)
    {
      df[i, nc] <- 0
      df[i, nc-3] <- 0
    }
    
    if (df[i, nc-2]<df[i, nc-1])
    {
      df[i, nc-1] <- 0
      df[i, nc-4] <- 0
    }
  }
  return(df)
}

#computes GW leaching 
compute_leaching_gw <- function(year, tier_ssnb)
{
  df <- area_leaching_correction_algorithm(year, tier_ssnb)
  df$leaching_gw <- df$total_leaching-df$leaching_large-df$leaching_small
  
  return(df)
}

compute_leaching_proportion <- function(year, tier_ssnb)
{
  main_df <- compute_leaching_gw(year, tier_ssnb)
  df <- main_df[, c(1,2)]
  
  df$Lf_large <- round(main_df$leaching_large/main_df$total_leaching, 2)
  df$Lf_small <- round(main_df$leaching_small/main_df$total_leaching, 2)
  df$Lf_gw <- round(main_df$leaching_gw/main_df$total_leaching, 2)
  
  df <- data_cleaning(df)
  
  return(df)
}


compute_leaching_frac <- function(year)
{
  main_df <- compute_leaching_proportion(year)
  df <- load_lf_fraction(year)
  df <- df[, ncol(df)]
  
  for (i in 3:ncol(main_df))
  {
    main_df[, i] <- main_df[, i]*df
  }
  return(main_df)
}

################## LEACHING TO GW PER GW_ID ################3
correct_gw_surface_areas <- function(year, tier_ssnb)
{
  db <- compute_leaching_gw(year, tier_ssnb)
  list_id <- c(1, 2)
  
  for (i in list_id)
  {
    df <- load_surface_csv(2009)[[i]]
    
    if (i==1)
    {
      df_merge <- merge(df, db[, c('CAA_ID', 'poly_corr', 'leaching_nha', 'correct_large')], 'CAA_ID')
      sum_if <- sumif(df, 'CAA_ID', 'area_large')
      sum_if_db <- cbind(unique(df$CAA_ID), sum_if)
      colnames(sum_if_db)[1] <- 'CAA_ID'
      colnames(sum_if_db)[2] <- 'summed_areas'
      
      merge_large <- merge(df, sum_if_db, 'CAA_ID')
      merge_large <- merge(merge_large, db[, c('CAA_ID', 'poly_corr', 'leaching_nha', 'correct_large')], 'CAA_ID')
      merge_large$gw_large_areas <- round(merge_large$area_large*merge_large$correct_large/merge_large$summed_areas, 1)
    }
    
    else if (i ==2)
    {
      df_merge <- merge(df, db[, c('CAA_ID', 'poly_corr', 'leaching_nha', 'correct_small')], 'CAA_ID')
      sum_if <- sumif(df, 'CAA_ID', 'area_small')
      sum_if_db <- cbind(unique(df$CAA_ID), sum_if)
      colnames(sum_if_db)[1] <- 'CAA_ID'
      colnames(sum_if_db)[2] <- 'summed_areas'
      
      merge_small <- merge(df, sum_if_db, 'CAA_ID')
      merge_small <- merge(merge_small, db[, c('CAA_ID', 'poly_corr', 'leaching_nha', 'correct_small')], 'CAA_ID')
      merge_small$gw_small_areas <- round(merge_small$area_small*merge_small$correct_small/merge_small$summed_areas, 1)
    }
  }

  return(list(merge_large, merge_small))
}

#computes the leaching of each specific surface water
#list_id 1 large, 2 small
compute_gw_leaching_sources <- function(year, list_id, tier_ssnb)
{
   df <- correct_gw_surface_areas(year, tier_ssnb)[[list_id]]
   ifelse(list_id==1, df$leaching_large <- 0, df$leaching_small <- 0)   
   
   df[, ncol(df)] <- df[, 'leaching_nha']*df[, ncol(df)-1]
   df <- data_cleaning(df)
  return(df)
}

sumif_leaching_water_gw <- function(year, list_id, tier_ssnb)
{
  df <- compute_gw_leaching_sources(year, list_id, tier_ssnb)
  
  gw_df <- data.frame(GW_ID=unique(df$GW_ID))
  gw_df <- gw_df[order(gw_df$GW_ID),]
  
  sum_df <- round(sumif(df, 'GW_ID', colnames(df)[ncol(df)]), 2)
  gw_df <- cbind(gw_df, sum_df)
  gw_df <- as.data.frame(gw_df)
  colnames(gw_df)[1] <- 'aquifer_ID'
  
  return(gw_df)
}


arrange_gw_leaching_dataset <- function(year)
{
  df <- gw_complete_dataset(year)
  df <- df[, c(1, ncol(df), 2)]
  colnames(df)[3] <- 'total_leaching'
  df[, 3] <- df[, 3]/1000000 #kg N
  
  return(df)
}

#Loops around leaching_gw for each source and corrects it
#Only one case, in 1999
#AUXILIARY FUNCTION
correct_gw_leaching <- function(df)
{
  for (i in 1:nrow(df))
  {
    if (df[i, 'leaching_gw']<0)
    {
      df[i, 'leaching_large'] <- df[i, 'total_leaching']
      df[i, 'leaching_small'] <- 0
      df[i, 'leaching_gw'] <- 0
    }
  }
  return(df)
}

populate_gw_leaching_dataset_surface <- function()
{
  year <- c(1999, 2009)
  list_id <- c(1, 2)
  
  for (i in year)
  {
    gw_db <- arrange_gw_leaching_dataset(i)
      
    for (j in list_id)
    {
      df <- sumif_leaching_water_gw(i, j)
      gw_db <- merge(gw_db, df, 'aquifer_ID', all=TRUE)
      gw_db <- data_cleaning(gw_db)
      rm(df)
    }
    gw_db[, 'leaching_gw'] <- gw_db[, 3]-gw_db[, 4]-gw_db[, 5] 
    
    gw_db <- correct_gw_leaching(gw_db)
    
    write_leaching_output(gw_db, 'gw_leaching', i, 'tieeeer')
    rm(gw_db)
  }
}


#loads the leaching per aquifer_ID
load_leaching_gw <- function(year, only_gw)
{
  folder_path <- select_module_output('Leaching')
  folder_files <- list.files(folder_path, pattern = 'csv')
  select_files <- folder_files[grepl('gw', folder_files)]
  
  select_year <- disaggregate_year(select_files, year)
  file_read <- read.csv(paste0(folder_path, select_year))
  
  ifelse(only_gw==TRUE,
         file_read <- file_read[, c(1,ncol(file_read))],
         file_read)
  
  return(file_read)
}

