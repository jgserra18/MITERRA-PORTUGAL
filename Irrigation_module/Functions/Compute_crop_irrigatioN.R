source('./Irrigation_module/Functions/Compute_1999_areas.R')
source('./Irrigation_module/Functions/no3_functions.R')
source('./Irrigation_module/Functions/compute_irrigatioN.R')

#get water volume per crop for each year
#get water source and NO3
#get irrigated areas per crop (irrespective of irrig system)
#calculate irrig N

#aggregate crops as follows: winter cereals (non-irrigated land), fruit trees (fresh, dry, citrus, olive, vine), grassland
#irrigated crops

compute_tot_irrig_areas09 <- function()
{
  #computes total crop area (sum of irrig system areas) and compiles these into one dataset
  
  main_crops <- get_maincrops_names(2009)
  df <- create_main_csv() #create main df to populate crop water volume usage per irrigation system specified
  create_irrig_output_dir('irrigated_crop_areas', TRUE)
  
  for (a in main_crops)
  {
    crops <- get_crop_names(2009, a)
    print(paste0('Working on', a))
    for (b in crops)
    {
      print(b)
      crop_area <- get_irrig_areas(2009, a, b)
      crop_area$sum <- rowSums(crop_area[, seq(4, ncol(crop_area))])
      df[, b] <- crop_area$sum
    }
  }
  df$irrig_areas09 <- rowSums(df[, seq(4, ncol(df))])
  write_irrig_output('irrigated_crop_areas', df, 'tot_irrig_crop09', 2009)
  return(df)
}



compute_prop_crop09 <- function()
{
  #computes area proportion for each area
  
  irrig_dataset09 <- get_output_file('irrigated_crop_areas', 2009, 'tot_irrig_crop')
  sum <- ncol(irrig_dataset09)
  crop_cols <- seq(4, sum)
  
  irrig_dataset09[, crop_cols] <- sapply(crop_cols, function(x) irrig_dataset09[, x] <- irrig_dataset09[, x]/irrig_dataset09[, sum])
  return(irrig_dataset09)  
}

extrapolate_1999_crop_areas <- function()
{
  #calculate 1999 crop areas at the municipality level based on 2009 proportions
  
  irrig_tot99 <- get_irrig_1999()
  prop_crop09 <- compute_prop_crop09()
  
  for (i in 4:(ncol(prop_crop09)-1))
  {
    prop_crop09[, i] <- round(prop_crop09[, i]*irrig_tot99[, 4], 3)       
  }
  return(prop_crop09)
}

func_compute_area_prop <- function(df)
{
  df$sum <- rowSums(df[, seq(4, ncol(df))])
  nc <- ncol(df)
  
  for (i in 4:nc)
  {
    df[, i] <- df[, i]/df[, nc]
  }
  
  return(df)
}


create_irrig_crop_dir99 <- function(main_crop, return_path, name)
{
  irrig_output_path <- select_module_output('Irrigation_module')
  dir_path <- file.path(irrig_output_path, 'irrigated_crop_areas')
  ifelse(missing(name)==TRUE, name <- '1999', name <- name)
  yr_path <- file.path(dir_path, name)
  crop <- file.path(yr_path, main_crop)
  dir.create(crop)
  #return pathfile
  if(return_path==TRUE)
  {
    return(crop)
  }
}


populate_output_irrig_areas99 <- function()
{
  #compute irrigation areas for crop and irrigation system based on 2009 data
  
  irrig_a99 <- extrapolate_1999_crop_areas()
  colnames(irrig_a99)[which(colnames(irrig_a99)=='olive.grove')] <- 'olive grove'
  main_crops <- get_maincrops_names(2009)
  
  for (a in main_crops)
  {
    crops <- get_crop_names(2009, a)
    path <- create_irrig_crop_dir99(a, T)
    
    for (b in crops)
    {
      crop_area <- get_irrig_areas(2009, a, b)
      crop_area_prop <- func_compute_area_prop(crop_area)
      crop_df <- as.data.frame(irrig_a99[, b])
      #calculate proportion of irrig systems for 1999
      for (i in 4:ncol(crop_area_prop))
      {
        crop_area_prop[, i] <- round(crop_area_prop[,i]*crop_df, 0)
        crop_area_prop <- data_cleaning(crop_area_prop)
        crop_area_prop <- switch_drip_to_sprinkler(crop_area_prop) #prof rosario assumption
      }
  
      write.csv(crop_area_prop, file.path(path, paste0(b, '.csv')))
    }
  }
}


correct_output_irrig_areas99 <- function()
{
  #correct_output_irrig_areas99()
  #correct irrigated areas 1999 based on total 1999 areas per crop
  #note: if anything wrong arises, it is because get_irrig_areas are directed from uncorrected99 subfolder
  
  main_crops <- get_maincrops_names(2009)
  
  for (a in main_crops)
  {
    crops <- get_crop_names(2009, a)
    path <- create_irrig_crop_dir99(a, T, 'corrected99')
    
    for (b in crops)
    {
      tot_crop_a <- get_raw_crop_areas(1999, a, b)
      irrig_crop_a <-get_irrig_areas(1999, a, b, '1999')
      
      irrig_crop_a <- cbind(irrig_crop_a, tot_crop_a[, ncol(tot_crop_a)])
      colnames(irrig_crop_a)[ncol(irrig_crop_a)] <- paste0('tot_', b)
      col_name <- paste0('tot_', b)
      irrig_crop_a$diff <- irrig_crop_a[, col_name]-irrig_crop_a$sum
      
      corr_id <- which(irrig_crop_a$diff<0)
      
      for (i in 4:(ncol(irrig_crop_a)-2))
      {
        irrig_crop_a[corr_id, i] <- round(irrig_crop_a[corr_id, col_name]*irrig_crop_a[corr_id, i]/irrig_crop_a[corr_id, 'sum'], 0)
      }
      irrig_crop_a$sum <- rowSums(irrig_crop_a[, seq(4,9)])
      final_df <- irrig_crop_a[, seq(1,9)]
      write.csv(final_df, file.path(path, paste0(b, '.csv')))
    }

  }
}


write_crop_irrigation <- function(year, path, file, filename)
{
  # simple function to write the output of crop irrigation N
  # can be applied either to crop class totals or individual crop
  
  filename <- paste0(filename, '.csv')
  full_path <- file.path(path, filename)
  
  data.table::fwrite(x = file, file = full_path)
}


create_dir_ind_crops <- function(year, module, subfolder, subfolder_name)
{
  #creates subfolders for each main crop when calculating individual crop N
  
  irrig_output <- select_module_output('Irrigation')
  module_path <- list.files(irrig_output, pattern = module, full.names = TRUE)
  year_folder <- file.path(module_path, year)
  select_subfolder <- list.files(year_folder, pattern = subfolder, full.names = TRUE)
  
  main.crop_subfolder <- file.path(select_subfolder, subfolder_name)
  dir.create(path = main.crop_subfolder)
  return(main.crop_subfolder)
}


general_crop_irrigatioN_func <- function(year, individual_crop)
{
  options(warn=-1)
  # computes irrigation N for each crop as the sum of each irrig system
  # this function is useful to allocate irrigation N for each LU classes
  # note: this is the ideal function to compute irrigationN for each source
  # INDIIVIDUAL_CROP can be either TRUE or FALSE
  compute_corrected_water_sources(year, T) #for water source
  main_crops <- get_maincrops_names(2009)
  df <- create_main_csv() #create main df to populate crop water volume usage per irrigation system specified
  water_source <- c('spring', 'superficial', 'well')
  
  #create directory for total and individual crops
  ifelse(individual_crop==TRUE, folder <- 'individual', folder <- 'Total')
  path <- main_crop_dir_create('Irrigation_crop_N', folder, year)

  for (a in main_crops) {
    if (individual_crop==TRUE){path <- create_dir_ind_crops(year, 'crop_N', folder, a)} #create subfolder for maincrop
    crops <- get_crop_names(2009, a)
    main_crop_df <- df
    
    for (b in crops) {
      #get crop water volume
      crop_volume <- get_crop_water_volume(a, b, year)
      crop_volume$sum <- rowSums(crop_volume[, seq(4, ncol(crop_volume))]) #compute total volume
      sel_cols <- seq(4, ncol(crop_volume)) # define range of cols in calculations
      #create empty dataframe
      crop_df <- crop_volume
      crop_df[, sel_cols] <- sapply(crop_df[, sel_cols], function(x) x <- 0)
      calc_df <- df
      
      for (c in water_source)
      {
        #get water source and NO3
        w_source <- specify_water_source(c)
        no3_source <- conversion_no3(year, c)
        #N in individual crops is calculated by summing the N from each water source
        #main crops is sim
        ifelse(individual_crop==FALSE,
               calc_df[, c] <- crop_volume$sum*w_source*no3_source,
               crop_df[, sel_cols] <- crop_df[, sel_cols] + sapply(crop_volume[, sel_cols], function(x) round(x*w_source*no3_source, 0)))
      }
      #aggregate the datasets to export
      #no need to aggregate the inidividual crops as the dataset already contains the sum of N for each system
      if (individual_crop==FALSE){
        calc_df$sum <- rowSums(calc_df[, seq(4, ncol(calc_df))])
        main_crop_df[,b] <- calc_df$sum
      }
      else {write_crop_irrigation(year, path = path, file = crop_df, filename = paste0('irrigN_',b)) }
    }
    if (individual_crop==FALSE){
      ifelse(ncol(main_crop_df)>4, main_crop_df$sum <- rowSums(main_crop_df[, seq(4, ncol(main_crop_df))]), colnames(main_crop_df)[4] <- 'sum')
    write_crop_irrigation(year, path = path, file = main_crop_df, filename = paste0('irrigN_', a))
    }
  }
}

loop_through_crop_irrigatioN <- function()
{
  ## loops through both individual and total crop irrigatioN for both years
  
  year <- c(1999, 2009)
  cond <- c(TRUE, FALSE)
  mapply(function(x, y){general_crop_irrigatioN_func(x,y)}, x <- year, y <- cond)
}


