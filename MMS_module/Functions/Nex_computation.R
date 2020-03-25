source('./Main_functions.R')


## ---------------- ANIMAL POPULATIONS GETTERS  ---------------- ##
## --------------------------------------------------------------##

set_livestock_act_data <- function() {
  # sets the main activity data for all subsequent livestock calculations
  
  main_folder <- load_raw_data(subfolder = 'Livestock')
  return(main_folder)
}

select_livestock_subfolder <- function(subfolder, year) {
  # sselects any livestock subfolder and yar
  # e.g. Animal_population, 2009
  
  main_folder <- set_livestock_act_data()
  select_folder <- list.files(path = main_folder, pattern = subfolder, full.names = T)
  # if the user does not need the year (eg EMEP)
  if (missing(year)==FALSE) {
    select_year <- list.files(path = select_folder, pattern = as.character(year), full.names = T)
    return(select_year)
  } else {
    return(select_folder)
  }
}

get_animal_classes <- function() {
  
  animal_class <- c('Bovine', 'Equides', 'Goats', 'Poultry', 'Rabbits', 'Sheep', 'Swine')
  return(animal_class)
}

get_animal_pop <- function(year, animal_class) {
  # gets the dataset regarding animal population for each animal subclasses within the specified animal_class
  # e.g. get_animal_pop(2009, 'Bovine')?
  # Unit: heads 
  
  select_folder <- select_livestock_subfolder(subfolder = 'Animal_population', year = year)
  select_animal_class <- list.files(path = select_folder, pattern = animal_class, full.names=T)
  read_file <- read.csv(select_animal_class, header=T, check.names = F)
  return(read_file)
}
  
get_milk_prod <- function() {
  # gets milk production (in tonnes) per NUTS2 region for the period 2003-2018
  # Unit: tonnes milk produced
  
  milk_prod_folder <- select_livestock_subfolder(subfolder = 'Milk_production')
  milk_prod <- read.csv(
    list.files(path = milk_prod_folder, pattern = 'Dairy_milk', full.names=TRUE))
  
  colnames(milk_prod) <- gsub(pattern = 'X', replacement = '', x = colnames(milk_prod))
  return(milk_prod)
}



## ----------------------- EMEP GETTERS  ----------------------- ##
## --------------------------------------------------------------##

select_EMEP_param_folders <- function(subfolder) {
  
  emep_folder <- select_livestock_subfolder('EMEP')
  select_folder <- list.files(path = emep_folder, pattern = subfolder, full.names=T)
  
  return(select_folder)
}

get_Nex_coefs <- function() {
  # gets the Nex coefficients for all animals apart from dairy cows
  # Unit : kg N head-1
  
  animal_Nex <- select_EMEP_param_folders(subfolder = 'N_excretion')
  animal_Nex <- read.csv(animal_Nex)
  return(animal_Nex)
}

get_TAN_animal_class <- function() {
  # gets the TAN coefficients for the main animal classes
  # Unit : % of N flow
  
  animal_TAN <- select_EMEP_param_folders(subfolder = 'Animal_TAN')
  animal_TAN <- read.csv(animal_TAN)
  return(animal_TAN)
}


## ----------------------- MANURE FIELD APPLICATION RATES ----------------------- ##
## -------------------------------------------------------------------------------##

get_manure_application_rates <- function(year) {
  # get manure application rates for a given year
  # unit: % of manure N 
  
  manure_app_folder <- select_EMEP_param_folders(subfolder = 'Manure_application_rates')
  manure_app_rates <- list.files(path = manure_app_folder, pattern = year_prefix(year), full.names=T)
  r_file <- read.csv(manure_app_rates)
  return(r_file[, 4])
}




## ----------------------- MANURE STORED ----------------------- ##
## --------------------------------------------------------------##

get_manure_stored <- function(year) {
  # gets data related to the distribution of animals
  # housing, yards, grazing
  # unit: % of animal population
  
  animal_dist_folder <- select_EMEP_param_folders(subfolder = 'Manure_stored')
  select_animal_dist <- list.files(path = animal_dist_folder, pattern = year_prefix(year), full.names=T)
  r_file <- read.csv(select_animal_dist)
  return(r_file)
}

## Manure division
## Total manure = Xstore + Xfeed + direct spread
## Xfeed for biogas is assumed 0

# note: this may change in the future (the values, not the code)
get_manure_stored_subclass <- function(year, livestock_subclass, manure_type) {
  # subsets the proportion of manure stored for spreading or whatnot for a given livestock subclass and manure_type
  # currently this is all set to 1 because biogas data are not available
  # Unit: % of the total manure stored for spreading and whatnot
  
  manure_stored <- get_manure_stored(year)
  ifelse(manure_type=='solid',
         Xstore_col <- 'Xstore_solid',
         Xstore_col <- 'Xstore_slurry')
  subset_subclass <- manure_stored[which(manure_stored$Livestock_subclass==livestock_subclass), Xstore_col]
  return(subset_subclass)
}



## ----------------------- CROP DISTRIBUTION AHP ----------------------- ##
## ----------------------------------------------------------------------##

get_crop_AHP_weights <- function() {
  # gets the manure crop distribution weights
  # based on AHP
  # the values represent only the AHP's direct output here
  
  crop_ahp_folder <- select_EMEP_param_folders(subfolder = 'Manure_crop_distribution')
  r_crop.ahp <- read.csv(list.files(crop_ahp_folder, full.names = TRUE))
  return(r_crop.ahp)
}

select_crop_AHP_weights <- function(crop) {
  # calls crop AHP and selects the weight for the specified crop
  # division_weights (col 5) correspond to the weights scaled concerning ruminant and non-ruminant distribution
  
  ahp_df <- get_crop_AHP_weights()
  weight <- ahp_df[which(ahp_df[, 2]==crop), 5]
  return(weight)
  rm(list='ahp_df')
}


## ----------------------- ANIMAL DISTRIBUTION GETTERS ----------------------- ##
## ----------------------------------------------------------------------------##

get_animal_distribution <- function(year) {
  # gets data related to the distribution of animals
  # housing, yards, grazing
  # unit: % of animal population
  
  animal_dist_folder <- select_EMEP_param_folders(subfolder = 'Animal_distribution')
  select_animal_dist <- list.files(path = animal_dist_folder, pattern = year_prefix(year), full.names=T)
  r_file <- read.csv(select_animal_dist)
  return(r_file)
}

subset_set_NUTS2_ID <- function(spatial_disagg_df, nuts2_id, set_value, col_name) {
  # subsets the spatial disaggregtion df (which must be specified as param)
  # based on the specified nuts2 ID ;; the user can then set a whole param for this whole region
  # and changes the dataset accordingly
  
  subset_idx <- which(spatial_disagg_df$nuts2_ID==nuts2_id)
  spatial_disagg_df[subset_idx, col_name] <- set_value
  return(spatial_disagg_df)
}

get_dairy_distribution <- function() {
  # allocates dairy distribution based on assumptions
  # No grazing in the North, 67.5% housing, 32.5% yards
  # 25% grazing, 55% housing, 20% yards in the remainder
  
  df <- call_spatial_disaggregation()
  dist_names <- c('Grazing', 'Housing', 'Yards')
  for (i in dist_names) { 
    df[, i] <- 1 
  }
  
  # set grazing in the North to 0
  df <- subset_set_NUTS2_ID(spatial_disagg_df = df, nuts2_id = 11, set_value = 0, col_name = 'Grazing')
  # set grazing to 25% in the remaining regions
  df[which(df$Grazing!=0), 'Grazing'] <- 0.25
  
  #set housing to either 67.5% or 55%
  df[which(df$Grazing==0), 'Housing'] <- 0.675
  df[which(df$Grazing!=0), 'Housing'] <- 0.55
  
  #set yards to the remainder
  df$Yards <- 1-df$Grazing-df$Housing
  
  subset_df <- subset(df, select=c('Muni_ID', 'ID', 'Muni', 'Grazing', 'Housing', 'Yards'))
  return(subset_df)
}




## ----------------------- MMS GETTERS ----------------------- ##
## ------------------------------------------------------------##

get_animal_MMS <- function(year) {
  
  animal_mms_folder <- select_EMEP_param_folders(subfolder = 'MMS')
  select_animal_mms <- list.files(path = animal_mms_folder, pattern = year_prefix(year), full.names=T)
  r_file <- read.csv(select_animal_mms, header=T, check.names = F)
  return(r_file)
}

select_animal_MMS <- function(year, animal_subclass, MMS) {
  # MMS is either solid or liquid
  # returns the percentage of animal MMS in solid or liquid systems
  # output: % of housed animals
  
  mms_dataset <- get_animal_MMS(year)
  subclass_mms <- mms_dataset[which(mms_dataset[, 2]==animal_subclass), MMS]
  return(subclass_mms)
}



## ----------------------- GENERAL MMS WRITE/EXPORT MODULE ---------------------- ##
## -------------------------------------------------------------------------------##1

create_MMS_subfolder <- function(subfolder_name) {
  # creates a MMS output subfolder according to the specified name
  
  mms_output <- select_module_output('MMS')
  mms_subfolder <- file.path(mms_output, subfolder_name)
  dir.create(path = mms_subfolder, showWarnings = F)
  return(mms_subfolder)
}

create_MMS_subfolderX2 <- function(subfolder_name, subfolder_nameX2) {
  # creates an additional subfolder within a given MMS subfolder
  # e.g., Grazing\N_excretion or NH3
  
  mms_subfolder <- create_MMS_subfolder(subfolder_name)
  mms_add_subfolder <-  file.path(mms_subfolder, subfolder_nameX2)
  dir.create(path = mms_add_subfolder, showWarnings = F)
  return(mms_add_subfolder)
}

creates_annual_subfolder <- function(subfolder_name, year, subfolder_nameX2) {
  # creates a subfolder for a given year

  if (missing(subfolder_nameX2)==TRUE) {
    mms_subfolder <- create_MMS_subfolder(subfolder_name)
    subfolder_year <- file.path(mms_subfolder, year)
    dir.create(path = subfolder_year, showWarnings = F)
    return(subfolder_year)
  } 
  else {
    mms_add_subfolder <- create_MMS_subfolderX2(subfolder_name, subfolder_nameX2)
    add_subfolder_year <- file.path(mms_add_subfolder, year)
    dir.create(path = add_subfolder_year, showWarnings = F)
    return(add_subfolder_year)
  }
}

write_annual_data <- function(subfolder, file, filename, year, subfolder_nameX2) {
  
  ifelse(missing(subfolder_nameX2)==TRUE,
         export_path <- creates_annual_subfolder(subfolder, year),
         export_path <- creates_annual_subfolder(subfolder, year, subfolder_nameX2))
  name <- paste0(filename, '.csv')
  file_path <- file.path(export_path, name)
  write.csv(x = file, file = file_path, row.names = F)
}


## ----------------------- OUTPUT GETTERS ----------------------- ##
## ------------------------------------------------------------##

get_MMS_output_file <- function(subfolder, year, file_pattern) {
  # reads a MMS output file
  # however this function is not enough to read subfolders like N excretion in grazing, or NH3 losses in grazing
  # e.g., tottal N excretion
  
  subfolder_path <- creates_annual_subfolder(subfolder, year)
  select_file <- list.files(path = subfolder_path, pattern = file_pattern, full.names = TRUE)
  r_file <- read.csv(select_file, header=T, check.names = F)
  return(r_file)
}

get_MMS_subfolder_output_file <- function(subfolder, subfolderX2, year, file_pattern) {
  # reads sub-output files of MMS 
  # e.g., n excreted in grazing
  
  add_subfolder_year <- creates_annual_subfolder(subfolder, year, subfolderX2)
  select_file <- list.files(path = add_subfolder_year, pattern = file_pattern, full.names = TRUE)
  r_file <- read.csv(select_file, header=T, check.names = F)
  return(r_file)
}


## ----------------------- DAIRY COW NEX CALCULATION  ----------------------- ##
## ---------------------------------------------------------------------------##

compute_dairy_regional_heads <- function(year) {
  # computes the total number of dairy cows for a given in year at the NUTS2 level
  # Unit: heads
  
  spatial_disagg_df <- call_spatial_disaggregation()
  
  bovine_pop <- get_animal_pop(year = year, animal_class = 'Bovine')
  dairy_cow <- bovine_pop[, c('Muni_ID', 'Dairy_cows')]
  dairy_cow <- merge(dairy_cow, spatial_disagg_df, 'Muni_ID')
  
  dairy_cow_n2 <- dairy_cow %>%
    group_by(nuts2_ID) %>%
    summarize(Dairy_cows = sum(Dairy_cows))
  return(dairy_cow_n2)
}

compute_regional_dairy_productivity <- function(year,write) {
  # dairy cow productivity in terms of milk production is calculated on a regional basis for the specified year
  # Dairy cow productivity = Milk production_NUTS2 / Dairy_cow_pop_NUTS2
  # Unit: kg milk per dairy cow
  
  dairy_heads_n2 <- compute_dairy_regional_heads(year)
  milk_prod_n2 <- get_milk_prod()
  milk_prod_n2 <- milk_prod_n2[, c('nuts2_ID', 'nuts2_name', as.character(year))]
  colnames(milk_prod_n2)[3] <- paste0('Milk_production', year_prefix(year))
  milk_prod_n2[, 3] <- milk_prod_n2[, 3]*1000 #convert milk to kg milk
  
  dairy_df <- merge(milk_prod_n2, dairy_heads_n2, 'nuts2_ID')
  dairy_df$dairy_productivity <- round(dairy_df[, 3]/dairy_df[, 4], 0)
  
  if (missing(write)==FALSE) {
    write_annual_data(subfolder = 'Dairy_productivity', file = dairy_df, filename = 'dairy_prod', year = year)
  }
  return(dairy_df)
}

disagg_Nex <- function(df) {
  # disaggregates dairy cow Nex at the regional to the municipality level
  
  spatial_disagg_df <- call_spatial_disaggregation()
  calc_df <- merge(spatial_disagg_df, df, 'nuts2_ID', sort = F)
  calc_df <- calc_df[, c(2,3,4, ncol(calc_df))]
  
  return(calc_df)
}

compute_dairy_Nex <- function(year) {
  # computes dairy cow N excretion coefficient for a given year based on dairy cow milk productivity
  # this uses the Portuguese GHG inventory methodology where the Nex is calculated based on a standardized value
  # of a milk productivity of 7,000 kg milk dairy cow-1 and a Nex value of 115 kg N dairy cow-1
  # Nex increases 2% for each +1000 kg milk per cow of difference
  # nex decreases 10% for each -1000 kg milk per cow of difference
  # unit: kg N dairy cow-1
  
  standard_productivity <- 7000 # kg milk per dairy cow
  standard_nex <- 115 # kg N per dairy cow
  
  dairy_cow_productivity <- compute_regional_dairy_productivity(year)
  dairy_cow_productivity$productivity_dif <- dairy_cow_productivity$dairy_productivity-7000

  pos_indx <- which(dairy_cow_productivity$productivity_dif>0)
  dairy_cow_productivity[pos_indx, 'Nex'] <- 115+115*0.02*dairy_cow_productivity[pos_indx, 'productivity_dif']/1000
  dairy_cow_productivity[-pos_indx, 'Nex'] <- 115+115*0.1*dairy_cow_productivity[-pos_indx, 'productivity_dif']/1000
  
  dairy_Nex_muni <- disagg_Nex(dairy_cow_productivity)
  return(dairy_Nex_muni)
}

## ----------------------- COMPUTE N EXCRETION PER ANIMAL SUBCLASS  ----------------------- ##
## -----------------------------------------------------------------------------------------##

compute_Nex_subclass <- function(year) {
  # computes the TOTAL N excretion for each animal subclass
  # unit: kg N yr-1
  
  animal_class <- get_animal_classes()
  nex_rates <- get_Nex_coefs()
  
  for (i in animal_class) {
    
    animal_pop <- get_animal_pop(year, animal_class = i)
    animal_subclass <- colnames(animal_pop)[-c(1,2,3)] 
    
    for (j in animal_subclass) {
      
      #search for the corresponding Nex
      ifelse(j=='Dairy_cows',
             nex_subclass <- compute_dairy_Nex(year)[, 4],
             nex_subclass <- nex_rates[which(nex_rates[, 2]==j), 3])
      animal_pop[, j] <- animal_pop[, j]*nex_subclass
    }
    write_annual_data(subfolder = 'Total_Nexcretion', file = animal_pop, filename = i, year = year)
  }
}

compute_total_Nexcreted <- function(year) {
  # compute the total amounts of N excreted for all the livestock for a given year
  # unit: kg N yr-1
  
  Nex_df <- create_main_csv()
  animal_class <- get_animal_classes()
  for (i in animal_class) {
    
    animal_Nex <-  get_module_subfolder_output(module = 'MMS_module', 
                                               submodule = 'Total_Nexcretion', 
                                               submoduleX2 = as.character(year), 
                                               file_pattern = i)
    Nex_df[, as.character(i)] <- rowSums(animal_Nex[, seq(4, ncol(animal_Nex))])
  }
  
  Nex_df[, 'total'] <- rowSums(Nex_df[, seq(4, ncol(Nex_df))])
  write_annual_data(module_name= 'MMS_module', 
                    subfolder_name = 'Total_Nexcretion', 
                    file = Nex_df, 
                    filename = 'Total_Nexcreted', 
                    year = year)
  
  rm(list=c('Nex_df', 'animal_class', 'animal_Nex'))
}


compute_N_excretion_housing_MMS <- function(total_nex_housing, animal_class, year) {
  # computes the N excretion for housing based on the MMS (liquid or solid)
  # subfunction to be used in compute_N_excretion_distribution()
  # total_nex_housing is a dataframe with the total N excretion in housing for a given animal class 
  # i corresponds to the animal class
  # j corresponds to animal subclasses used in a loop
  # subclass_dist is the animal distribution (e.g., housingg, yards)
  # unit : kg N yr-1 in housing per MMS
  
  mms_types <- c('solid', 'slurry')
  animal_subclass <- colnames(total_nex_housing)[-c(1,2,3)]  
  
  # export the total N excretion for housing (irrespective of MMS) first of all
  write_annual_data(subfolder = 'Housing', file = total_nex_housing, filename = paste0(animal_class, '_total'), year = year, subfolder_nameX2 = 'N_excretion')
  
  # compute and export N excretion according to MMS in housing
  for (i in mms_types) {
    nex_mms <- total_nex_housing
    for (j in animal_subclass) {
      
      mms_dist<- select_animal_MMS(year, animal_subclass = j, MMS = i)
      nex_mms[, j] <- round(nex_mms[, j] * mms_dist, 0)
    }
    # export N excretion according to MMS of housing for the given animal class
    write_annual_data(subfolder = 'Housing', file = nex_mms, filename = paste0(animal_class, '_', i), year = year, subfolder_nameX2 = 'N_excretion')
  }
}


compute_N_excretion_distribution <- function(year, dist_system) {
  # compute N excreted on the specified distribution system (e.g.,  Grazing, Housing, Yards)
  # unit: kg N yr-1
  
  animal_class <- get_animal_classes()
  animal_dist <- get_animal_distribution(year)

  for (i in animal_class) {
    # get total N excretion 
    total_nex <- get_MMS_output_file(subfolder = 'Total_Nexcretion', 
                                     year = year, 
                                     file_pattern = i)
    animal_subclass <- colnames(total_nex)[-c(1,2,3)]  
    
    for (j in animal_subclass) {
      ifelse(j=='Dairy_cows',
             subclass_dist <- get_dairy_distribution(),
             subclass_dist <- animal_dist[which(animal_dist[, 2]==j), ])
      total_nex[, j] <- round(total_nex[, j] * subclass_dist[, dist_system], 0)
    }
    if(dist_system=='Housing') {
      # this exports total N excretion in housing and further exports its disaggregation per MMS
      compute_N_excretion_housing_MMS(total_nex_housing = total_nex, animal_class = i, year = year)
    } else {
      # for grazxing and yards
      write_annual_data(subfolder = dist_system, file = total_nex, filename = i, year = year, subfolder_nameX2 = 'N_excretion')
    }
  }
}


compute_TAN_flows_distribution <- function(year, dist_system, animal_class) {
  # convert the N flows (kg N) into TAN flows (kg TAN) #
  # unit: kg TAN yr-1
  
  animal_class_TAN <- get_TAN_animal_class()
  
  # load N excreted for the specified distribution system
  nex_dist <- get_MMS_subfolder_output_file(subfolder = dist_system, 
                                            subfolderX2 = 'N_excretion', 
                                            year = year, 
                                            file_pattern = animal_class)
  
  # load the TAN proportion for the given animal class
  # remove _slurry or _solid in housing systems
  ifelse(dist_system=='Housing',
         class_TAN <- animal_class_TAN[which(animal_class_TAN[, 1]==sub("\\_.*", "", animal_class)), 2],
         class_TAN <- animal_class_TAN[which(animal_class_TAN[, 1]==animal_class), 2])
  # select animal subclasses 
  animal_subclass_cols <- seq(4, ncol(nex_dist))
  # convert N flows to TAN flows
  nex_dist[, animal_subclass_cols] <- mapply("*", nex_dist[, animal_subclass_cols], class_TAN)

  return(nex_dist)
}


## ----------------------- COMPUTE TOTALS (SLURRY + SOLID) ----------------------- ##
## --------------------------------------------------------------------------------##

file_pattern <- function(subfolderX2, animal_class, manure_type) {
  # this function is used to specify the file pattern for the vlook up 
  # based on the source (i.e. gaseous or excretion), animal class and/or manure type
  
  if (subfolderX2=='NH3' | subfolderX2=='N2O' | subfolderX2=='NOx' | subfolderX2=='N_excretion' | subfolderX2=='Gross_soil_manure_N') {
    
    if (missing(manure_type)==TRUE && subfolderX2=='N_excretion') {
      file_pattern <- animal_class
    }
    else {
      ifelse(subfolderX2=='Gross_soil_manure_N',
             file_pattern <- paste0('NN_', animal_class, '_', manure_type),
             file_pattern <- paste0(animal_class, '_', manure_type))  
    }
  }
  else {
    tryCatch('Wrong subfolderX2. The specified folder may have TAN data, which is not included in this function.')
  }
  return(file_pattern)
}


compute_totals <- function(year, subfolder, subfolderX2) {
  # computes TOTAL N-NH3 emissions from animal housing
  # TOTAL = SLURRY + SOLID
  # unit: kg N-NH3 yr-1
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    # data dump 
    solid <- get_MMS_subfolder_output_file(subfolder = subfolder, 
                                           subfolderX2 = subfolderX2, 
                                           year = year, 
                                           file_pattern = file_pattern(subfolderX2, i, 'solid'))
    slurry <- get_MMS_subfolder_output_file(subfolder = subfolder, 
                                            subfolderX2 = subfolderX2, 
                                            year = year, 
                                            file_pattern = file_pattern(subfolderX2, i, 'slurry'))
    
    animal_subclass_cols <- colnames(slurry)[-c(1,2,3)]  
    total <- slurry
    total[, animal_subclass_cols] <- mapply("+", slurry[, animal_subclass_cols], solid[, animal_subclass_cols])
    
    if (subfolderX2=='Gross_soil_manure_N') {
      write_annual_data(subfolder = subfolder, file = total, filename = paste0('NN_',i, '_total'), year = year, subfolder_nameX2 = subfolderX2)
    } else {
      write_annual_data(subfolder = subfolder, file = total, filename = paste0(i, '_total'), year = year, subfolder_nameX2 = subfolderX2)
    }
  }
}

