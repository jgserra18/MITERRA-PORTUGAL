source('./Main_functions.R')
source('./GIS_module/Function/General_GIS_functions.R')

# manure spreading is already calculated
# Sludge N --> Nh3 emissions (EMEP, 2016) Tier 1
# Fertiliser N --> NH3 emissions (EMEP, 2016) Tier 2

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ----------------------------------------
# N-NH3 emissions can be calculated for each crop N application 



## ----------------------- SLUDGE N-NH3 EMISSIONS----------------------- ##
## ----------------------------------------------------------------------##

get_sludge_NH3_efs <- function() {
  # EMEP (2016)  Table 3.1
  # unit: kg N-NH3 (kg N applied)-1 yr-1
  
  sludge_N <- 0.08 * 14/17
  return(sludge_N)
}

compute_sludge_NH3 <- function(year) {
  # compute N-NH3 emissions from biosolids based on EMEP (2016) Tier 1
  # sludgeN at the municipality level had to be converted from tonnes to kg N
  # unit: kg N-NH3
  
  NH3_EF <- get_sludge_NH3_efs()
  sludgeN <- get_module_subfolder_output(module = 'Fertilization', 
                                         submodule = 'Biosolids_rates',
                                         submoduleX2 = 'Total_municipality',
                                         file_pattern = 'total', 
                                         submoduleX3 = year)
  # compute sludge N-NH3 emissions (in kg)
  sludgeN[, as.character(year)] <- sludgeN[, as.character(year)] * 1000 * NH3_EF
  write_annual_data(module_name = 'Environmental_losses_module', 
                    subfolder_name = 'NH3',
                    file = sludgeN, 
                    filename = 'Biosolids_totals', 
                    year = year, 
                    subfolder_nameX2 = 'Biosolids')
  rm(list=c('NH3_EF', 'sludgeN'))
}


## ----------------------- Fertiliser N-NH3 EMISSIONS----------------------- ##
## --------------------------------------------------------------------------##

get_rcl_soilpH <- function() {
  # gets soil pH (in log) and reclassifies it according to EMEP EF scheme (i.e., normal (<7) and high (>7))
  
  soil_ph <- get_activity_data(subfolder = 'EnvironmentalConditions', subfolderX2 = 'Soil', file_pattern = 'pH')
  soil_ph <- reclassify(x = soil_ph, rcl = c(0, 7, 1, 7, +Inf, 2))
  return(soil_ph)
}

get_tot_fert_muni <- function(year) {
  # calls the total fertiliser N for each municipality for a given year
  # unit: kg N yr-1

  muni_fertN <- get_module_subfolder_output(module = 'Fertilization_module', 
                                             submodule = 'Fertiliser_FAN', 
                                             submoduleX2 = 'Municipality_totals',
                                             file_pattern = 'Municipality_total', 
                                             submoduleX3 = year)
  # select only totals
  muni_fertN <- muni_fertN[, c(1,2,3, ncol(muni_fertN))]
  return(muni_fertN)
}


## ----------------------- FERTILISER N TYPOLOGY DOWNSCALING----------------------- ##

get_fertiliser_typology <- function() {
  # gets APA (2018) historical data about national fertiliser usage per type
  # unit: ktonnes N yr-1
  
  crop_data_folder <- load_raw_data(subfolder = 'Crop_data')
  fert_support <- list.files(path = crop_data_folder, pattern = 'Fertilization_support', full.names = T)
  fert_support <- list.files(path = fert_support, pattern = 'National_fertilizer', full.names = T)
  fert_type <- list.files(path = fert_support, full.names = T)
  fert_type <- read.csv(fert_type)
  
  return(fert_type)
  rm(list=c('crop_data_folder',))
}

compute_fertiliser_typology_prop <- function(yr) {
  # computes the fraction of total fertiliser N at the mmainland level for a given fert type
  
  fert_type <- get_fertiliser_typology()
  fert_type <- subset(fert_type, year==yr)
  
  calc_cols <- seq(2, ncol(fert_type))
  fert_type[1, calc_cols] <- sapply(fert_type[1, calc_cols], function(x) x/fert_type[1, ncol(fert_type)])
  colnames(fert_type)[2:ncol(fert_type)] <- gsub('_ktN', '', colnames(fert_type)[2:ncol(fert_type)] )
  return(fert_type)
}


general_fertiliser_typology_N <- function(year, fert_type) {
  # general function to compute the total N applied for a specified fert type
  # unit: kg N yr-1
  
  fertN <- get_tot_fert_muni(year)
  
  # select fert type fraction at the national level
  fert_type_prop <- compute_fertiliser_typology_prop(year)
  fert_type_prop <- fert_type_prop[1, fert_type]
  
  fertN[, 4] <- round(fertN[, 4] * fert_type_prop, 2)
  return(fertN)
}

spatialise_fertiliser_typology_N <- function(year, fert_type) {
  # spatialize the fertiliser N for a given fertiliser typology#
  # unit: kg N ha-1 yr-1
  
  
  # compute fertN in kg N UAA-1 yr-1
  fertN_type <- general_fertiliser_typology_N(year, fert_type)
  fertN_type[, 4] <- fertN_type[,4]/load_uaa(2009)
  
  #spatialise to agricultural land
  fertN_type <- spatial_disagg_muni_caa(fertN_type, 'Muni_ID', 'total_municipality', year)
  
  return(fertN_type)
}




list_fert_type <- function() {
  # get a list with all the specified fertiliser types (e.g., urea,)
  
  fert_type <- get_activity_data(subfolder = 'EFs', file_pattern = 'NH3')
  fert_type <- fert_type[, 1]
  
  return(fert_type)
}


disaggregate_EFs_environmental <- function(climatic_region, sph, fert_type) {
  # call IPCC 2006 climatic regions for mainland Portugal where 1 == mediterranean/warm and 2 == temperate/cool
  # call soil pH where 1 == normal/<7 and 2==high/>=7
  # spatializes the EF_NH3 for synthetic fertilisers for a given climatic region, soil ph and fert_type
  # unit: kg N-NH3 kg N applied-1 yr-1
  
  ef_nh3 <- get_activity_data(subfolder = 'EFs', file_pattern = 'NH3')
  
  climatic_reg <- get_activity_data(subfolder = 'Climatic_data',
                                    subfolderX2 = 'Climatic_regions',
                                    file_pattern = 'Climatic_regions')
  soil_ph <- get_rcl_soilpH()
  
  if (climatic_region=='mediterranean') {
    reg <- (climatic_reg[[1]]==1)
  }
  else {
    reg <- (climatic_reg[[1]]==2)
  }
  
  if (sph=='normal') {
    ph <- (soil_ph[[1]]==1)
  }
  else {
    ph <- (soil_ph[[1]]==2)
  }
  
  pattern <- paste0(climatic_region, '_', sph)
  col_idx <- which(colnames(ef_nh3)==pattern)
  fert_idx <- which(ef_nh3[, 1]==fert_type)

    ef <- ef_nh3[fert_idx, col_idx]
  ef <- ef * reg * ph
  
  return(ef)
  rm(list=c('ef_nh3', 'climatic_reg', 'soil_ph', 'reg', 'ph', 'col_idx', 'fert_idx'))
}


compute_fert_NH3_type <- function(year, fert_type) {
  # computes the N-NH3 emissions from the application of a given fert type (specified)
  # implements a wrap-up function that spatializes EMEP 2016 emission factors according to climatic region and soil pH
  # unit: kg N-NH3 ha-1 yr-1
  
  fertN_type <- spatialise_fertiliser_typology_N(year, fert_type)
  
  clim <- c('mediterranean', 'temperate')
  ph <- c('normal', 'high')
  
  calc_list <- list()
  
  for (i in clim) {
    for (j in ph) {
      
      print(paste0('Working in ', i, ' regions with ', j, ' soil pH.'))
      ef_nh3 <- disaggregate_EFs_environmental(climatic_region = i, sph = j, fert_type = fert_type)
      fert_nh3 <- fertN_type * ef_nh3
      calc_list <- append(calc_list, fert_nh3)
    }
  }
  calc_list <- stack(calc_list)
  fert_nh3 <- sum(calc_list)
  
  return(fert_nh3)
  rm(list=c('fertN_type', 'clim', 'ph', 'calc_list', 'ef_nh3', 'calc_list'))
}


compute_fert_NH3_total <- function(year, write) {
  # computes the UNADJUSTED NH3 emissions from synthetic fertilisers for
    # 1 - all fert types
    # 2 - total fertilisers
  # unit: kg N-NH3 ha-1 yr-1
  # note: the area is of totat CLC UAA
  
  fert_types <- list_fert_type()
  calc_list <- list()
  
  for (i in fert_types) {
    print(i)
    
    fert_nh3 <- compute_fert_NH3_type(year, i)
    calc_list <- append(calc_list, fert_nh3)
    
    if (write==TRUE) {
      write_annual_data(module_name = 'Environmental_losses_module', 
                        subfolder_name = 'NH3',
                        file = fert_nh3, 
                        filename = i, 
                        year = year, 
                        subfolder_nameX2 = 'Fertiliser_N')
    }
  }
  calc_list <- stack(calc_list)
  tot_nh3_fert <- sum(calc_list)
  write_annual_data(module_name = 'Environmental_losses_module', 
                    subfolder_name = 'NH3',
                    file = tot_nh3_fert, 
                    filename = 'Total_fertN', 
                    year = year, 
                    subfolder_nameX2 = 'Fertiliser_N')
  
  rm(list=c('fert_types', 'calc_list', 'fert_nh3', 'calc_list', 'fert_nh3', 'tot_nh3_fert'))
}

compute_unadjusted_muni_fertNH3 <- function(year, write) {
  
  fert_nh3 <- get_module_subfolder_output(module = 'Environmental_losses_module', 
                                         submodule = 'NH3', 
                                         submoduleX2 = 'Fertiliser_N',
                                         file_pattern = 'Total_fertN', 
                                         submoduleX3 = year)
  muni_df <- create_main_csv()
  muni <- get_muni_shp('sf') 
  ctr <- 0
  
  for (i in muni_df[, 1]) {
    
    print(paste0('Working with municipality ID ', i))
    ctr <- ctr + 1
    sb_muni <- subset(muni, Muni_ID == i)
    sb_fertnh3 <- crop(fert_nh3, extent(sb_muni))
    sb_fertnh3 <- mask(sb_fertnh3, sb_muni)
    muni_df[ctr, 'avg_fertnh3'] <- round( cellStats(sb_fertnh3, 'mean') , 3)
  }
  
  ifelse(write==TRUE,
         write_annual_data(module_name = 'Environmental_losses_module', 
                           subfolder_name = 'NH3',
                           file = muni_df, 
                           filename = 'fert_nh3_df', 
                           year = year, 
                           subfolder_nameX2 = 'Fertiliser_N'),
         return(muni_df))
  rm(list=c('fert_nh3', 'muni', 'ctr', 'sb_muni', 'sb_fertnh3'))
}

d <- compute_unadjusted_muni_fertNH3(2009, T)

fert_nh3 <- get_module_subfolder_output(module = 'Environmental_losses_module', 
                                        submodule = 'NH3', 
                                        submoduleX2 = 'Fertiliser_N',
                                        file_pattern = 'Total_fertN', 
                                        submoduleX3 = 2009)
