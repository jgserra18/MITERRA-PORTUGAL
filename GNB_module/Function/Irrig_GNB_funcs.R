source('./Irrigation_module/Functions/Global_irrigation_functions.R')
source('./CropProduction_module/Functions/Crop_acreage_distribution.R')
source('./Fertilization_module/Functions/Fertilizer_N.R')
source('./Fertilization_module/Functions/Manure_distribution.R')
source('./Irrigation_module/Functions/Compute_crop_irrigatioN.R')
source('./Main_functions.R')


##/// PESQUISAR FERTILIZAÇÕES VS DOTAÇÕES DE REGA E TIPO DE REGA
## //// this irrig GNB is currently only using the manure spreading and not the gross N balance
d <- compile_irrig_sys_GNB_dataset(2009, TRUE)
names(d)
names(d) <- c('Muni_ID', 'ID', 'Muni', 'furrow', 'other_grav',
              'sprinkler', 'gun', 'pivot', 'drip', 'microsprinkler', 'rainfed')
df <- data.table::melt(d, id.vars = c('Muni_ID', 'ID', 'Muni'))

ggplot(df, aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot()

ggsave('Irrig_sys_GNB.pdf', dpi=600)

View(d)

# ---------------------------------------------------------------------------
# FURROW | OTHER GRAVITY | SPRINKLER | PIVOT | GUN | DRIP | MICROSPRINKLER

# (E.G.,)
# FURROW 2009 CEREALS MAIZE.CSV with (atmN manureN fertN irrigN sludgeN crop_offtake res_burnt res_removed)

# get crop irrigated areas by one of each these systems
# call (e.g.,) manure crop application rates (kg N (crop area)-1 yr-1)
# calculate total N in crop irrigated area
#


# 1 - compute crop rainfed areas
# 2 - distribute fodder production into the different acreages


get_total_crop_irrig_areas <- function(year, main_crop, crop) {
  # gets ONLY the total irrigated areas for a given crop and year
  # unit: ha yr-1
  
  irrig_area <- get_irrig_areas(year, main_crop, crop)
  irrig_area[, 'total'] <- rowSums(irrig_area[, seq(4, ncol(irrig_area))])
  return(irrig_area[, c(1,2,3, ncol(irrig_area))])
}

compute_crop_rainfed_acreage <- function(year, main_crop, crop) {
  # computes the rainfed crop acrea of a given crop and year
  # unit: ha yr-1
  
  crop_area <- get_crop_data(year, 'Areas', main_crop, crop)[, -1]
  irrig_crop_area <- get_total_crop_irrig_areas(year, main_crop, crop)
  
  # compute rainfed acreage per crop
  rainfed_crop_area <- crop_area
  rainfed_crop_area[, as.character(year)] <- round( crop_area[, as.character(year)] - irrig_crop_area[, 'total'], 2)
  
  # catch negative areas (due to rounding?) and clean the dataset
  condition <- which(rainfed_crop_area[, as.character(year)]<0)
  if (length(condition)==0) {
    return(rainfed_crop_area)
  }
  else {
    print('Rainfed acreage of this crop has negative values.')
    rainfed_crop_area <- data_cleaning(rainfed_crop_area)
    return(rainfed_crop_area)
  }
  rm(list=c('crop_area', 'irrig_crop_area'))
}

compute_total_rainfed_acreage <- function(year, only_total) {
  
  calc_df <- create_main_csv()
  main_crops <- get_maincrops_names(year)
  for (i in main_crops) {
    crop <- get_crop_names(year = year, main_crop = i)
    
    for (j in crop) {
      rain_fed <- compute_crop_rainfed_acreage(year, i, j)
      calc_df[, j] <- rain_fed[, 4]
    }
  }
  # compute total acreage of the irrig sys specified
  calc_df[, 'total'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  ifelse(missing(only_total)==TRUE | only_total == FALSE, 
         return(calc_df),
         return(calc_df[, c(1,2,3, ncol(calc_df))]))
  rm(list=c('main_crops', 'crop', 'rain_fed'))
}




## ----------------------- DATA PREP FOR IRRIG SYS GNB----------------------- ##
## ---------------------------------------------------------------------------##

check_microaspersion <- function(df) {
  # checks whether the irrigated areas acount for microaspersion
  # microaspersion is always the last col
  # if not, it creates mciroaspersion column set to 0
  # unit: ha yr-1
  
  condition <- grepl('microaspersion', names(df)[ncol(df)])
  
  if (condition==FALSE) {
    df[, 'microaspersion'] <- 0
  }
  return(df)
}


get_crop_irrig_sys_area <- function(year, main_crop, crop, irrig_sys) {
  # calls crop irrigated acreage and subsets the dataset based on the specified irrig sys
  # additionally, it checks and corrects the microaspersion subdataset within
  # unit: ha yr-1
  
  irrig_area <- get_irrig_areas(year, main_crop, crop)
  
  if (irrig_sys=='microaspersion') {
    irrig_area <- check_microaspersion(irrig_area)
  }
  # find the column regarding the specified irrig syst
  irrig_sys_id <- which(names(irrig_area)==irrig_sys)
  # subset irrig_area
  irrig_area <- irrig_area[, c(1,2,3, irrig_sys_id)]
  return(irrig_area)
}


compute_total_irrig_sys_area <- function(year, irrig_sys, only_total) {
  # computes the total crop acreage that uses a given irrig sys
  # unit: ha yr-1
  
  calc_df <- create_main_csv()
  
  main_crops <- get_maincrops_names(year)
  for (i in main_crops) {
    crop <- get_crop_names(year = year, main_crop = i)
    
    for (j in crop) {
      irrig_area <- get_crop_irrig_sys_area(year = year, 
                                            main_crop = i, 
                                            crop = j, 
                                            irrig_sys = irrig_sys)
      calc_df[, j] <- irrig_area[, 4]
    }
  }
  # compute total acreage of the irrig sys specified
  calc_df[, 'total'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  ifelse(missing(only_total)==TRUE | only_total == FALSE, 
    return(calc_df),
    return(calc_df[, c(1,2,3, ncol(calc_df))]))
  rm(list=c('main_crops', 'crop', 'irrig_area'))
}


create_Nflows_param_df <- function() {
  # creates a dataframe, for each relevant GNB flow, the module, submodule and submoduleX2( for outputs)
  # to be able to get the crop acreage N flows
  
  flows_df <- data.frame(
    N_param = c('BNF', 'manure', 'fertN', 'sludgeN', 'irrigation',
                'Crop_Nremoval', 'Crop_Nres_burnt', 'Crop_Nres_removed', 'Crop_Nroughage'),
    module = c('BNF_module', 'Fertilization_module', 'Fertilization_module', 'Fertilization_module', 'Irrigation_module',
               'CropProduction_module', 'CropProduction_module', 'CropProduction_module', 'CropProduction_module'),
    submodule = c('Crop_acreage_BNF', 'Manure_rates', 'Fertilization_FAN_rates', 'Biosolids_rates', 'Irrigation_crop_N',
                  'Crop_acreage_distribution', 'Crop_acreage_distribution', 'Crop_acreage_distribution', 'Crop_acreage_distribution'),
    submoduleX2 = c('', '', '', '', '', 'Crop_Nremoval', 'Crop_Nres_burnt', 'Crop_Nres_removed', 'Crop_Nroughage'))
  return(flows_df)
}

getter_crop_irrigatioN <- function(year, irrig_sys, main_crop, crop) {
  # gets crop irrigation N and subsets the dataset based on the specified irrig sys
  # unit: kg N yr-1
  
  crop_irrigN <- get_module_subfolder_output(module = 'Irrigation_module',
                                             submodule = 'Irrigation_crop_N', 
                                             submoduleX2 = main_crop, 
                                             file_pattern = crop, 
                                             submoduleX3 = year)
  # subset based on the specified irrig sys
  irrig_sys_id <- which(names(crop_irrigN)==irrig_sys)
  crop_irrigsysN <- crop_irrigN[, c(1,2,3, irrig_sys_id)]
  
  return(crop_irrigsysN)
  rm(list=c('crop_irrigN', 'irrig_sys_id'))
}

getter_crop_Ninputs <- function(year, N_param, main_crop, crop) {
  # gets the crop acreage distributed for the specified N-inputs
  # atmN is excluded as it will be a fixed value per municipality
  # unit : kg N (crop area)-1 yr-1
  # unit (irrigation): kg N yr-1
  
  Nflows_df <- create_Nflows_param_df()
  param_row <- which(Nflows_df[, 1]==N_param)
  module <- as.character(Nflows_df[param_row, 2])
  submodule <- as.character(Nflows_df[param_row, 3])
  
  flow <- get_module_subfolder_output(module = module,
                                      submodule = submodule, 
                                      submoduleX2 = main_crop, 
                                      file_pattern = crop, 
                                      submoduleX3 = year)
  return(flow)
  rm(list=c('Nflows_df', 'param_row', 'module', 'submodule'))
}

getter_crop_Noutputs <- function(year, N_param, main_crop, crop) {
  # calls a function from Crop_acreage_distribution.R
  # gets and returns a dataframe related to the specified crop and N floe
  # unit: kg N (crop area)-1 yr-1

  flow <- get_crop_acreage_dist_Noutput(N_output = N_param, 
                                        year = year, 
                                        main_crop = main_crop, 
                                        crop = crop)
  return(flow)
}


getter_crop_all_Nflows <- function(year, N_param, main_crop, crop) {
  
  ifelse(N_param == 'manure' | N_param == 'fertN' | N_param == 'sludgeN' | N_param == 'BNF' | N_param =='irrigation',
    flow_df <- getter_crop_Ninputs(year, N_param, main_crop, crop),
    flow_df <- getter_crop_Noutputs(year, N_param, main_crop, crop))
  return(flow_df)
}


get_Nflow_params <- function() {
  
  Nflows_params <- c('BNF', 'manure', 'fertN', 'sludgeN', 'irrigation',
                     'Crop_Nremoval', 'Crop_Nres_burnt', 'Crop_Nres_removed', 'Crop_Nroughage')
  return(Nflows_params)
}


get_Nflow_maincrops <- function(N_param) {
  # returns the main crop classes for a given GNB parameter
  
  if (N_param=='BNF') {
    main_crops <- c('pulses', 'pastures')
  }
  else if (N_param=='fertN') {
    main_crops <- get_all_maincrop_names()
  }
  else if (N_param=='manure' | N_param == 'sludgeN') {
    main_crops <- append(get_fodder_main.crops(), get_non.fodder_main.crops())
  }
  else if (N_param=='irrigation') {
    main_crops <- get_maincrops_names(2009)
  }
  else if (N_param=='Crop_Nremoval') {
    main_crops <- get_maincrops_offtake()
  }
  else if (N_param=='Crop_Nroughage') {
    main_crops <- c('forage', 'pastures')
  }
  else if (N_param=='Crop_Nres_burnt') {
    main_crops <- get_burnt_maincrops()
  }
  else if (N_param=='Crop_Nres_removed') {
    main_crops <- c('cereals', 'industry', 'potato', 'pulses', 'pastures')
  }
  return(main_crops)
}

get_Nflow_crops <- function(N_param, main_crop) {
  # implemented to handle the fact that extensive pastures are not fertilized
  
  if ((N_param =='manure' | N_param == 'sludgeN' | N_param =='fertN') & main_crop=='pastures') {
    crop <- 'intensive_pasture'
  }
  else {
    crop <- get_all_crop_names(main.crop = main_crop)
  }
  return(crop)
}

## ----------------------- DATA PREP FOR IRRIG SYS GNB----------------------- ##
## ---------------------------------------------------------------------------##



general_func_irrigation_sys_Nflows_param <- function(year, irrig_sys, N_param) {
  # general func to calculate the different GNB N flows for each crop within their irrigated acreage and different irrig sys
  # unit: kg N yr-1 irrig sys-1
  
  param_df <- create_main_csv()
  irrig_sys_area <- compute_total_irrig_sys_area(year, irrig_sys, only_total = FALSE)
  
  crops <- names(irrig_sys_area)[-seq(1,3)]
  
  main_crops <- get_Nflow_maincrops(N_param)
  
  print(paste0(N_param, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'))
  for (i in main_crops) {
    crop <- get_Nflow_crops(N_param = N_param, main_crop = i)
    print(paste0('Working within ', i, 'and ', irrig_sys, '-----'))
    
    for (j in crop) {
      # irrigation already has the crop N flows calculated
      if (N_param != 'irrigation') {
        crop_Nflow <- getter_crop_all_Nflows(year = year, 
                                             N_param = N_param, 
                                             main_crop = i, 
                                             crop = j) 
        # find crop col in irrig sys area
        crop_col <- which(names(irrig_sys_area)==j)
        # irrigation N flows are already given in kg N yr-1 
        crop_Nflow[, 4] <- round( crop_Nflow[, 4] * irrig_sys_area[, crop_col], 2)
      }
      else {
        # get the total crop irrigation N for this irrig sys
       crop_Nflow <- getter_crop_irrigatioN(year = year, 
                                            irrig_sys = irrig_sys, 
                                            main_crop = i, 
                                            crop = j) 
      }

      param_df[, j] <- crop_Nflow[, 4]
    }
  }

  # sum the totals -------------------------------------------------
  ifelse(ncol(param_df)>4,
    param_df[, 'total'] <- rowSums(param_df[, seq(4, ncol(param_df))]),
    colnames(param_df)[4] <- 'total')

  if (irrig_sys=='microaspersion') {
    irrig_sys <- 'microsprinkler'
  }
  
  write_annual_data(module_name = 'GNB_module', 
                    subfolder_name = 'Irrig_sys_NB', 
                    file = param_df, 
                    filename = N_param,
                    year = year, 
                    subfolder_nameX2 = irrig_sys)
  print('================================================================================')
}


compute_Nflows_irrig_areas_N <- function(year) {
  # computes the N flows for the different GNB param
  #  the irrigated area for each crop is assessed individually
  # unit: kg N yr-1
  
  Nflows_params <- get_Nflow_params()
  irrig_sys <- get_irrig_sys_names()
  
  for (i in Nflows_params) {
    for (j in irrig_sys) {
      general_func_irrigation_sys_Nflows_param(year = year, 
                                      irrig_sys = j, 
                                      N_param = i)
    }
  }
}


general_func_irrig_sys_Ninputs <- function(year, irrig_sys) {
  # general function to compute the total N inputs for the specified irrig syst
  # unit: kg N yr-1
  
  Ninputs <- c('manure', 'sludgeN', 'fertN', 'BNF', 'irrigation')
  calc_df <- create_main_csv()
  
  for (i in Ninputs) {
    
    irrig_sys_param <- get_module_subfolder_output(module = 'GNB_module', 
                                                   submodule = 'Irrig_sys_NB', 
                                                   submoduleX2 = irrig_sys, 
                                                   file_pattern = i, 
                                                   submoduleX3 = year)
    calc_df[, i] <- irrig_sys_param[, 'total']
  }
  calc_df[, 'tot_Ninputs'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  
  return(calc_df)
  rm(list=c('Ninputs', 'irrig_sys_param'))
}



general_func_irrig_sys_Noutputs <- function(year, irrig_sys) {
  # general function to compute the total N outputs for the specified irrig syst
  # unit: kg N yr-1
  
  Noutputs<- c('Crop_Nremoval', 'Crop_Nres_burnt', 'Crop_Nres_removed', 'Crop_Nroughage')
  calc_df <- create_main_csv()
  
  for (i in Noutputs) {
    
    irrig_sys_param <- get_module_subfolder_output(module = 'GNB_module', 
                                                   submodule = 'Irrig_sys_NB', 
                                                   submoduleX2 = irrig_sys, 
                                                   file_pattern = i, 
                                                   submoduleX3 = year)
    calc_df[, i] <- irrig_sys_param[, 'total']
  }
  calc_df[, 'tot_Noutputs'] <- rowSums(calc_df[, seq(4, ncol(calc_df))])
  
  return(calc_df)
  rm(list=c('Noutputs', 'irrig_sys_param'))
}


## -----------------------RAINFED DATA PREPARATION----------------------- ##
## -----------------------------------------------------------------------##


general_func_Nflows_rainfed <- function(year, N_param) {
  # similar to the general func applied to Ninputs, Noutputs to the different N_Params
  # this, however, is only applied to rainfed crops
  # unit: kg N yr-1
  
  rainfed_df <- create_main_csv()
  
  main_crops <- get_Nflow_maincrops(N_param)
  for (i in main_crops) {
    crops <- get_Nflow_crops(N_param = N_param, main_crop = i)
    
    for (j in crops) {
      print(j)
      rainfed_area <- compute_crop_rainfed_acreage(year = year, 
                                                   main_crop = i, 
                                                   crop = j)
      crop_Nflow <- getter_crop_all_Nflows(year = year, 
                                           N_param = N_param, 
                                           main_crop = i, 
                                           crop = j) 
      crop_Nflow[, 4] <- round( crop_Nflow[, 4] * rainfed_area[, as.character(year)], 2)
      # join the crop N flows for a given param ;; if irrigation, set it to 0 because it is rainfed
      ifelse(N_param=='irrigation',
             rainfed_df[, j] <- 0,
             rainfed_df[, j] <-crop_Nflow[, 4] )
    }
  }
  # sum the totals -------------------------------------------------
  if (ncol(rainfed_df)>4) {
    rainfed_df[, 'total'] <- rowSums(rainfed_df[, seq(4, ncol(rainfed_df))])
  }
  else {
    rainfed_df[, 'total'] <- rainfed_df[, 4]
  }
  
  write_annual_data(module_name = 'GNB_module', 
                    subfolder_name = 'Irrig_sys_NB', 
                    file = rainfed_df, 
                    filename = N_param,
                    year = year, 
                    subfolder_nameX2 = 'rainfed')
  print('================================================================================')
}

compute_Nflows_rainfed_areas_N <- function(year) {
  # compute ALL different GNB N flows for rainfed areas
  # unit: kg N yr-1
  
  Nflows_params <- get_Nflow_params()
  for (i in Nflows_params) {
    print(i)
    general_func_Nflows_rainfed(year = year, N_param = i)
  }
}

## -----------------------IRRIG SYS GNB----------------------- ##
## ------------------------------------------------------------##


general_func_irrig_sys_GNB <- function(year, irrig_sys, in_tot_area) {
  # general function to compute the total GNB for the specified irrig syst
  # the output can be given based on the total N (kg N) 
  # or alternatively based on the total acreage of the irrig sys specified
  # unit: kg N yr-1
  
  GNB_df <- create_main_csv()
  
  Ninputs <- general_func_irrig_sys_Ninputs(year, irrig_sys)
  Noutputs <- general_func_irrig_sys_Noutputs(year, irrig_sys)
  
  
  GNB_df[, 'Ninputs'] <- Ninputs[, 'tot_Ninputs']
  GNB_df[, 'Noutputs'] <- Noutputs[, 'tot_Noutputs']
  GNB_df$GNB <- round( GNB_df$Ninputs - GNB_df$Noutputs , 2)
  
  if (in_tot_area==TRUE) {
    
    if (irrig_sys=='microsprinkler') {
      irrig_sys_area <- compute_total_irrig_sys_area(year, 'microaspersion', only_total = TRUE)
    }
    else if (irrig_sys=='rainfed') {
      irrig_sys_area <- compute_total_rainfed_acreage(year, only_total = TRUE)
    }
    else {
      irrig_sys_area <- compute_total_irrig_sys_area(year, irrig_sys, only_total = TRUE)
    }
    
    calc_cols <- seq(4, ncol(GNB_df))
    
    GNB_df[, calc_cols] <- sapply(calc_cols, function(x) round( GNB_df[, x] / irrig_sys_area[, 4], 2))
    return(GNB_df)
  } 
  else {
    return(GNB_df)
  }
}


compute_irrig_sys_GNB <- function(year, in_tot_area) {
  # calculate the GNB for each irrir sys
  # similary to the general GNB func, output can be in kg N yr-1 or kg N (tot_irrig_sys_area)-1 yr-1
  
  ifelse(in_tot_area==TRUE,
         file_name <- 'HA_GNB',
         file_name <- 'N_GNB')
  
  irrig_sys <- append(get_irrig_sys_names(), 'rainfed')
  for (i in irrig_sys) {
    if (i=='microaspersion') {
      i <- 'microsprinkler'
    }
    print(i)
    GNB_df <- general_func_irrig_sys_GNB(year = year, irrig_sys = i, in_tot_area = in_tot_area)
    write_annual_data(module_name = 'GNB_module', 
                      subfolder_name = 'Irrig_sys_NB', 
                      file = GNB_df, 
                      filename = file_name,
                      year = year, 
                      subfolder_nameX2 = i)
    
  }
}

compile_irrig_sys_GNB_dataset <- function(year, in_tot_area) {
  
  calc_df <- create_main_csv()
  ifelse(in_tot_area==TRUE,
         file_pattern <- 'HA_GNB',
         file_pattern <- 'N_GNB')
  irrig_sys <- append(get_irrig_sys_names(), 'rainfed')
  
  for (i in irrig_sys) {
    if (i=='microaspersion') {
      i <- 'microsprinkler'
    }
    irrig_sys_GNB <- get_module_subfolder_output(module = 'GNB_module', 
                                                 submodule = 'Irrig_sys_NB', 
                                                 submoduleX2 = i, 
                                                 file_pattern =file_pattern, 
                                                 submoduleX3 = year)
    calc_df[, i] <- irrig_sys_GNB[, 'GNB']
  }
  return(calc_df)
}


