source('./MMS_module/Functions/Nex_computation.R')



get_EFs <- function(gaseous_source) {
  # get MMS EFs for the given gaseous source
  
  EFs_path <- select_EMEP_param_folders(subfolder = 'EFs')
  select_gaseous_EF <- list.files(path = EFs_path, pattern = gaseous_source, full.names=T)
  r_file <- read.csv(select_gaseous_EF)
  return(r_file)
}



## ---------------- MMS N-NH3  ---------------- ##
## ---------------------------------------------##

animal_subclass_EF_NH3_distribution <- function(animal_class, animal_subclass) {
  # subsets the NH3_EFS based on animal category and respective subclasses
  # output is a subset dataframe containing all the NH3_efs based on the conditions
  
  nh3_efs <- get_EFs('NH3')

  #subset based on animal class
  nh3_efs <- nh3_efs[which(nh3_efs$Livestock_class==animal_class),]
  
  # animal subclass conditions ------------------------------------------
  # bovines
  if (animal_class=='Bovine' && grepl('Dairy', animal_subclass)==TRUE) {
    ef <- nh3_efs[which(nh3_efs$Livestock_subclass=='Dairy'), ]
  } else if (animal_class=='Bovine' && grepl('Dairy', animal_subclass)==FALSE) {
    ef <- nh3_efs[which(nh3_efs$Livestock_subclass=='Beef'), ]
  }
  # swines
  else if (animal_class=='Swine' && grepl('sows', animal_subclass)==TRUE) {
    ef <- nh3_efs[which(nh3_efs$Livestock_subclass=='Sows'), ]
  } else if (animal_class=='Swine' && grepl('sows', animal_subclass)==FALSE) {
    ef <- nh3_efs[which(nh3_efs$Livestock_subclass=='Other swine'), ]
  }
  # goats and sheeps ;; they share the same EFs
  else if (animal_class=='Goats') {
    ef <- nh3_efs[which(nh3_efs$Livestock_class=='Goats'), ]
  }
  else if (animal_class=='Sheep') {
    ef <- nh3_efs[which(nh3_efs$Livestock_class=='Sheep'), ]
  }
  # horses 
  else if (animal_class=='Equides') {
    ef <- nh3_efs[which(nh3_efs$Livestock_class=='Equides'), ]
  }
  # rabbits
  else if (animal_class=='Rabbits') {
    ef <- nh3_efs[which(nh3_efs$Livestock_class=='Rabbits'), ]
  }
  # poultry
  else if (animal_class=='Poultry' && grepl('hens', animal_subclass)==TRUE) {
    ef <- nh3_efs[which(nh3_efs$Livestock_subclass=='Hens'), ]
  }
  else if (animal_class=='Poultry') {
    
    poultry_subclass_idx <- which(nh3_efs$Livestock_subclass==animal_subclass)
    ifelse(length(poultry_subclass_idx)==0,
           ef <- nh3_efs[which(nh3_efs$Livestock_subclass=='Other poultry'), ],
           ef <- nh3_efs[which(nh3_efs$Livestock_subclass==animal_subclass), ])
  }
  return(ef)
}


select_EFs_NH3_MMS <- function(animal_class, animal_subclass, pathway, manure_type) {
  # selects the MMS NH3 EFs for a given animal subclass, a specified pathway loss and manure type
  # pathways can be Grazing, Housing, Yards, Storage or Spreading
  # in Grazing the manure type does not matter
  # unit: kg N-NH3 kg TAN-1
  
  ef_nh3_df <- animal_subclass_EF_NH3_distribution(animal_class, animal_subclass)
  
  if (pathway=='Grazing' && missing(manure_type)==TRUE) {
    ef_nh3 <- ef_nh3_df[, c('Livestock_class', 'Livestock_subclass', 'Manure_type', 'Grazing')]
    ef_nh3 <- ef_nh3[which(ef_nh3$Manure_type=='solid'), ]
  } else {
    ef_nh3 <- ef_nh3_df[, c('Livestock_class', 'Livestock_subclass', 'Manure_type', pathway)]
    ef_nh3 <- ef_nh3[which(ef_nh3$Manure_type==manure_type), ]
  }
  return(ef_nh3)
}

## ---------------- ANIMAL BEDDING ---------------- ##
## -------------------------------------------------##

get_animal_bedding <- function(livestock_subclass, param) {
  # get data of animal bedding for a given animal subclass and specified param
  # param is either bedding_mass or bedding_N
  # unit: bedding_mass is in kg straw yr-1
  # unit: bedding_N is kg N head-1 yr-1
  
  animal_bedding_path <- select_EMEP_param_folders(subfolder = 'Animal_bedding')
  r_file <- read.csv(animal_bedding_path)
  
  select_param <- ifelse(param=='bedding_mass',
                         col <- 'Straw',
                         col <- 'N_bedding')
  animal_bedding_param  <- r_file[which(r_file$Livestock_subclass==livestock_subclass), col]
  return(animal_bedding_param)
}

## ---------------- STORAGE GASEOUS EFs  ---------------- ##
## -------------------------------------------------------##

select_storage_EFs_gaseous <- function(gaseous_source, animal_class, manure_type) {
  # select the storage EFs for a given gaseous source, animal class and manure type
  # gaseous source are either NOx, N2O or N2
  
  #unit: kg N-gaseous_source kg TAN-1
  
  # load storage EF for the specified gaseous source
  ef_source <- paste0(gaseous_source, '_storage')
  select_ef_source <- get_EFs(gaseous_source = ef_source)
  
  # subset based on animal class and manure type ACCORDING TO the specified gaseous_source
  ifelse(gaseous_source=='N2O',
    select_ef_class <- select_ef_source[which((select_ef_source$Livestock_class=='Bovine') & (select_ef_source$Manure_type==manure_type)), 
                                        'Storage'],
    select_ef_class <- select_ef_source[which(select_ef_source$Manure_type==manure_type), 'Storage'])
  return(select_ef_class)
}


## ---------------- GRAZING N-N2O EFS  ---------------- ##
## -----------------------------------------------------##

select_EFs_N2O_grazing <- function(animal_class) {
  # IPCC (2006) Tier 1 emission factors
  # selects the N2O emissions factors based on the animal class specified
  # unit: kg N-N2O kg N
  
  ef_n2o_graz <- get_EFs('N2O_grazing')
  ef_n2o_graz <- ef_n2o_graz[which(ef_n2o_graz[, 1]==animal_class), 2]
  return(ef_n2o_graz)
}

select_EFs_NOx <- function() {
  # tier 1 emission factor of NOx for EVERYTHING
  # unit: kg N-NO kg N applied-1
  
  ef_NOx <- 0.04*16/30
  return(ef_NOx)
}
