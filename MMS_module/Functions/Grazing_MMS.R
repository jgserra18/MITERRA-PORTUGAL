source('./MMS_module/Functions/Nex_computation.R')
source('./MMS_module/Functions/Prepare_MMS_EFs.R')


compute_N_excretion_grazing <- function(year) {
  # compute N excreted onto pastures from grazing animals for a given year
  # unit: kg N yr-1
  
  tot_nex_grazing <- compute_N_excretion_distribution(2009, 'Grazing')
}


## ---------------- GRAZING N-NH3  ---------------- ##
## -------------------------------------------------##

compute_NH3_grazing <- function(year) {
  # computes grazing N-NH3 emissions for each animal class and respective subclasses
  # unit: kg N-NH3 yr-1
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    graz_TAN <- compute_TAN_flows_distribution(year = year, 
                                               dist_system = 'Grazing', 
                                               animal_class = i)
    graz_NH3 <- graz_TAN
    animal_subclass <- colnames(graz_TAN)[-c(1,2,3)]  
    
    for (j in animal_subclass) {
      nh3_ef <- select_EFs_NH3_MMS(animal_class = i, animal_subclass = j, pathway = 'Grazing')
      graz_NH3[, j] <- graz_TAN[, j] * nh3_ef$Grazing
    }
    write_annual_data(subfolder = 'Grazing', file = graz_NH3, filename = i, year = year, subfolder_nameX2 = 'NH3')
  }
}

## ---------------- GRAZING N-N2O and NOx tier 1 ---------------- ##
## ---------------------------------------------------------------##

compute_N_gaseous_grazing_tier1 <- function(year, gas) {
  # general function to ocmpute grazing tier 1 emissions
  # this applies for N2O (IPCC, 2006) and NOx (EMEP, 2016)
  # Unit: either kg N-N2O or kg N-NOx per kg N applied
  
  animal_class <- get_animal_classes()
  
  for (i in animal_class) {
    print(i)
    nex_graz <- get_MMS_subfolder_output_file(subfolder = 'Grazing', 
                                              subfolderX2 = 'N_excretion', 
                                              year = year, 
                                              file_pattern = i)
    # select EF based on the specified gaseous source
    # unit: kg N-GAS kg N applied-1
    if(gas=='N2O') {
           ef_graz <- select_EFs_N2O_grazing(animal_class = i)
    } else if (gas=='NOx') {
           ef_graz <- select_EFs_NOx()
    }
    
    # compute N emission from grazing
    animal_subclass_cols <- seq(4, ncol(nex_graz))
    nex_graz[, animal_subclass_cols] <- mapply("*", nex_graz[, animal_subclass_cols], ef_graz)
    write_annual_data(subfolder = 'Grazing', file = nex_graz, filename = i, year = year, subfolder_nameX2 = gas)
    
  }
}

compute_N2O_grazing <- function(year) {
  # general function applied to N2O emissions
  # see compute_N_gaseous_grazing_tier1
  
  compute_N_gaseous_grazing_tier1(year = year, gas = 'N2O')
}

compute_NOx_grazing<- function(year) {
  # general function applied to NOx emissions
  # see compute_N_gaseous_grazing_tier1
  
  compute_N_gaseous_grazing_tier1(year = year, gas = 'NOx')
}
