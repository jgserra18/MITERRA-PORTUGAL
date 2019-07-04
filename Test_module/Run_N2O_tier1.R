#main script functions
source('./Main_functions.R')
source('./Gaseous_functions.R')

##################################
#============== RUN =============#
##################################
lol <- set_maindata_folder()
#lol

#prints for each gaseous source: the folder name name and emission path 
print_gaseous_sources()

#load all the major N-NH3 pathways
#the subsequent functions can be optimized and the scalability improved
n2o_source <- gaseous_select_source(pattern = 'N2O', source = c('Application', 'storage'))

#this sub-function is ought to be called in gaseous_store_filepath
#read all the files inside each major N-NH3 pathway loss
n2o_db_files <- gaseous_main_fullpath(n2o_source, F)

#disaggregates the main db according to the specified year
n2o_99 <- gaseous_disagg_list_yr(n2o_source, 1999, F)
n2o_09 <- gaseous_disagg_list_yr(n2o_source, 2009, F)

#filters irrigatioon N2O according to specifications
#mode is either ON or OFF
n2o_99 <- select_irrigation_n2o_module(n2o_99, 'ON')
n2o_09 <- select_irrigation_n2o_module(n2o_09, 'ON')

tiers <- c('tier1', 'tier2')

for (i in tiers)
{
  #aggregates all the gaseous sources of application 
  n2o_app_sources99 <- gaseous_merge_application(n2o_99, i)
  n2o_app_sources09 <- gaseous_merge_application(n2o_09, i)
  #merge all
  n2o_all99 <- gaseous_merge_all(n2o_app_sources99, n2o_99)
  n2o_all09 <- gaseous_merge_all(n2o_app_sources09, n2o_09)
  #compute all n-n2o emissions
  n2o_total99 <- gaseous_compute_all(n2o_all99)
  n2o_total09 <- gaseous_compute_all(n2o_all09)

  irrig_filename <- correct_filename_irrigation_n2o(n2o_total09)
  #write outputs
  write_output_gaseous(n2o_app_sources99, paste0(i, '_Application_sources99', irrig_filename), 'N-N2O')
  write_output_gaseous(n2o_app_sources09, paste0(i, '_Application_sources09', irrig_filename), 'N-N2O')
  
  write_output_gaseous(n2o_total99, paste0(i, '_Main_N2O_db99', irrig_filename), 'N-N2O')
  write_output_gaseous(n2o_total09, paste0(i, '_Main_N2O_db09', irrig_filename), 'N-N2O')
}

n2o_app_sources99 <- gaseous_merge_application(n2o_99, i)
n2o_app_sources09 <- gaseous_merge_application(n2o_09, i)
#merge all
n2o_all99 <- gaseous_merge_all(n2o_app_sources99, n2o_99)
n2o_all09 <- gaseous_merge_all(n2o_app_sources09, n2o_09)
#compute all n-n2o emissions
n2o_total99 <- gaseous_compute_all(n2o_all99)
n2o_total09 <- gaseous_compute_all(n2o_all09)

nc <- ncol(n2o_total09)
nc
n2o_total09[, seq(4,nc)] <- mapply(function(x,y) x*y,
                                                 n2o_total09[, seq(4, nc)],
                                                 load_uaa(2009))
n2o_total09 <- data_cleaning(n2o_total09)
44/28*sapply(n2o_total09[, seq(4,nc)], function(x) sum(x))
