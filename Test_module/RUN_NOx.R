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
nox_source <- gaseous_select_source(pattern = 'NOx', source = c('Application', 'storage'))


#this sub-function is ought to be called in gaseous_store_filepath
#read all the files inside each major N-NH3 pathway loss
nox_db_files <- gaseous_main_fullpath(nox_source, F)

#disaggregates the main db according to the specified year
nox_99 <- gaseous_disagg_list_yr(nox_source, 1999, F)
nox_09 <- gaseous_disagg_list_yr(nox_source, 2009, F)

#aggregates all the gaseous sources of application 
nox_app_sources99 <- gaseous_merge_application(nox_99)
nox_app_sources09 <- gaseous_merge_application(nox_09)

#merge all
nox_all99 <- gaseous_merge_all(nox_app_sources99, nox_99)
nox_all09 <- gaseous_merge_all(nox_app_sources09, nox_09)

#compute all n-nh3 emissions
nox_total99 <- gaseous_compute_all(nox_all99)
nox_total09 <- gaseous_compute_all(nox_all09)

#write outputs
write_output_gaseous(nox_app_sources99, 'Application_sources99', 'N-NOx')
write_output_gaseous(nox_app_sources09, 'Application_sources09', 'N-NOx')

write_output_gaseous(nox_total99, 'Main_NOx_db99', 'N-NOx')
write_output_gaseous(nox_total09, 'Main_NOx_db09', 'N-NOx')

