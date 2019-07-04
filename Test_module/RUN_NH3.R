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
nh3_source <- gaseous_select_source(pattern = 'NH3', source = c('Application', 'storage', 'housing'))

#nh3_app <- gaseous_select_source(pattern = 'NH3', source = 'Application')
#nh3_storage <- gaseous_select_source(pattern = 'NH3', source = 'storage')
#nh3_house <- gaseous_select_source(pattern = 'NH3', source = 'housing')


#this sub-function is ought to be called in gaseous_store_filepath
#read all the files inside each major N-NH3 pathway loss
nh3_db_files <- gaseous_main_fullpath(nh3_source, T)

#optional
#this creates a list
nh3_all_losses_list <- gaseous_store_filepath(nh3_source, T)

#this is also viable
#nh3_app_files <- store_folder_files(nh3_app)
#nh3_storage <- store_folder_files(nh3_storage)
#nh3_house <- store_folder_files(nh3_house)

#disaggregates the main db according to the specified year
nh3_99 <- gaseous_disagg_list_yr(nh3_source, 1999, T)
nh3_09 <- gaseous_disagg_list_yr(nh3_source, 2009, T)

#aggregates all the gaseous sources of application 
nh3_app_sources99 <- gaseous_merge_application(nh3_99)
nh3_app_sources09 <- gaseous_merge_application(nh3_09)

#merge all
nh3_all99 <- nh3_merge_all(nh3_app_sources99, nh3_99)
nh3_all09 <- nh3_merge_all(nh3_app_sources09, nh3_09)

#compute all n-nh3 emissions
nh3_total99 <- gaseous_compute_all(nh3_all99)
nh3_total09 <- gaseous_compute_all(nh3_all09)

#write outputs
write_output_gaseous(nh3_app_sources99, 'Application_sources99', 'N-NH3')
write_output_gaseous(nh3_app_sources09, 'Application_sources09', 'N-NH3')

write_output_gaseous(nh3_total99, 'Main_NH3_db99', 'N-NH3')
write_output_gaseous(nh3_total09, 'Main_NH3_db09', 'N-NH3')

