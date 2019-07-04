source('./Main_functions.R')
source('./Irrigation_module/Functions/Global_irrigation_functions.R')


#THIS IS THE STARTING FOR EVERY CALCULATION MODULE
#EITHER GNB, GASEOUS LOSSES, RUNOFF OR LEACHING

##################################
#============== RUN =============#
##################################

lol <- set_maindata_folder()

gnb_data <- select_maindata_pattern('GNB')
print_loop_files(gnb_data)

store_file_gnb <- store_folder_files(gnb_data)

db99 <- disaggregate_year(store_file_gnb, 1999)
db09 <- disaggregate_year(store_file_gnb, 2009)

db_file99 <- read_disagg_files(db99, gnb_data, 1999)
db_file09 <- read_disagg_files(db09, gnb_data, 2009)

irrig_db_file99 <- populate_gng_input_irrig(1999, get_irrigatioN_gnb, db_file99)
irrig_db_file09 <- populate_gng_input_irrig(2009, get_irrigatioN_gnb, db_file09)

#writing irrigatioN to GNB activity data
write_irrig09 <- replace_gnb_input_irrig(irrig_db_file09, 2009)
write_irrig99 <- replace_gnb_input_irrig(irrig_db_file99, 1999)
