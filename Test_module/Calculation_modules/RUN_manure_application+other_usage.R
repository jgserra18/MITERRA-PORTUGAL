source('./Manure_application_module/Manure_app_function.R')
source('./Gaseous_functions.R')



#write read_me file
read_me()

#============== start the calculation ======================#
#===========================================================#

lol <- set_maindata_folder()

#Activity data folder for manure application
man_app_data_path <- select_maindata_pattern('Manure')
man_app_files <- store_folder_files(man_app_data_path)

#disaggregate by year
man_app99 <- disaggregate_year(man_app_files, 1999)
man_app09 <- disaggregate_year(man_app_files, 2009)

#read these files and then convert them to dataframe
r_man_app99 <- read_disagg_files(man_app99, man_app_data_path, 1999)
r_man_app09 <- read_disagg_files(man_app09, man_app_data_path, 2009)

df_man99 <- as.data.frame(r_man_app99)
df_man09 <- as.data.frame(r_man_app09)

#read manure application rates
app_rates <- read.csv(man_app_files[3])

#Calculate manure applied to soil
soil_app99 <- compute_soil_app(df_man99)
soil_app09 <- compute_soil_app(df_man09)

#Calculate other manure usage
#other manure usage
oth_man99 <- compute_manure_usage(df_man99, 1999)
oth_man09 <- compute_manure_usage(df_man09, 2009)

#====================== WRITE OUTPUTS ==========================#
#OTHER MANURE USAGE
write_output('SSNB', oth_man09, 'other_manure_usage09')
write_output('SSNB', oth_man99, 'other_manure_usage99')

#MANURE APPLIED TO THE SOIL
export_to_activity('Run', 'Application', soil_app09, 'Manure_soil09')
export_to_activity('Run', 'Application', soil_app99, 'Manure_soil99')

write_output('Manure', soil_app09, 'Manure_soil09')
write_output('Manure', soil_app99, 'Manure_soil99')
