
#Import Get_data functions
source('Get_data.R')

#==================================================================================#
####################################################################################
######################### RUN V_0 CODE SIMULATION ##################################

#1 Main directory definition
path <- define_main_path()

#2 See all activity data modules
all_modules <- all_datamodules(path)

#3 select which module by using a pattern
all_folders <- read_folders(path, 'GNB')

#4 list all subfolders within the chosen module
file_folder <- list_subfolders(all_folders)
print_subfolders <- print(list_subfolders(all_folders))
#4 alternatively
print_subfoldersv2 <- loop_files(folder_to_loop = list_subfolders(all_folders))

#===========================================#
#5 store files in vector for N-NH3
#nh3_read_subfolders(all_folders, 'Storage')
#===========================================#
#5 store files in vector
print_return_datafiles(all_folders)
input09 <- print_return_datafiles(all_folders)[1]

#6 collate data by year #.csv files
year_99 <- data_by_year(file_folder, 1999, 'print')
year_09 <- data_by_year(file_folder, 2009, 'print')
# Access data
year_99_acc <-  data_by_year(file_folder, 1999)

#read individual file
year_99_inp <- select_data(all_folders, year_99_acc, 1)


nh3 <- read_folders(main_path, 'GNB')
list_subfolders(nh3)
xd <- read_subfolders(main_path, 'GNB')
ab <- print_return_datafiles(folder_file_path = nh3)[1]



data_by_year(xd, 1999)
d <- select_data(sub_folder = return_subfolder(nh3, 'Manure_housing'),
                 subfolder_files = xd, index = 1)
