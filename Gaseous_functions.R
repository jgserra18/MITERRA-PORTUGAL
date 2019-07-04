######################### MAIN GASEOUS FUNCTIONS ###############################3

#LIBARIES
library(data.table)

source('./Main_functions.R')

#note: this can be optimized
#prints all the sources of the specified gaseous loss
print_gaseous_sources <- function()
{
  nh3 <- cat('NH3:\n(i) Application: sludge, manure, fertiliser, grazing.\n\n(ii) Manure_housing.\n\n(iii) Manure_storage\n')
  n2o <-  cat('N2O:\n(i) Application: sludge, manure, fertiliser, irrigation, grazing, crop residues.\n\n(ii) Manure_storage.\n')
  nox <-  cat('NOx:\n(i) Application: sludge, manure, fertiliser, grazing.\n\n(ii) Manure_storage.\n')
  
  print_me <- (paste(nh3, n2o, nox))
  return(print_me)
}

#allows to specify sub-folders of the gaseous N losses (application, storage, housing)
#needs to be called instead of simply select_maindata_source
gaseous_select_source <- function(pattern, path, source)
{
  gaseous_path <- select_maindata_pattern(pattern, path)
  gaseous_path <- check_folder(gaseous_path) #added this, if error due to this
  
  store_gaseous <- as.character(source)
  
  for (i in 1:length(store_gaseous))
  {
    select_source <- list.files(gaseous_path, pattern = store_gaseous[i])
    select_source <- check_folder(select_source)
    store_gaseous[i] <- paste0(gaseous_path, select_source)
  }
  
  return(store_gaseous)
}

#this can be improved
#Note: this sub-function is called in gaseous_store_filepath
#this a sub-function that creates a list with the 3 major pathway losses
#Within each list_index the fullpath of the said pathway is stored
#can beaccessed as cond[[i]] #C:\(...)\\Application
gaseous_main_fullpath <- function(gaseous_source, nh3)
{
  print('This can never be higher than 3: application, storage, housing.')
  if (nh3==T)
  {
    cond <- vector(mode='list', length = 3)
    names(cond) <- c('Application', 'storage', 'housing')
  }
  else if (nh3==F)
  {
    cond <- vector(mode='list', length = 2)
    names(cond) <- c('Application', 'storage')
  }

  for (i in 1:length(names(cond)))
  {
    for (j in 1:length(gaseous_source))
    {
      if (grepl(names(cond[i]), gaseous_source[j])==T)
      {
        cond[[i]] <- gaseous_source[j]
      }
    }
  }
  return(cond)

}

#this function stores in a list all the full_path for each spreadsheet within each major pathway
#can be accessed similarly to gaseous_main_fullpath but it will contain the fullpath of each .csv file
gaseous_store_filepath <- function(gaseous_source, nh3)
{
  list_db <- gaseous_main_fullpath(gaseous_source, nh3)

  ifelse(nh3==T,
    path_name <- vector(mode='list', length=3),
    path_name <- vector(mode='list', length=2))

  names(path_name) <- names(list_db)

  for (i in 1:length(list_db))
  {
    path_name[[i]] <- store_folder_files(list_db[[i]])
  }

  return(path_name)
  }

#disaggregate list with the fullpath of each spreadhseet per pathway loss
#into the specified year
gaseous_disagg_list_yr <- function(gaseous_source, year, nh3)
{
  #call gaseous_store_filepath with the main_db list
  main_db_list <- gaseous_store_filepath(gaseous_source, nh3)

  for (i in 1:length(main_db_list))
  {
    main_db_list[[i]] <- disaggregate_year(main_db_list[[i]], year)
  }
  print('')
  print(paste0('Disaggregated to the year ', year))
  print('')
  return(main_db_list)
}

#subfucntion of tier_identifier
#includes grazing, as it does not have any tier emission (well by default it is tier 1)
incl_graz <- function(df)
{
	graz_path <- sapply(df['Application'], function(x) df[[1]][which(grepl('graz', x)==TRUE)])

	return(graz_path[[1]])
}

#tier must be either tier1 or tier 2
tier_identifier_n2o_application <- function(application_db, tier, incl_graz)
{
	main_df <- application_db

	store <- c()
	ctr <- 1

	for (i in 1:length(main_df[[1]]))
	{
		if (grepl(tier, main_df[[1]][i])==TRUE)
		{
			store[ctr] <-  main_df[[1]][i]
			ctr <- ctr + 1 #this is here as we want the real positions of ['App']

		}
	}

	if (incl_graz==TRUE)
	{
		store[ctr] <- incl_graz(main_df)
	}

	return(store)
}

#this merges all the major sources of gaseous losses after application of N sources
gaseous_merge_application <- function(list_db_yr, tier)
{
  app_id <- list_db_yr['Application']
  main_file <- create_main_csv()

  if (missing(tier)==TRUE)
  {
	  for (i in 1:lengths(app_id))
	  {
	    file <- select_ncol(app_id[[1]][i])
	    main_file <- cbind(main_file, file[[1]])
	    colnames(main_file)[ncol(main_file)] <- file[[2]]
	  }

   }

   else 
   {
   		app_id <- tier_identifier_n2o_application(app_id, tier, TRUE)

   		for (i in 1:length(app_id))
   		{
   			file <- select_ncol(app_id[i])
	    	main_file <- cbind(main_file, file[[1]])
	    	colnames(main_file)[ncol(main_file)] <- file[[2]]
   		}
   }

  return(main_file)
}

#note: can be further improved; a bit hardcoded
#this  merges all the gaseous losses into a main dataframe
nh3_merge_all <- function(merged_app, list_db_yr)
{
  main_file <- merged_app
  gas_hous <- select_ncol(list_db_yr['housing'][[1]])
  gas_stor <- select_ncol(list_db_yr['storage'][[1]])

  store <- list(gas_hous, gas_stor)

  for (i in 1:length(store))
  {
    df <- store[[i]][[1]]
    col_name <- store[[i]][[2]]

    main_file <- cbind(main_file, df)
    colnames(main_file)[ncol(main_file)] <- col_name
  }

  return(main_file)
}

gaseous_merge_all <- function(merged_app, list_db_yr)
{
  main_file <- merged_app
  gas_stor <- select_ncol(list_db_yr['storage'][[1]])

  df <- gas_stor[[1]]
  col_name <- gas_stor[[2]]

  main_file <- cbind(main_file, df)
  colnames(main_file)[ncol(main_file)] <- col_name

  return(main_file)

}


#this computes the total losses of a particupar file, starting from 4 to ncol
#can be applied to merged_all or merged_application
gaseous_compute_all <- function(merged_all)
{
  computing_file <- merged_all

  if (class(computing_file) != 'data.frame')
  {
    print('Please convert this to a dataframe.')
  }
  else
  {
    total_sum <- rowSums(computing_file[, 4:ncol(computing_file)])
    computing_file <- cbind(computing_file, total_sum)
    colnames(computing_file)[ncol(computing_file)] <- 'total_sum'
  }

  return(computing_file)

}

#this modifies the main function "write_output" according to gaseous losses
write_output_gaseous <- function(data_to_write, name, pattern2)
{
  write_output("Gaseous_N_losses", data_to_write, name, pattern2)

}

#exports gaseous N losses (in form of any emission) to the respectivr activity data
#this is then compiled (in other function) in main datasets
export_to_activity <- function(gas, source, file, name)
{
  no_path <- select_maindata_pattern(pattern=gas)
  no_path <- check_folder(no_path)

  full_path <- check_folder(paste0(no_path, source))
  write_path <- paste0(full_path, name, '.csv')
  fwrite(file, write_path)
}

#applied in N2O emissions
#this either filters or not irrigation N2O application losses
#data is the output of gaseous_disagg_list_yr
select_irrigation_n2o_module <- function(data, mode)
{
  data_application <- data[[1]]

  if (mode=='OFF')
  {
      id_irrig <- grepl('irrig', data)
      id_condition <- which(id_irrig==FALSE) #this excludes irrigation
      select_data <- data[id_condition] #selects the standard gaseous application N losses

      return(select_data)
  }
  else if (mode=='ON')
  {
    return(data)
  }
}

#this function checks if colnames of an application dataset contains irrigation
#if true, it modifies filename to be written
correct_filename_irrigation_n2o <- function(df)
{
  cols <- colnames(df)
  id <- grepl('irrig', cols)
  condition <- ((TRUE %in% id)==TRUE)

  ifelse(condition==FALSE,
        filename <- '',
        filename <- '_irrig')
  return(filename)
}