
#===========================================================================================#
################## CALCULATES N-N2O EMISSIONS FROM GRAZING PER ANIMAL CLASS #################

#load used submodules
source('./Main_functions.R')
source('./Gaseous_functions.R')


#define EFs according to grazing animals
#unit: kg N-N2O kg N applied-1
#IPCC (2006)

#cattle, poultry, pigs
ef_1 <- 0.02 

#sheep, goats, other animals
ef_2 <- 0.01

#returns grazing activity data path
grazing_path <- function()
{
	path <-  path <- select_maindata_pattern('Grazing')
	return(path)

}

#loads the path of grazing data
load_grazing <- function()
{
  #see rounding noumbers
  path <- grazing_path()
  store_files <- store_folder_files(path)

  return(store_files)
}

#disaggregates according to specified year
load_grazing_yr <- function(year)
{
	db_grazing <- load_grazing()
	disag_yr_files <- disaggregate_year(db_grazing, year)

	return(disag_yr_files)
}

#reads csv file according to specified year

read_grazing_yr <- function(year)
{
	graz_db_yr <- load_grazing_yr(year)
	path <- grazing_path()

	file_read <- read_disagg_files(graz_db_yr, path, year)[[1]]

	return(file_read)
}

#specifies EF condition
grazing_animal_condition <- function()
{
	group_ef_1 <- c('dairy', 'horses', 'other_cattle', 'poultry')

	return(group_ef_1)
}


disagg_animal_class <- function(year, cond)
{
	graz_file <- read_grazing_yr(year)

	if (cond==T)
	{
		cond <- grazing_animal_condition()
		#subset based on condition
		cond_id <- graz_file[which(colnames(graz_file) %in% cond==TRUE)]

		return(cond_id)
	}
	else 
	{
		cond <- c('sheep', 'goat', 'pigs')
		other_ids <- which(colnames(graz_file) %in% cond)
		other_ids <- graz_file[other_ids] 

		return(other_ids)

	}
}

#fix this
#computes N-N2O emissions from grazing according to specified condition
compute_n2o_graz <- function(year)
{
	df <- create_main_csv()

	cond <- c(TRUE, FALSE)

	for (i in cond)
	{
		main_df <- disagg_animal_class(year, i)

		ifelse(i==TRUE,
			ef <- ef_1,
			ef <- ef_2)
		
		main_df <- main_df*ef
		df <- cbind(df, main_df)
	}

	df$graz_tot <- rowSums(df[, 4:ncol(df)])

	return(df)
}

#computes N-N2O/ha
compute_n2o_ha <- function(year)
{
	n2o_df <- compute_n2o_graz(year)
	uaa <- load_uaa(year)

	#n2o_ha <- n2o_df[, 4:ncol(n2o_df)]/uaa #alternative 1withou 3 cols
	for (i in 4:ncol(n2o_df))
	{
		n2o_df[i] <- n2o_df[i]/uaa
	}

	return(n2o_df)
}

#exports this to activity data
export_n2O <- function(file, name)
{
	export_to_activity('N2O', 'Application', file, name)

}