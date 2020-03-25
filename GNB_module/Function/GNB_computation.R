source('./Main_functions.R')


#this function receives as input read_disagg_files and identifies (a list)
#reads the list names
#and returns the id of input and output
#if input first element 1 2, otherwise 2 1
gnb_element_id <- function(db_yr)
{
  element_names <- names(db_yr)
  ifelse(grepl('Input', element_names[1])==T,
         return(c(1,2)),
         return(c(2,1)))
  
}

#sub-function used in gnb_sum_input and output
#creates a new df with only the total value of input and outputs per municipality
gnb_organize <- function(main_df, sum_col, col_name) {	
  df <- cbind(main_df, sum_col)
  df <- subset_data(df, c(1,2,3, ncol(df)))
  colnames(df)[4] <- col_name
  
  return(df)
}


gnb_sum_input <- function(db_yr, irrig) {
  #this receives as sub-function the gnb_element_id, which identifies the id of the db_yr list
  #corresponding to input and output
  #this function specifically focusses on the input
  #converts the input list to db and then sums all the inputs
  #irrigation can be selected or not and it computes the total accordingly
  
  input_id <- gnb_element_id(db_yr)[1] #stores the id of the list with inputs
  input_list <- db_yr[input_id] #opens the input file from db_yr list
  ##convert to dataframe
  input_db <- as.data.frame(input_list, col.names = colnames(input_list))
  
  if (irrig==F){
    sum_input <- rowSums(input_db[, 4:8])
  }
  else if (irrig==T){
    #ifelse(is.na(input_db[, length(input_db)]), 0)
    sum_input <- rowSums(input_db[, 4:9])
  }
  gnb_organize(input_db, sum_input, 'tot_input')
  
}

gnb_sum_output <- function(db_yr) {
  #similar to the previous function but it calculates the total outputs
  
  output_id <- gnb_element_id(db_yr)[2]
  output_list <- db_yr[output_id]
  output_db <- as.data.frame(output_list, col.names = colnames(output_list))
  
  sum_output <- rowSums(output_db[, 4:7])
  gnb_organize(output_db, sum_output, 'tot_output')
  
}


gnb_compute <- function(db_yr, irrig) {
  #compute GNB
  #receives as input the db_yr list, then calls gnb_sum_input and output
  #hese specify the list index with input and output
  #then computes the gnb 
  if (missing(irrig)==TRUE) {
    tot_input <- gnb_sum_input(db_yr, F)
    tot_output <- gnb_sum_output(db_yr)[, 4]
    
    main_df <- cbind(tot_input, tot_output)
    gnb <- main_df[, 4] - main_df[, 5]
    
    main_df$gnb <- as.integer(gnb)
    return(main_df)    
  }
  else {
    tot_input <- gnb_sum_input(db_yr, T)
    tot_output <- gnb_sum_output(db_yr)[, 4]
    
    main_df <- cbind(tot_input, tot_output)
    gnb <- main_df[, 4] - main_df[, 5]
    
    main_df$gnb <- as.integer(gnb)
    return(main_df)    
  }
  
}

#compute nue
#receives as input the output of the gnb_compute
nue_compute <- function(main_gnb_df)
{
  gnb_db <- main_gnb_df
  nue <- gnb_db[, 5]/gnb_db[, 4]*100
  nue[is.na(nue)] <- 0
  gnb_db$nue <- as.integer(nue)
  
  return(gnb_db)
}

#computes the impact of irrigatioN in total inputs, GNB and NUE
compute_irrig_influence <- function(df_wo_irrig, df_w_irrig)
{
  df1 <- df_wo_irrig
  df2 <- df_w_irrig
  main_df <- create_main_csv()
  
  cols <- c('tot_input', 'gnb', 'nue')
  
  #compute impact of irrigatioN
  for (i in cols)
  {
    diff <- df2[, i]-df1[, i]
    main_df <- cbind(main_df, df1[, i], df2[, i], diff) #this allows to establish a baseline values of the differences
    
    colnames(main_df)[ncol(main_df)-2] <- i
    colnames(main_df)[ncol(main_df)-1] <- paste0('irrig_', i)
    colnames(main_df)[ncol(main_df)] <- paste0('diff_', i)
  }

  return(main_df)
}



