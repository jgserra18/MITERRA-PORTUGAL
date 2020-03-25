library(rjson)


get_INE_data <- function(INE_param_id, year, muni_id, var_id) {
  
  # convert to json file
  test <- paste0('https:/www.ine.pt/ine/json_indicador/pindica.jsp?op=2&varcd=', INE_param_id, 
                 '&Dim1=', year, '&Dim2=', 
                 muni_id, '&Dim3=', 
                 var_id, '&lang=PT')
  test <- gsub('/', '//', test)
  # test <- getURL(URLencode(test))
  d <- jsonlite::fromJSON(test)
  return(d[[7]][[1]][[1]][5])
}


get_agrarian_region_INE <- function(INE_param_id, var_id) {
  
  #year <- seq(1987, 2017)
  year <- 2018
  muni_id <- as.character(seq(1,7))
  df <- data.frame()
  
  for (i in seq_along(muni_id)) {
    print(paste0('Agrarian region: ', i))
    df[i, 'id'] <- i
    for (j in year) {
      print(paste0('Year: ', j))
      js_year <- paste0('S7A', j)
      df[i, as.character(j)] <- get_INE_data(INE_param_id, js_year, i, var_id)
    }
  }
  return(df)
}

