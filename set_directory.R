set_dir <- function(dir_path)
{
  if (missing(dir_path)==TRUE)
  {
    path <- readline(prompt="Directory: ")
    path <- as.character(path)
    setwd(path)
  }
  else 
  {
    dir_path <- dir_path
    setwd(dir_path)
  }
}

load_dir <- function()
{
  dir_path <- '/home/serra/grive/MITERRA/MITERRA-PORTUGAL/'
  dir_isa <- 'G:/My Drive/MITERRA/MITERRA-PORTUGAL/'
  dir_home <- 'G:/O meu disco/MITERRA/MITERRA-PORTUGAL/'
  
  store_db <- c(dir_path, dir_isa, dir_home)
  correct_path <- which(dir.exists(store_db)==TRUE)
  
  set_dir(store_db[correct_path])
}

load_dir()
