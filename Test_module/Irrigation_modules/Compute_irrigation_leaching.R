source('./Leaching_module/Function/compute_irrigation_leaching.R')

years <- c(1999, 2009)

write_irrig_leaching <- sapply(years, function(x) compute_irrigation_leaching(x, TRUE))

