source('./GIS_module/Function/General_GIS_functions.R')
source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')

gw_within_HUs <- function()
{
  gw <- load_shp('gw')
  colnames(gw@data)[ncol(gw)] <- 'aquifer_ID'
  hydro <- load_shp('main_hydro')
  hydro <- spTransform(hydro, proj4string(gw))
  
  ids <- unique(hydro$nome)
  df <- data.frame(gw_ids = as.data.frame(gw$GW_ID))
  
  for (i in ids)
  {
    sb_hydro <- subset(hydro, nome==i)
    crop_gw_hydro <- crop(gw, sb_hydro)
    gw_ids <- crop_gw_hydro$GW_ID
    df[gw_ids, 2] <- as.character(i)
  }
  return(df)
}

export_HU <- function(file)
{
  path <- select_maindata_pattern('Hydrogeological')
  write.csv(x = file, file = paste0(path,'/gw_HUs.csv'))
}

d <- gw_within_HUs()
export_HU(d)
