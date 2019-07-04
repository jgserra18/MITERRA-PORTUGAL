source('./ExploratoryAnalysis_module/Command function/GIS_functions.R')


surf_bod <- load_shp('surface_water')

d <- readOGR('G:\\O meu disco\\MITERRA\\MITERRA-PORTUGAL\\Activity data\\SurfaceWater_data\\large99_buffer.shp')

surf_bod99 <- subset(surf_bod, !surface_id %in% c(4279, 3382, 2431, 2794))
surf_bod99$area <- rgeos::gArea(surf_bod99, byid=TRUE)/10000
surf_bod99$area <-round(surf_bod99$area, 3)

surf_bod09 <- surf_bod
surf_bod09$area <- rgeos::gArea(surf_bod09, byid=TRUE)/10000
surf_bod09$area <-round(surf_bod09$area, 3)


small_surface99 <- surf_bod99[surf_bod99$area < 50, ]
large_surface99 <-  surf_bod99[surf_bod99$area >= 50, ]

small_surface09 <- surf_bod09[surf_bod09$area < 50, ]
large_surface09 <-  surf_bod09[surf_bod09$area >= 50, ]


#create buffers
buff_small99 <- rgeos::gBuffer(small_surface99, width = 500, byid=TRUE)
buff_large99 <- rgeos::gBuffer(large_surface99, width = 500, byid=TRUE)

buff_small09 <- rgeos::gBuffer(small_surface09, width = 500, byid=TRUE)
buff_large09 <- rgeos::gBuffer(large_surface09, width = 500, byid=TRUE)


#output_path
plot_output <- select_maindata_pattern('SurfaceWater')

writeOGR(buff_small99, dsn=plot_output, layer='small99_buffer', driver='ESRI Shapefile')
writeOGR(buff_large99, dsn=plot_output, layer='large99_buffer', driver='ESRI Shapefile')

writeOGR(buff_small09, dsn=plot_output, layer='small09_buffer', driver='ESRI Shapefile')
writeOGR(buff_large09, dsn=plot_output, layer='large09_buffer', driver='ESRI Shapefile')