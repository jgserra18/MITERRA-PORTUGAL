d <- Raster map
d <- aggregate(x = d, 10)

mt <- as.data.frame(values(d), na.rm=F)

lol <- data.table::melt(mt)

ggplot(lol, aes(value)) + 
  geom_density(color='black', fill='lightblue')