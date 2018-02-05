source("load_R_pkg.R")

###########################################
## Coordinates
coord <- data.frame(lon=-75.696573,lat=45.415917)
map.city <- leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% setView(coord$lon, coord$lat, zoom = 11)
##
## GTFS
#stops <- fread("in/GTFS/octranspo/gtfs_oct_printemps2018/stops.txt")
stops <- fread("in/GTFS/sto/july2016-aug2017/stops.txt")
##
## Map
map1 <- addCircles(map.city,
                   stops$stop_lon,
                   stops$stop_lat,
                   label=paste0(stops$stop_name," (",stops$stop_id,")"),
                   highlightOptions = highlightOptions(fillColor = "white",stroke=T),
                   fillOpacity = 0.8)
##
## Focus
oct.lat.bot <- 45.410527
oct.lon.bot <- -75.722194
oct.lat.top <- 45.442935
oct.lon.top <- -75.665374
stops <- stops %>% filter( stop_lat >= oct.lat.bot &
                             stop_lat <= oct.lat.top &
                             stop_lon >= oct.lon.bot &
                             stop_lon <= oct.lon.top )
sto.lat.bot <- 45.392351
sto.lon.bot <- -75.760260
sto.lat.top <- 45.457524
sto.lon.top <- -75.670803
stops <- stops %>% filter( stop_lat >= sto.lat.bot &
                             stop_lat <= sto.lat.top &
                             stop_lon >= sto.lon.bot &
                             stop_lon <= sto.lon.top )


map2 <- addCircles(map.city,
                   stops$stop_lon,
                   stops$stop_lat,
                   label=paste0(stops$stop_name," (",stops$stop_id,")"),
                   highlightOptions = highlightOptions(fillColor = "white",stroke=T),
                   fillOpacity = 0.8)


