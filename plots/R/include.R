###########################################
## For the map
coord <<- data.frame(lon=-75.696573,lat=45.415917)
map.city <<- leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% setView(coord$lon, coord$lat, zoom = 11)
###########################################
get.stops.sto <- function(x){
  y <- x %>% 
    group_by(StopA) %>% 
    summarise (n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    left_join(stops.sto,by=c("StopA"="stop_id")) %>% 
    select(c(StopA,n,freq,stop_name,stop_lat,stop_lon))
  return(y)
}
###########################################
get.stops.oct <- function(x){
  y <- x %>% 
    group_by(StopB) %>% 
    summarise (n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    left_join(stops.oct,by=c("StopB"="stop_id")) %>% 
    select(c(StopB,n,freq,stop_name,stop_lat,stop_lon))
  return(y)
}
###########################################
pal.col <- function(x){
  if( x >= 0 & x < 5 ) return("black")
  if( x >= 5 & x < 10 ) return("blue")
  if( x >= 10 & x < 15 ) return("yellow")
  if( x >= 15 & x < 20 ) return("orange")
  if( x >= 20 ) return("red")
}
###########################################
## Transferts 1->2
maps.corr <- function(corr.stop1, corr.stop2, map0, groupName){
  ## Stop1 : in, Stop2 : out (transfert)
  corr.stop1 <- corr.stop1 %>% rowwise() %>% mutate(col = pal.col(100*freq))
  corr.stop2 <- corr.stop2 %>% filter(freq > 0.05 ) %>% rowwise() %>% mutate(col = pal.col(100*freq))
  
  map1 <- addCircles(map0,
                     corr.stop1$stop_lon,
                     corr.stop1$stop_lat,
                     fillColor = corr.stop1$col,
                     color = "blue",
                     stroke=T,
                     radius=25,
                     label=paste0(corr.stop1$stop_name," (",round(100*corr.stop1$freq,digits=1),"%)"),
                     highlightOptions = highlightOptions(fillColor = "white",stroke=T),
                     fillOpacity = 0.8,
                     group = groupName)
  
  map1 <- addCircles(map1,
                     corr.stop2$stop_lon,
                     corr.stop2$stop_lat,
                     #fillColor = corr.stop2$col,
                     fillColor = "red",
                     stroke=F,
                     radius=800 * corr.stop2$freq,
                     label=paste0(corr.stop2$stop_name," (",round(100*corr.stop2$freq,digits=1),"%)"),
                     highlightOptions = highlightOptions(fillColor = "white",stroke=F),
                     fillOpacity = 0.6,
                     group = groupName)
  
  return(map1)
}
###########################################
create.matrix <- function(x){
  
  hastus2lines <- "/Users/lavieestuntoucan/Documents/projets_perso/Start-up/Civilia/projets/STOttawa/data/cartes_a_puces/octranspo/2017_Trip_IDs_Table.xlsx"
  hastus2lines <- read.xlsx(hastus2lines,sheet=1,colNames =T)
  
  df <- x %>% left_join(hastus2lines,by=c("TripB" = "Internal.trp.number")) 
  
  lines.oct.now <- c(4,5,6,7,9,11,12,14,16,19,22,30,33,34,
                     38,44,61,62,63,64,80,85,86,87,91,94,
                     95,97,98,99,106,221,222,224,228,231,
                     232,233,234,235,237,252,256,261,262,
                     265,267,268,269,270,271,272,273,277,282,283,293)
  n.oct <- length(lines.oct.now)
  lines.sto.now <- c(22,25,26,29,40,41,44,45,46,47,48,11,17,
                     27,85,88,93,94,95,98,87,20,31,32,33,23,
                     24,34,35,36,37,38,55,59,67,200,400,300)
  n.sto <- length(lines.sto.now)
  mat <- matrix(nrow = n.sto, ncol = n.oct)
  mat <- as.data.frame(mat)
  colnames(mat) <- lines.oct.now
  rownames(mat) <- lines.sto.now
  
  for( sto in 1:n.sto){
    for( oct in 1:n.oct){
      n <- df %>% filter(LigneA == lines.sto.now[sto] & Route == lines.oct.now[oct]) %>% nrow()
      n <- n * 0.111 / 0.80
      # if( data.size == "10k" ) n <- n / ( 0.80 * 0.336 )
      # if( data.size == "full" ) n <- n * 0.111 / 0.80
      mat[sto,oct] <- round(n)
    }
  }
  write.table(mat,"~/Desktop/temp.csv",quote = F,row.names = F,col.names = F,sep=",")
}

