
source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")
source("/Users/lavieestuntoucan/civ-sto/tech/correspondance/plots/R/include.R")
##
corr.sto.oct <- "/Users/lavieestuntoucan/civ-sto/tech/correspondance/out/sto-vers-oct/corr.sto.oct_complete.rds"
d.sto.oct <- readRDS(corr.sto.oct)
d.sto.oct <- d.sto.oct[complete.cases(d.sto.oct), ]
##
corr.oct.sto <- "/Users/lavieestuntoucan/civ-sto/tech/correspondance/out/oct-vers-sto/corr.oct.sto.rds"
d.oct.sto <- readRDS(corr.oct.sto)
d.oct.sto <- d.oct.sto[complete.cases(d.oct.sto), ]
##
gtfs.oct <- "/Users/lavieestuntoucan/civ-sto/data/GTFS/octranspo/"
gtfs.sto <- "/Users/lavieestuntoucan/civ-sto/data/GTFS/sto/jan2017-july2017/"
stops.oct <<- fread(paste0(gtfs.oct,"stops.txt"))
stops.sto <<- fread(paste0(gtfs.sto,"stops.txt"))

################################
## OCT -> STO
corr.stop.sto <- get.stops.sto(d.oct.sto)
corr.stop.oct <- get.stops.oct(d.oct.sto)

## Plot the map
map1 <- maps.corr(corr.stop1=corr.stop.oct, corr.stop2=corr.stop.sto, map.city, "24h")

## Create the matrix of transferts for 24h
create.matrix(d.oct.sto)

################################
## STO(A) -> OCT(B)
corr.stop.sto <- get.stops.sto(d.sto.oct)
corr.stop.oct <- get.stops.oct(d.sto.oct)

## Plot the map
map1 <- maps.corr(corr.stop1=corr.stop.sto, corr.stop2=corr.stop.oct, map.city, "24h")

## Create the matrix of transferts for 24h
create.matrix(d.sto.oct)

#################################

## STO -> OCT
## First higher transfert rate
stopB.1 <- corr.stop.oct %>% arrange(desc(freq)) %>% slice(1) %>% select(StopB) %>% as.character()
stopB.2 <- corr.stop.oct %>% arrange(desc(freq)) %>% slice(2) %>% select(StopB) %>% as.character()
stopB.3 <- corr.stop.oct %>% arrange(desc(freq)) %>% slice(3) %>% select(StopB) %>% as.character()
df.1 <- d.sto.oct %>% filter(StopB == stopB.1) %>% group_by(titre) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
df.2 <- d.sto.oct %>% filter(StopB == stopB.2) %>% group_by(titre) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
df.3 <- d.sto.oct %>% filter(StopB == stopB.3) %>% group_by(titre) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
p1 <- plot_ly(data = df.1 , x = ~titre, y = ~round(freq*100), type="bar", name="1er arrêt OCT") 
p2 <- plot_ly(data = df.2 , x = ~titre, y = ~round(freq*100), type="bar", name="2eme arrêt OCT") 
p3 <- plot_ly(data = df.3 , x = ~titre, y = ~round(freq*100), type="bar", name="3eme arrêt OCT") 

m <- list(
  l = 0.1,
  r = 0.5,
  b = 0.5,
  t = 0.1
)
p <- plotly::subplot(p1, p2, p3, nrows = 1) %>%
  layout(autosize = T,yaxis=list(title="Pourcentage (%)"))
p

## Ex. of input data
# NumCarte       date HeureA LigneA StopA HeureB StopB    TripB
# 1 1172607443740544 2017-01-19    6.7     37  2122    7.0 CJ903 44561910
# 2 1290071719948416 2017-01-26    7.2     33  2011    7.8 CK210 44556070
# 3 1220582099461760 2017-01-17    7.7     27  2179    8.6 CB910 44561845
# 4 1277974670494336 2017-01-19    9.6     55  1358   10.1 CB910 44557260
# 5 1186984711435648 2017-01-12    8.5     85  2714    8.9 CB910 44561737
# 6 1250753167772288 2017-01-12    8.1     47  1203    9.2 CD140 44558965

