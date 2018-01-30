#####################################################
## User input data
set.user.data <- function(){
  
  chipCards.oct <<- "in/cartes_a_puces/octranspo/sto_20171026100652.csv.RDS"
  chipCards.sto <<- "in/cartes_a_puces/sto/destination_estimee/Deplacement2017JanFev.CSV.RDS"
  
  routes.sto <<- c( 11, 17, 22, 23, 24, 25, 26, 27, 29, 31, 
                    32, 33, 34, 35, 36, 37, 38, 40, 41, 44, 
                    45, 46, 47, 55, 59, 67, 87, 85, 82, 94, 
                    98, 93, 95, 300, 400, 200, 20 )
  
  my.week  <<- c("Tuesday","Wednesday","Thursday")
  my.week.tag <<- "Semaine"
  output.dir <<- "/home/xprudent/civilia/STO/correspondance-oct-sto/out/"
  first.day <<- as.Date("2017-01-09")
  last.day <<- as.Date("2017-01-27")
  
  oct.lat.bot <- 45.410527
  oct.lon.bot <- -75.722194
  oct.lat.top <- 45.442935
  oct.lon.top <- -75.665374
  gtfs.oct <<- "in/GTFS/octranspo/"
  gtfs.sto <<- "in/GTFS/sto/jan2017-july2017/"
}
#####################################################
read.cards <- function(){
  cards.oct <<- readRDS(chipCards.oct)
  cards.sto <<- readRDS(chipCards.sto)
  ## To data.table
  setDT(cards.oct,key=c("date","FarecardID"))
  setDT(cards.sto,key=c("DateDeplacement","NumCarteSerie"))
  
  ## To skim the OCT data
  # chipCards.oct <<- "/Users/lavieestuntoucan/civ-sto/data/cartes_a_puces/octranspo/sto_20171026100652.csv"
  # cards.oct <<- fread(chipCards.oct,colClasses=c(FarecardID="character"),header=TRUE)
  # cards.oct <<- cards.oct %>% filter(Year == 2017 & Month <= 3)
  # cards.oct <<- cards.oct %>% mutate(date = as.Date(paste(Year,Month,Day,sep="-")))
  # p <- plot_ly(data = cards.oct , x = ~date, type = "histogram") %>%
  #   layout(xaxis=list(type="date", tickformat="%d/%m", title="Date du déplacement"),
  #          yaxis=list(title="Décompte"))
  # htmlwidgets::saveWidget(p, paste0(output.dir,"frequentation_jours_OCT.html"))
  # cards.oct <<- cards.oct %>% filter( date >= first.day & date <= last.day )
  # cards.oct <<- cards.oct %>% mutate(weekday = weekdays(date))
  # cards.oct <<- cards.oct %>% filter( weekday %in% my.week )
  # cards.oct <<- cards.oct %>% separate(col=Time,sep=":",into=c("Hre","Mins","Sec")) %>%
  #   mutate(Time = round(as.numeric(Hre) + as.numeric(Mins)/60,digits=1)) %>% select(-c(Hre,Mins,Sec))
  ##
  # stops.oct <<- fread(paste0(gtfs.oct,"stops.txt"))
  # stops.oct <<- stops.oct %>% filter( stop_lat >= oct.lat.bot & 
  #                              stop_lat <= oct.lat.top & 
  #                              stop_lon >= oct.lon.bot & 
  #                              stop_lon <= oct.lon.top )
  #  coord <<- data.frame(lon=-75.696573,lat=45.415917)
  # map.city <- leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% 
  #   setView(coord$lon, coord$lat, zoom = 11)
  # map1 <- addCircles(map.city,stops.oct$stop_lon,stops.oct$stop_lat) 
  # cards.oct <<- cards.oct %>% filter(BusStopId %in% stops.oct$stop_id)
  # saveRDS(cards.oct,paste0(chipCards.oct,".RDS"))
  ##        
  ## To skim the STO data
  # chipCards.sto <<- "/Users/lavieestuntoucan/civ-sto/data/cartes_a_puces/sto/destination_estimee/Deplacement2017JanFev.CSV"
  # cards.sto <<- fread(chipCards.sto,sep="~",colClasses=c(NumCarteSerie="character"),header=TRUE)
  # merge.trips.info()
  # cards.sto <<- cards.sto %>% filter(DateDeplacement >= first.day & DateDeplacement <= last.day)
  # cards.sto <<- cards.sto %>% filter(weekday %in% my.week)
  # cards.sto <<- cards.sto %>% filter(NoLigne %in% routes.sto)
  # saveRDS(cards.sto,paste0(chipCards.sto,".RDS"))
}
#####################################################
## define a helper function
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}
#####################################################
## Merge all trips info
merge.trips.info <- function(){
  ## Change to date and weekdays
  cards.sto <<- cards.sto %>% 
    mutate(DateDeplacement = as.Date(DateDeplacement))
  ##
  DateDeplacement <- c(cards.sto$DateDeplacement,cards.sto$DateDeplacement,cards.sto$DateDeplacement,
                       cards.sto$DateDeplacement,cards.sto$DateDeplacement,cards.sto$DateDeplacement,
                       cards.sto$DateDeplacement,cards.sto$DateDeplacement,cards.sto$DateDeplacement,cards.sto$DateDeplacement) 
  NumCarteSerie <- c(cards.sto$NumCarteSerie,cards.sto$NumCarteSerie,cards.sto$NumCarteSerie,
                     cards.sto$NumCarteSerie,cards.sto$NumCarteSerie,cards.sto$NumCarteSerie,
                     cards.sto$NumCarteSerie,cards.sto$NumCarteSerie,cards.sto$NumCarteSerie,cards.sto$NumCarteSerie) 
  LibelTitre <- c(cards.sto$LibelTitre,cards.sto$LibelTitre,cards.sto$LibelTitre,
                  cards.sto$LibelTitre,cards.sto$LibelTitre,cards.sto$LibelTitre,
                  cards.sto$LibelTitre,cards.sto$LibelTitre,cards.sto$LibelTitre,cards.sto$LibelTitre) 
  ## transform all columns to NA if empty
  cards.sto <<- cards.sto %>% mutate_all(funs(empty_as_na)) 
  ## append all trips
  HeureTransaction <- c(cards.sto$HeureTransaction,cards.sto$HeureTransaction2,cards.sto$HeureTransaction3,
                        cards.sto$HeureTransaction4,cards.sto$HeureTransaction5,cards.sto$HeureTransaction6,
                        cards.sto$HeureTransaction7,cards.sto$HeureTransaction8,cards.sto$HeureTransaction9,cards.sto$HeureTransaction10)
  NoLigne <- c(cards.sto$NoLigne1,cards.sto$NoLigne2,cards.sto$NoLigne3,
               cards.sto$NoLigne4,cards.sto$NoLigne5,cards.sto$NoLigne6,
               cards.sto$NoLigne7,cards.sto$NoLigne8,cards.sto$NoLigne9,cards.sto$NoLigne10)
  NumArret <- c(cards.sto$NumArret1,cards.sto$NumArret2,cards.sto$NumArret3,
                cards.sto$NumArret4,cards.sto$NumArret5,cards.sto$NumArret6,
                cards.sto$NumArret7,cards.sto$NumArret8,cards.sto$NumArret9,cards.sto$NumArret10)
  NumArretDebarquement <- c(cards.sto$NumArretDebarquement1,cards.sto$NumArretDebarquement2,cards.sto$NumArretDebarquement3,
                            cards.sto$NumArretDebarquement4,cards.sto$NumArretDebarquement5,cards.sto$NumArretDebarquement6,
                            cards.sto$NumArretDebarquement7,cards.sto$NumArretDebarquement8,cards.sto$NumArretDebarquement9,cards.sto$NumArretDebarquement10)
  
  ## Make de dataframe
  cards.sto <<- data.frame(NumCarteSerie,LibelTitre,DateDeplacement,HeureTransaction,NoLigne,NumArret,NumArretDebarquement)
  #cards.sto <<- cards.sto %>% filter(!is.na(NumArretDebarquement))
  ## Hour
  cards.sto <<- cards.sto %>% 
    separate(col=HeureTransaction, sep=":", into=c("Hres","Mins","Secs")) %>% 
    mutate(weekday=weekdays(DateDeplacement)) %>%
    mutate(HeureTransaction = round(as.numeric(Hres) + as.numeric(Mins)/60,digits=1)) %>% select(-c(Hres,Mins,Secs))
}
#####################################################
read.gtfs <- function(){
  stops.oct <<- fread(paste0(gtfs.oct,"stops.txt"))
}
#####################################################
loop.cards.sto2oct <- function(d){
  d.out <- d %>% rowwise() %>% do(search.corr.sto2oct(.))
  return(d.out)
}
loop.cards.oct2sto <- function(d){
  d.out <- d %>% rowwise() %>% do(search.corr.oct2sto(.))
  return(d.out)
}
#####################################################
## Search for the transferts STO -> OCT
search.corr.sto2oct <- function(d){
  d.oct <- cards.oct[date == d$DateDeplacement & 
                       FarecardID == d$NumCarteSerie & 
                       Time > d$HeureTransaction & 
                       Time - d$HeureTransaction < 1.5]
  if( nrow(d.oct) > 1 ){
    d.oct <- d.oct %>% arrange(Time) %>% head(1)
  }
  d.out <- data.frame(NumCarte=NA,titre=NA,date=NA,HeureA=NA,LigneA=NA,StopA=NA,HeureB=NA,StopB=NA,TripB=NA)
  if( nrow(d.oct) == 0 ) return(d.out)
  else {
    d.out$NumCarte <- as.character(d$NumCarteSerie)
    d.out$titre <- as.character(d$LibelTitre)
    d.out$date <- as.Date(d$DateDeplacement)
    d.out$HeureA <- as.numeric(d$HeureTransaction)
    d.out$LigneA <- as.character(d$NoLigne)
    d.out$StopA <- as.character(d$NumArret)
    d.out$HeureB <- as.numeric(d.oct$Time)
    d.out$StopB <- as.character(d.oct$BusStopId)
    d.out$TripB <- as.numeric(d.oct$HastusTripId)
  }
  return(d.out)
}
#####################################################
## Search for the transferts OCT -> STO
search.corr.oct2sto <- function(d){
  
  d.sto <- cards.sto[DateDeplacement == d$date & NumCarteSerie == d$FarecardID & HeureTransaction > d$Time & HeureTransaction - d$Time < 1.5]
  if( nrow(d.sto) > 1 ){
    d.sto <- d.sto %>% arrange(HeureTransaction) %>% head(1)
  }
  d.out <- data.frame(NumCarte=NA,titre=NA,date=NA,HeureA=NA,LigneA=NA,StopA=NA,HeureB=NA,StopB=NA,TripB=NA)
  if( nrow(d.sto) == 0 ){
    return(d.out)
  }
  else {
    d.out$NumCarte <- as.character(d.sto$NumCarteSerie)
    d.out$titre <- as.character(d$TrxTypeName)
    d.out$date <- as.Date(d.sto$DateDeplacement)
    d.out$HeureA <- as.numeric(d.sto$HeureTransaction)
    d.out$LigneA <- as.character(d.sto$NoLigne)
    d.out$StopA <- as.character(d.sto$NumArret)
    d.out$HeureB <- as.numeric(d$Time)
    d.out$StopB <- as.character(d$BusStopId)
    d.out$TripB <- as.numeric(d$HastusTripId)
  }
  return(d.out)
}
