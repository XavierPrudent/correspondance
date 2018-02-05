#####################################################
## User input data
set.user.data <- function(){
  chipCards.oct <<- "in/cartes_a_puces/octranspo/sto_20171026100652.csv"
  chipCards.sto <<- "in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus.CSV"
  my.week  <<- c("Tuesday","Wednesday","Thursday")
  my.week.tag <<- "Semaine"
  first.day <<- as.Date("2017-01-09")
  last.day <<- as.Date("2017-01-27")
  oct.lat.bot <- 45.410527
  oct.lon.bot <- -75.722194
  oct.lat.top <- 45.442935
  oct.lon.top <- -75.665374
  gtfs.oct <<- "in/GTFS/octranspo/"
  gtfs.sto <<- "in/GTFS/sto/jan2017-july2017/"
  stops_oct_gattineau <<- c("CK101","CK103","CK105","CK110","CK115","CK125","CK130","CK140","CK150","CK155","CK160","CK170","CK175","CK180","CK183","CK190","CK195","CK210","CK220","CK230")
  stops_sto_ottawa <<- c("5000","5003","5004","5014","5016","5022","5024","5025","5030","5032","5034","5040","5042","5048","5050","5063","5080","5081","5082","5086","5085")
}
#####################################################
read.cards <- function(){
  if( 1 < 0 ){
    #########################################
    # OCT data
    cards.oct <- fread(chipCards.oct,colClasses=c(FarecardID="character"),header=TRUE)
    setDT(cards.oct)
    cards.oct <- cards.oct[Year == 2017 & Month == 1]
    cards.oct <- cards.oct %>% mutate(date = as.Date(paste(Year,Month,Day,sep="-")))
    cards.oct <- cards.oct %>% filter(date >= first.day & date <= last.day)
    cards.oct <- cards.oct %>% mutate(weekday = weekdays(date))
    cards.oct <- cards.oct %>% filter(weekday %in% my.week)
    cards.oct <- cards.oct %>% separate(col=Time,sep=":",into=c("Hre","Mins","Sec")) %>%
      mutate(Time = round(as.numeric(Hre) + as.numeric(Mins)/60,digits=1)) %>% select(-c(Hre,Mins,Sec))
    saveRDS(cards.oct,"in/cartes_a_puces/octranspo/sto_20171026100652_9joursSemaine_janvier2017.RDS")
    ## GTFS
    stops.oct <- fread("in/GTFS/octranspo/gtfs_oct_printemps2018/stops.txt")
    ## Focus
    oct.lat.bot <- 45.410527
    oct.lon.bot <- -75.722194
    oct.lat.top <- 45.442935
    oct.lon.top <- -75.665374
    stops.oct <- stops.oct %>% filter( stop_lat >= oct.lat.bot &
                                         stop_lat <= oct.lat.top &
                                         stop_lon >= oct.lon.bot &
                                         stop_lon <= oct.lon.top )
    cards.oct <- cards.oct %>% filter(BusStopId %in% stops.oct$stop_id)
    saveRDS(cards.oct,"in/cartes_a_puces/octranspo/sto_20171026100652_9joursSemaine_janvier2017_focusDowntown.RDS")
    
    #########################################
    # STO data
    cards.sto <- fread(chipCards.sto,sep=";",colClasses=c(NumCarteSerie="character"),header=TRUE)
    cards.sto <- cards.sto %>%
      separate(col=DateService,sep=" ", into=c("DateDeplacement","X")) %>%
      select(-X) %>%
      mutate(DateDeplacement = as.Date(DateDeplacement))
    cards.sto <- cards.sto %>% filter(DateDeplacement >= first.day & DateDeplacement <= last.day)
    cards.sto <- cards.sto %>%
      mutate(weekday=weekdays(DateDeplacement)) %>%
      filter(weekday %in% my.week)
    cards.sto <- cards.sto %>%
      mutate( m = str_sub(HeureTransaction,-4,-3),
              h = str_sub(HeureTransaction,-6,-5) ) %>%
      mutate(m = ifelse(m=="", "00", m),
             h = ifelse(h=="", "00", h)) %>%
      mutate(HeureTransaction = round(as.numeric(h) + as.numeric(m)/60,digits=2)) %>%
      select(-c(h,m))
    saveRDS(cards.sto,"in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus_9joursSemaine_janvier2017.RDS")
    ## GTFS
    stops.sto <- fread("in/GTFS/sto/july2016-aug2017/stops.txt")
    ## Focus
    sto.lat.bot <- 45.392351
    sto.lon.bot <- -75.760260
    sto.lat.top <- 45.457524
    sto.lon.top <- -75.670803
    stops.sto <- stops.sto %>% filter( stop_lat >= sto.lat.bot &
                                         stop_lat <= sto.lat.top &
                                         stop_lon >= sto.lon.bot &
                                         stop_lon <= sto.lon.top )
    cards.sto <- cards.sto %>% filter(NumArret %in% stops.sto$stop_id)
    saveRDS(cards.sto,"in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus_9joursSemaine_janvier2017_focusDowntown.RDS")
  }
  
  cards.oct.full <- readRDS("in/cartes_a_puces/octranspo/sto_20171026100652_9joursSemaine_janvier2017.RDS")
  cards.oct.focus <- readRDS("in/cartes_a_puces/octranspo/sto_20171026100652_9joursSemaine_janvier2017_focusDowntown.RDS")
  cards.sto.full <- readRDS("in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus_9joursSemaine_janvier2017.RDS")
  cards.sto.focus <- readRDS("in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus_9joursSemaine_janvier2017_focusDowntown.RDS")
  
  setDT(cards.oct.full)
  setDT(cards.oct.focus)
  setDT(cards.sto.full)
  setDT(cards.sto.focus)
  
  .GlobalEnv$cards.oct.full <- cards.oct.full
  .GlobalEnv$cards.oct.focus <- cards.oct.focus
  .GlobalEnv$cards.sto.full <- cards.sto.full
  .GlobalEnv$cards.sto.focus <- cards.sto.focus
  # .GlobalEnv$stops.sto <- stops.sto
  # .GlobalEnv$stops.oct <- stops.oct
}

#####################################################
## Search for the transferts STO -> OCT
search.corr.sto2oct <- function(d, oct.j){
  d.oct <- oct.j[ FarecardID == d$NumCarteSerie & 
                    Time > d$HeureTransaction & 
                    Time - d$HeureTransaction < 1.5]
  if( nrow(d.oct) > 1 ){
    d.oct <- d.oct %>% arrange(Time) %>% head(1)
  }
  d.out <- data.frame(NumCarte=NA,titre=NA,date=NA,HeureA=NA,LigneA=NA,StopA=NA,HeureB=NA,StopB=NA,TripB=NA)
  if( nrow(d.oct) == 0 ) return(d.out)
  else {
    d.out$NumCarte <- as.character(d$NumCarteSerie)
    d.out$titre <- as.character(d$CodeTypeTitre)
    d.out$date <- as.Date(d$DateDeplacement)
    
    d.out$HeureA <- as.numeric(d$HeureTransaction)
    d.out$StopA <- as.character(d$NumArret)
    d.out$LigneA <- as.character(d$NumLigne)
    
    d.out$HeureB <- as.numeric(d.oct$Time)
    d.out$StopB <- as.character(d.oct$BusStopId)
    d.out$TripB <- as.numeric(d.oct$HastusTripId)
  }
  return(d.out)
}
#####################################################
## Search for the transferts OCT -> STO
search.corr.oct2sto <- function(d, sto.j){
  
  d.sto <- sto.j[ NumCarteSerie == d$FarecardID & 
                    HeureTransaction > d$Time & 
                    HeureTransaction - d$Time < 1.5]
  if( nrow(d.sto) > 1 ){
    d.sto <- d.sto %>% arrange(HeureTransaction) %>% head(1)
  }
  d.out <- data.frame(NumCarte=NA,titre=NA,date=NA,HeureA=NA,TripA=NA,StopA=NA,HeureB=NA,StopB=NA,LigneB=NA)
  if( nrow(d.sto) == 0 ){
    return(d.out)
  }
  else {
    d.out$NumCarte <- as.character(d.sto$NumCarteSerie)
    d.out$titre <- as.character(d$TrxTypeName)
    d.out$date <- as.Date(d.sto$DateDeplacement)
    
    d.out$HeureA <- as.numeric(d$Time)
    d.out$StopA <- as.character(d$BusStopId)
    d.out$TripA <- as.numeric(d$HastusTripId)
    
    d.out$HeureB <- as.numeric(d.sto$HeureTransaction)
    d.out$StopB <- as.character(d.sto$NumArret)
    d.out$LigneB <- as.character(d.sto$NumLigne)
  }
  return(d.out)
}
