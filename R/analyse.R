
setwd("/Users/lavieestuntoucan/civ-sto/tech/correspondance/R")

d.sto.oct <- readRDS("out/sto-vers-oct/corr.sto.oct_complete.rds")
d.sto.oct <- d.sto.oct[complete.cases(d.sto.oct), ]

d.sto.oct$StopA <- as.integer(d.sto.oct$StopA )

arretOtt <- d.sto.oct %>% filter(StopA >= 8000)
arretGat <- d.sto.oct %>% filter(StopA < 8000)

##########################################################
## Etude fuite commerciale

## Number of STO cards in OCT network
d.oct <- readRDS("in/cartes_a_puces/octranspo/sto_20171026100652_9joursSemaine_janvier2017.RDS")
d.sto <- readRDS("in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus_9joursSemaine_janvier2017.RDS")
## Number of cards
nCards.oct <- length(unique(d.oct$FarecardID))
nCards.sto <- length(unique(d.sto$NumCarteSerie))
## Overlap
OCTinSTO <- d.oct %>% filter(FarecardID %in% d.sto$NumCarteSerie)
nCards.OCTinSTO <- length(unique(OCTinSTO$FarecardID))
OCTnotSTO <- d.oct %>% filter(!(FarecardID %in% d.sto$NumCarteSerie))
nCards.OCTnotSTO <- length(unique(OCTnotSTO$FarecardID))
write.table(unique(OCTnotSTO$FarecardID), file="out/cartesSTO_inOCT_notInSTO.csv", quote = F, row.names = F, col.names = F)
STOinOCT <- d.sto %>% filter(NumCarteSerie %in% d.oct$FarecardID)
nCards.STOinOCT <- length(unique(STOinOCT$NumCarteSerie))
STOnotOCT <- d.sto %>% filter(!(NumCarteSerie %in% d.oct$FarecardID))
nCards.STOnotOCT <- length(unique(STOnotOCT$NumCarteSerie))
## Type of cards
## Fraction of trips
df <- STOinOCT %>% 
  group_by(CodeTypeTitre) %>% 
  dplyr::summarise(n=round(100*n()/nrow(STOinOCT),1)) %>% 
  arrange(desc(n))
df$CodeTypeTitre <- factor(df$CodeTypeTitre, levels = unique(df$CodeTypeTitre)[order(df$n, decreasing = TRUE)])
plot_ly(data=df, x=~CodeTypeTitre, y=~n, type="bar",color=I(rgb(145/255,191/255,39/255))) %>% 
  layout(xaxis=list(title="Titres de transport"),yaxis=list(title="Fraction de l'ensemble des voyages (%)"))
## Fraction of total cards
df <- STOinOCT %>% 
  select(c(NumCarteSerie,CodeTypeTitre)) %>% 
  distinct() %>% 
  group_by(CodeTypeTitre) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(tot = sum(n), n = round(100*(n/tot),digits = 1)) %>%
  arrange(desc(n))
df$CodeTypeTitre <- factor(df$CodeTypeTitre, levels = unique(df$CodeTypeTitre)[order(df$n, decreasing = TRUE)])
plot_ly(data=df, x=~CodeTypeTitre, y=~n, type="bar",color=I(rgb(145/255,191/255,39/255))) %>% 
  layout(xaxis=list(title="Titres de transport"),yaxis=list(title="Fraction de l'ensemble des cartes (%)"))
## Users that jump in at Gatineau during 24h and btw 6-9am
stops_oct_gattineau <- c("CK101","CK103","CK105","CK110","CK115","CK125","CK130","CK140","CK150","CK155","CK160","CK170","CK175","CK180","CK183","CK190","CK195","CK210","CK220","CK230")
d.oct.gat <- d.oct %>% filter(BusStopId %in% stops_oct_gattineau)
d.octNotSTO.gat <- d.oct.gat %>% filter(!(FarecardID %in% d.sto$NumCarteSerie))
nCards.d.octNotSTO.gat <- length(unique(d.octNotSTO.gat$FarecardID))
d.octNotSTO.gat.AM <- d.octNotSTO.gat %>% filter(Time >= 6 & Time <= 9)
nCards.d.octNotSTO.gat.AM <- length(unique(d.octNotSTO.gat.AM$FarecardID))

## Does the phenomen increase with time?

## OCT data
cards.oct <- fread("in/cartes_a_puces/octranspo/sto_20171026100652.csv",colClasses=c(FarecardID="character"),header=TRUE)
setDT(cards.oct)
cards.oct <- cards.oct[Year == 2017]
cards.oct <- cards.oct %>% mutate(date = as.Date(paste(Year,Month,Day,sep="-")))
cards.oct <- cards.oct %>% mutate(weekday = weekdays(date))
cards.oct <- cards.oct %>% filter(weekday %in% c("Tuesday","Wednesday","Thursday"))
cards.oct <- cards.oct %>% mutate(weeknb = strftime(date, format = "%V"))
cards.oct$weeknb <- as.numeric(cards.oct$weeknb)
saveRDS(cards.oct,"in/cartes_a_puces/octranspo/sto_20171026100652_janvier-aout2017.RDS")

## STO data
cards.sto <- fread("in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus.CSV",sep=";",colClasses=c(NumCarteSerie="character"),header=TRUE)
cards.sto <- cards.sto %>%
  separate(col=DateService,sep=" ", into=c("DateDeplacement","X")) %>%
  select(-X) %>%
  mutate(DateDeplacement = as.Date(DateDeplacement)) %>%
  mutate(weekday=weekdays(DateDeplacement)) %>%
  filter(weekday %in% c("Tuesday","Wednesday","Thursday")) %>% 
  mutate(weeknb = strftime(DateDeplacement, format = "%V"))
cards.sto <- cards.sto %>% filter(DateDeplacement > as.Date("2017-01-01 00:00:00"))
cards.sto <- cards.sto %>% filter(DateDeplacement < as.Date("2069-01-01 00:00:00"))
cards.sto$weeknb <- as.numeric(cards.sto$weeknb)
saveRDS(cards.sto,"in/cartes_a_puces/sto/originaux/TransactionBusjuin2016Plus_9joursSemaine_janvier-aout2017.RDS")

setDT(cards.oct)
setDT(cards.sto)

listWeeks <- sort(unique(cards.sto$weeknb))
df <- data.frame(nWeek=rep(x = NA,times=length(listWeeks)),
                 nCardsOCT=rep(x = NA,times=length(listWeeks)), 
                 nCardsOCTinSTO=rep(x = NA,n=length(listWeeks)))
for( i in 1:length(listWeeks)){
  print(i)
  sto.i <- cards.sto[weeknb == listWeeks[i]]
  oct.i <- cards.oct[weeknb == listWeeks[i]]
  
  OCTinSTO <- oct.i[FarecardID %in% sto.i$NumCarteSerie]
  
  df$nWeek[i] <- listWeeks[i]  
  df$nCardsOCT[i] <- length(unique(oct.i$FarecardID))
  df$nCardsOCTinSTO[i] <- length(unique(OCTinSTO$FarecardID))
}
df <- df %>% mutate(frac = 100-round(100*(nCardsOCTinSTO/nCardsOCT),1))


plot_ly(data=df,x=~nWeek,y=~frac,mode="lines+markers",type="scatter") %>%
  layout(xaxis=list(title="Semaines 2017"),
         yaxis=list(title="Fraction non vues sur le réseau STO (%)",
          range=c(0,100)))


plot_ly(data=df,x=~nWeek,y=~(nCardsOCT-nCardsOCTinSTO),mode="lines+markers",type="scatter") %>%
  layout(xaxis=list(title="Semaines 2017"),
         yaxis=list(title="Nb. de cartes non vues sur le réseau STO"))





