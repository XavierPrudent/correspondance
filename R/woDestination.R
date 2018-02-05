#!/usr/bin/env Rscript

## Compute charge profile for STO lines heading to Ottawa downtown
## Consider only a portion of Jan/Feb 2017 and average it to 1 day

source("load_R_pkg.R")
source("woDestination_include.R")

## Input information
set.user.data()

## Read chipcards
read.cards()

##########################
## STO -> OCT
list.j <- unique(cards.sto$DateDeplacement)
nDays <- length(list.j)
corr.sto.oct <- vector(mode="list", length = nDays)
for( j in 1:nDays ){
  print(j)
  sto.j <- cards.sto.full[DateDeplacement == list.j[[j]]]
  oct.j <- cards.oct.focus[date == list.j[[j]]]
    corr.sto.oct[[j]] <- sto.j %>%
    rowwise() %>%
    do(search.corr.sto2oct(., oct.j))
}
corr.sto.oct <- do.call(rbind.data.frame, corr.sto.oct)
corr.sto.oct <- corr.sto.oct[complete.cases(corr.sto.oct),]
saveRDS(corr.sto.oct,"out/sto-vers-oct/sto_rawCardsData/corr.sto.oct_complete.rds")

nCAP.sto <- nrow(cards.sto.full)
nTransferts <- nrow(corr.sto.oct)
nTransferts_Gat <- corr.sto.oct %>% filter(StopB %in% stops_oct_gattineau) %>% nrow()
nTransferts_Ott <- corr.sto.oct %>% filter(!(StopB %in% stops_oct_gattineau)) %>% nrow()

nCAP.sto <- round(nCAP.sto/nDays)
nTransferts <- round(nTransferts/nDays)
nTransferts_Gat <- round(nTransferts_Gat/nDays)
nTransferts_Ott <- round(nTransferts_Ott/nDays)

print(nCAP.sto)
print(nTransferts)
print(nTransferts_Gat)
print(nTransferts_Ott)

print(nCAP.sto*1.2)
print(nTransferts*1.2)
print(nTransferts_Gat*1.2)
print(nTransferts_Ott*1.2)

##############################################
## OCT -> STO
# 99;USAGER OC;USA_OC  
# 132;USAGER OC DESFIRE;US_OCT_D
# 255;USAGER OC TRANSPO;PRESTO
octID <- c(99,132,255)
octInSTO <- cards.sto.full %>% filter(CodeTypeTitre %in% octID)
print(nrow(octInSTO)/nDays)
nOctInSTO_Ott <- octInSTO %>% filter(NumArret %in% stops_sto_ottawa) %>% nrow()
nOctInSTO_Gat <- octInSTO %>% filter(!(NumArret %in% stops_sto_ottawa)) %>% nrow()
print(nOctInSTO_Ott/nDays)
print(nOctInSTO_Gat/nDays)
nSTO_Ott <- cards.sto.full %>% filter(NumArret %in% stops_sto_ottawa & Direction == "Nord") %>% nrow()
print(nSTO_Ott/nDays)
nCorrSTO2OCT_oct <- corr.sto.oct %>% filter(titre %in% octID) %>% nrow()
print(nCorrSTO2OCT_oct/nDays)


list.j <- unique(cards.sto$DateDeplacement)
nDays <- length(list.j)
corr.oct.sto <- vector(mode="list", length = nDays)
for( j in 1:nDays ){
  print(j)
  sto.j <- cards.sto.focus[DateDeplacement == list.j[[j]]]
  oct.j <- cards.oct.full[date == list.j[[j]]]
  
  corr.oct.sto[[j]] <- oct.j %>%
    rowwise() %>%
    do(search.corr.oct2sto(., sto.j))
}

corr.oct.sto <- do.call(rbind.data.frame, corr.oct.sto)
corr.oct.sto <- corr.oct.sto[complete.cases(corr.oct.sto),]
saveRDS(corr.oct.sto,"out/oct-vers-sto/sto_rawCardsData/corr.oct.sto_complete.rds")

nCAP.oct <- nrow(cards.oct.full)
nTransferts <- nrow(corr.oct.sto)
nTransferts_Ott <- corr.oct.sto %>% filter(StopB %in% stops_sto_ottawa) %>% nrow()
nTransferts_Gat <- corr.oct.sto %>% filter(!(StopB %in% stops_sto_ottawa)) %>% nrow()

nCAP.oct <- round(nCAP.oct/nDays)
nTransferts <- round(nTransferts/nDays)
nTransferts_Gat <- round(nTransferts_Gat/nDays)
nTransferts_Ott <- round(nTransferts_Ott/nDays)

print(nCAP.oct)
print(nTransferts)
print(nTransferts_Gat)
print(nTransferts_Ott)

print(nCAP.oct*1.2)
print(nTransferts*1.2)
print(nTransferts_Gat*1.2)
print(nTransferts_Ott*1.2)



