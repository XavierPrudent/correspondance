#!/usr/bin/env Rscript

## Compute charge profile for STO lines heading to Ottawa downtown
## Consider only a portion of Jan/Feb 2017 and average it to 1 day

source("/home/xprudent/civilia/general/load_R_pkg.R")
source("/home/xprudent/civilia/STO/correspondance-oct-sto/R/include.R")

## Input information
set.user.data()

## Read chipcards
read.cards()

cluster.job <- TRUE

if(!cluster.job){
corr.oct.sto <- cards.oct %>%
group_by(FarecardID) %>%
  do(loop.cards.oct2sto(.)) 
}

if(cluster.job){
cl <- create_cluster()
set_default_cluster(cl)

## Load objects to clusters
cluster_copy(cl, search.corr.oct2sto)
cluster_copy(cl, loop.cards.oct2sto)
cluster_copy(cl, cards.oct)
cluster_copy(cl, cards.sto)

## Load libraries
cluster_library(cl,list.pkg)

corr.oct.sto <- cards.oct %>%
partition(FarecardID) %>%
  do(loop.cards.oct2sto(.)) %>%
	collect()
}

## Loop over STO users, transfer to OCT
#corr.sto.oct <- cards.sto %>%
#  group_by(NumCarteSerie) %>%
#  rowwise() %>%
#  do(search.corr(.)) %>%
#  filter(!is.na(NumCarte))


saveRDS(corr.oct.sto,paste0(output.dir,"corr.oct.sto.rds"))