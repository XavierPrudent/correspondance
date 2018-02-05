
d.sto.oct <- readRDS("out/sto-vers-oct/corr.sto.oct_complete.rds")
d.sto.oct <- d.sto.oct[complete.cases(d.sto.oct), ]

d.sto.oct$StopA <- as.integer(d.sto.oct$StopA )

arretOtt <- d.sto.oct %>% filter(StopA >= 8000)
arretGat <- d.sto.oct %>% filter(StopA < 8000)