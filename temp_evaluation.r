vedataN <- read.csv("D:/Dropbox/Didattica/labB/2016/indice/N_tot.csv")[, -1]
vedataB<- read.csv("D:/Dropbox/Didattica/labB/2016/indice/B_tot.csv")[, -1]

save(vedataN, file = "data/vedataN.Rdata")
save(vedataB, file = "data/vedataB.Rdata")

db <- reshape2::melt(vedataN, id.vars = c(1:5))
names(db)[6:7] <- c("species", "abundance")


db <- reshape2::melt(vedataN, id.vars = c(1:5))
names(db)[6:7] <- c("species", "abundance")

dbB <- reshape2::melt(vedataB, id.vars = c(1:5))
names(dbB)[6:7] <- c("species", "biomass")


db <- merge(db, dbB)
save(db, file = "data/db.RData")

a = list("st")


indB <- which(names(db) == "biomass")
indN <- which(names(db) == "abundance")

N <- reshape2::dcast(db[, -c(indB)],  ... ~ species, value.var = names(db)[indN])
B <- reshape2::dcast(db[, -c(indN)],  ... ~ species, value.var = names(db)[indB])




my.spdb <- 6



library(fishindexr)

hfbi(db = db,  my.area = 5, cod = 1:5, my.sp = 6)

