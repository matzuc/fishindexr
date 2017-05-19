# prova <- nafi(N = NtotGM, B = BtotGM, c(7:54), cod = c(1:6))
#
#
#  N2 <- read.csv("C:/Users/matzu/Dropbox/HFI/NAFI/dati/Nup.csv")
#  B2 <- read.csv("C:/Users/matzu/Dropbox/HFI/NAFI/dati/Nup.csv")
# #
#  names(N2)[c(4, 5,7)] <- c("WBT", "WB", "vegetated")
#  names(B2)[c(4, 5,7)] <- c("WBT", "WB", "vegetated")
# #
# #
# #
#  N2$vegetated[which(is.na(N2$vegetated))] <- 0
#  N2[which(is.na(N2$vegetated)), ]
#  B2$vegetated[which(is.na(B2$vegetated))] <- 0
#
#  N2$season <- factor(N2$season, levels = c("primavera", "autunno"), labels = c("spring", "autumn"))
#  B2$season <- factor(B2$season, levels = c("primavera", "autunno"), labels = c("spring", "autumn"))
#
#  prova2 <- nafi(N = N2, B = B2, c(8:55), cod = c(1:7))
#  N<-N2
#  B<-B2
#
