# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

nafi <- function(N = NULL, B = NULL, my.sp = NULL, cod = NULL, guildsI = guildsGM, Gf_list = c(4:8), Gt_list = c(10:17), nafithresholdI= nafithreshold, nafirefcondI = nafirefcond) {

N$area <- 1
B$area <- 1
my.area <- which(names(N) == "area")

        # inserire un controllo per verificare se si trovano tutte le guild e avvisare se qualcuna manca

# Metrics computation

       # N[is.na(N), ] <- 0
        #B[is.na(B), ] <- 0
        #
        M <- N[ , my.sp]
        M [M >1] <- 1

        M_01_S <- rowSums(as.data.frame(M))

        rm(M)

        # M_04_d_N ----------
        M_04_d_N <- rowSums((N[ , my.sp]/N[,my.area]))



        # M_05_d_B ----------
        M_05_d_B <- rowSums((B[ , my.sp]/B[,my.area]))

        # M_08_d..B ----------
        M_08_d..B <- ifelse(M_01_S == 0, 0, ifelse(M_01_S == 1, 0.01, (M_01_S -1) / log(M_05_d_B + 1)))
        # plot(M1_d..N, M_08_d..B)


        # M_06_B.N ----------
        M_06_B_N <- M_05_d_B / M_04_d_N
        M_06_B_N[which(is.nan(M_06_B_N))] <- 0 #mean(M_06_B.N, na.rm = T) # E' giusto che dove non c'? niente sia 0



        # gruppi funzionali  ------------------------
        NN <-  N[, my.sp] / N[, my.area]# Abbondanze (densit?)
        PP <- N[, my.sp] # P/A
        PP [PP >1] <- 1
        BB <- B[, my.sp] / N[, my.area] # biomasse (densit?)

        # guilds

        G <- guildsI


        for (i in 1:length(Gt_list)){
                G[which(is.na(G[, Gt_list[i]])), Gt_list[i]] <- 0
        }

        NGf <- as.data.frame(as.matrix(NN) %*% as.matrix(G[match(names(NN), G$LAB), Gf_list]))
        SGf <- as.data.frame(as.matrix(PP) %*% as.matrix(G[match(names(PP), G$LAB), Gf_list]))
        BGf <- as.data.frame(as.matrix(BB) %*% as.matrix(G[match(names(BB), G$LAB), Gf_list]))


        # attenzione!!!!!!!!! I conti funzionano solo se ci sono le diete esclusivamente per gli ES e D MM, altrimenti NO!
        NGt <- as.data.frame(as.matrix(NN) %*% as.matrix(G[match(names(NN), G$LAB), Gt_list]))
        SGt <- as.data.frame(as.matrix(PP) %*% as.matrix(G[match(names(PP), G$LAB), Gt_list]))
        BGt <- as.data.frame(as.matrix(BB) %*% as.matrix(G[match(names(BB), G$LAB), Gt_list]))


        # M_36_d_N_Bv ------------
        M_36_d_N_Bv <- NGt$Bmi + NGt$Bma


        # M9_Ebal_N ------------
        M9_Ebal_N <- rowSums(sqrt(NGf/M_04_d_N))
        M9_Ebal_N[which(is.nan(M9_Ebal_N))] <- 1
        M9_Ebal_N[which((M9_Ebal_N == 0))] <- 1


        # M_23_d_B_migr --------------
        M_23_d_B_migr <- BGf$D + BGf$MM



        M <- data.frame(M_05_d_B,  M_06_B_N, M_08_d..B, M_23_d_B_migr, M_36_d_N_Bv)


        M <- cbind(N[, cod], M)

        T <- merge(x = M, y = nafirefcondI, by = c("season", "WBT", "vegetated"), all.x = TRUE, all.y = FALSE, sort = F)

        names(T)[(ncol(M)+1) : ncol(T)] <- sub("M" , "ref", x = names(T)[(ncol(M)+1) : ncol(T)])
        names(T) <- sub(".x" , "", x = names(T))
        names(T) <- sub(".y" , "", x = names(T))

        E <- M[, c((length(cod) + 1):ncol(M))] / T[, (ncol(M)+1):ncol(T)]

        for(i in (1: ncol(E))){
                E[which(E[, i] > 1), i] <- 1
        }

        names(E) <- sub("M_" , "EQR_", x = names(E))







        NAFI <- data.frame(NAFI = rowMeans(E))


        NAFI$class <- cut(NAFI$NAFI, c(nafithresholdI[,2], 1000),  labels = c("Bad", "Poor", "Moderate", "Good", "High"))


        # calcolo indice
        # classificazione
#NOOOOO
        all <- cbind(M, T[-c(1:length(cod))], E, NAFI)
        return(all)
}
