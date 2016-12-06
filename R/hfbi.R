# Test function
#


hfbi <- function(db = NULL, N = NULL, B = NULL, my.sp = NULL, my.area = NULL, cod = NULL, guildsI = guilds, Gf_list = c(4:8), Gt_list = c(10:17), hfbiweightsI = hfbiweights, hfbithresholdI = hfbithreshold, hfbirtI = hfbirt, hfbirefcondI = hfbirefcond, Nname = "abundance", Bname = "biomass") {

# check if the data have been supplied as a single database or as two data.frame (N & B)

        if(! is.null(db)){ # if a db has been provided (it should have a )
                indB <- which(names(db) == Nname)
                indN <- which(names(db) == Bname)

                #♣ pointer to the species column
                my.spdb <- my.sp

                all_species <- as.character(levels(factor(db[,my.spdb])))


                N <- reshape2::dcast(db[, -c(indB)],  ... ~ species, value.var = names(db)[indN])
                B <- reshape2::dcast(db[, -c(indN)],  ... ~ species, value.var = names(db)[indB])

                # set up the species list
                my.sp <- match(all_species, names(N))
        }




# Metrics computation


        N[is.na(N)] <- 0
        B[is.na(B)] <- 0
        #
        M <- N[ , my.sp]
        M [M >1] <- 1

        M_01_S <- rowSums(as.data.frame(M))

        rm(M)

        # M_04_d_N ----------
        M_04_d_N <- rowSums((N[ , my.sp]/N[,my.area]) * 100)

        # M_05_d_B ----------
        M_05_d_B <- rowSums((B[ , my.sp]/B[,my.area]) * 100)

        # M_06_B.N ----------
        M_06_B_N <- M_05_d_B / M_04_d_N
        M_06_B_N[which(is.nan(M_06_B_N))] <- 0 #mean(M_06_B.N, na.rm = T) # E' giusto che dove non c'? niente sia 0
        # M_10_SdomB  ----------
        M_10_SdomB <- unlist(lapply(1:nrow(B), function(x) length(my.sp)+1-length(which(cumsum(as.matrix(sort(B[x ,my.sp], decreasing = TRUE)))/sum(as.matrix(sort(B[x,my.sp], decreasing = TRUE)))>=0.9))))
        M_10_SdomB <- ifelse(M_01_S == 0, 0, M_10_SdomB)
        M_10_SdomB <- ifelse(M_10_SdomB >= length(my.sp), 0, M_10_SdomB)


        # M_11_DdomB  ----------
        M_11_D_domB <- ifelse(M_10_SdomB == 0, 0, ifelse(M_10_SdomB == 1, 0.01, (M_10_SdomB -1) / log(M_05_d_B *0.9 + 1)))






        # gruppi funzionali  ------------------------
        NN <- 100 * N[, my.sp] / N[, my.area]# Abbondanze (densit?)
        PP <- N[, my.sp] # P/A
        PP [PP >1] <- 1
        BB <- 100 * B[, my.sp] / N[, my.area] # biomasse (densit?)

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

        # M_19_S_migr ----------------
        M_19_S_migr <- SGf$D + SGf$MM

        # M_23_d_B_migr --------------
        M_23_d_B_migr <- BGf$D + BGf$MM

        # M_25_d..migr_B ------------
        M_25_D_migr_B  <- ifelse(M_19_S_migr == 0, 0, ifelse(M_19_S_migr == 1, 0.01, (M_19_S_migr -1) / log(M_23_d_B_migr + 1)))
        M_25_D_migr_B[which(is.infinite(M_25_D_migr_B))] <- max(M_25_D_migr_B[which(!is.infinite(M_25_D_migr_B))] )

        # M_37_d_B_Bv ------------
        M_37_densB_Bv <- BGt$Bmi + BGt$Bma

        M_33_S_Bv <- SGt$Bmi + SGt$Bma
        min_S_Bv <- 0.2 # to be tuned for other guilds! # A formula could subsetitute this parameter

        # M_39_d..Bv_B ------------
        M_39_D_Bv_B  <- ifelse(M_33_S_Bv == 0, 0, ifelse(M_33_S_Bv == min_S_Bv, 0.01, (M_33_S_Bv -min_S_Bv) / log(M_37_densB_Bv + 1)))
        M_39_D_Bv_B[which(is.nan(M_39_D_Bv_B))] <- 0
        M_39_D_Bv_B[which(is.infinite(M_39_D_Bv_B))] <- max(M_39_D_Bv_B[which(!is.infinite(M_39_D_Bv_B))] )

        #M_41_S_HZP -----------
        M_41_S_HZP <- SGt$HZ + SGt$HP # + SGt$PL i plantivori non sono inclusi!!!!!!!!
        min_S_HZP <- 0.2 # min(M_41_S_HZP[M_41_S_HZP > 0])

        # M_45_d_B_HZP ------
        M_45_d_B_HZP <- BGt$HZ + BGt$HP #+ BGt$PL # + SGt$PL i plantivori non sono inclusi!!!!!!!!

        # M_47_d..HZP_B ------------
        M_47_D_HZP_B  <- ifelse(M_41_S_HZP == 0, 0, ifelse(M_41_S_HZP == min_S_HZP, 0.01, (M_41_S_HZP - min_S_HZP) / log(M_45_d_B_HZP + 1)))

        # Data imputation: infinite values are replaced with the maximum of the dataset
        M_47_D_HZP_B[which(is.infinite(M_47_D_HZP_B))] <- max(M_47_D_HZP_B[which(!is.infinite(M_47_D_HZP_B))] )





        M <- data.frame(M_06_B_N, M_11_D_domB, M_25_D_migr_B, M_37_densB_Bv, M_39_D_Bv_B, M_47_D_HZP_B)
        Mnotlog <- M
        # this is the new part: the log transformation was missing!!! -------------
        M <- log(M + 1)

        # same order as the metrics order

        hfbiweightsI <- hfbiweightsI[match(names(M), hfbiweightsI$var), ]

        # end new part -------



        M <- cbind(N[, cod], M)
        M$id <- 1:nrow(M)

        Tr <- merge(x = M, y = hfbirefcondI, by= c("season", "WBT", "vegetated"), sort = F, all.x = T, all.y = F)

        # re-order the matrix
        Tr <- Tr[order(Tr$id), ]

        # delete id column
        which(names(Tr) == "id")
                Tr <- Tr[, -c(which(names(Tr) == "id"))]
                M <- M[, -c(which(names(M) == "id"))]


        names(Tr)[(ncol(M)+1) : ncol(Tr)] <- sub("M_" , "ref_", x = names(Tr)[(ncol(M)+1) : ncol(Tr)])
        names(Tr) <- sub(".x" , "", x = names(Tr))
        names(Tr) <- sub(".y" , "", x = names(Tr))

        E <- M[, c((length(cod) + 1):(ncol(M)))] / Tr[, (ncol(M)+1):ncol(Tr)]

        for(i in (1: ncol(E))){
                E[which(E[, i] > 1), i] <- 1
        }

        names(E) <- sub("M_" , "EQR_", x = names(E))


        names(Mnotlog) <- sub("M_" , "notlog_", x = names(Mnotlog))



        HFBI <- data.frame(HFBI = as.numeric((as.matrix(E) %*% as.matrix(hfbiweightsI$w [match(names(Tr)[(length(cod)+1) : (length(cod)+6) ], hfbiweightsI$var)]))/sum(hfbiweightsI$w [match(names(Tr)[(length(cod)+1) : (length(cod)+6) ], hfbiweightsI$var)])))

        HFBI$HFBIts <- (HFBI$HFBI + hfbirtI[[1]]) / hfbirtI[[2]]


        HFBI$HFBIts <- ifelse(HFBI$HFBIts > 0.999 & HFBI$HFBIts < (1 + hfbirtI[[1]]) / hfbirtI[[2]] , 0.999, # if greather than 0.999 but smaller than the max
               ifelse(HFBI$HFBIts == (1 + hfbirtI[[1]]) / hfbirtI[[2]], 1, # if equal to the max, then set to 1
                      ifelse(HFBI$HFBIts < 0.001 & HFBI$HFBIts > (0 + hfbirtI[[1]]) / hfbirtI[[2]] , 0.001, #if smalle than 0.001 but larger than the min
                                ifelse(HFBI$HFBIts == (0 + hfbirtI[[1]]) / hfbirtI[[2]], 0, HFBI$HFBIts))))# if equal to the min, then set to 0



        ####################################################################################
        ####################################################################################
        ####################################################################################
        ####################################################################################

        ####################################################################################
        ####################################################################################
        ####################################################################################
        ####################################################################################
        # rispeto ai calcoli effettuati prima del 6-5-2016 sono sbagliati, perchè prima si usavana
        # le soglie per l'indice non trasformato sull'indice trasformato ## da controllare!!!
        ####################################################################################
        ####################################################################################




        HFBI$class <- cut( HFBI$HFBIts, c(hfbithresholdI$x, 1000),  labels = c("Bad", "Poor", "Moderate", "Good", "High"))






        # calcolo indice
        # classificazione

        all <- cbind(N[, cod], Mnotlog, Tr[-c(1:length(cod))], E, HFBI)
        return(all)
}
