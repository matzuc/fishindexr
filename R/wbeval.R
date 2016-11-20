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

wbeval <- function(dat = NULL, wb = "WB", HFBIts = "HFBIts", add1 = NULL, add2 = NULL, add3 = NULL) {
# add = Additional grouping factor

        dat$wb <- dat[ , wb]
        if(!is.null(add1)){ dat$add1 <- dat[, add1]}
        if(!is.null(add2)){ dat$add2 <- dat[, add2]}
        if(!is.null(add3)){ dat$add3 <- dat[, add3]}

                dat$HFBIts <- dat[ , HFBIts]


        if (is.null(add1)){
                gdat <- dplyr::group_by(dat, wb)
        } else if (is.null(add2)){
                gdat <- dplyr::group_by(dat, wb, add1)} else if (is.null(add3)) {gdat <- dplyr::group_by(dat, wb, add1, add2)} else {gdat <- dplyr::group_by(dat, wb, add1, add2, add3)}



         sdat <- dplyr::summarise(gdat, HFBI = mean(HFBIts, na.rm = T), sd = sd(HFBIts, na.rm = T), n = length(which(!is.na(HFBIts))))




                sdat$sd <- ifelse(sdat$n < 3, NA, sdat$sd)

        # classification

        sdat$class <- cut(sdat$HFBI, c(hfbithreshold$x, 1000),  labels = c("Bad", "Poor", "Moderate", "Good", "High"))
        sdat$pgood <- pnorm(1, mean=sdat$HFBI,sd=sdat$sd) -pnorm(hfbithreshold$x[4], mean=sdat$HFBI,sd=sdat$sd)

        return(sdat)

        }
