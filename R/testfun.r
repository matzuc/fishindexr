testfun <- function(x){
        require(dplyr)
        out <- group_by(x, st) %>% summarise(aveAbu = mean(abundance))
}
