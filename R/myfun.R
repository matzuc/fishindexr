
myfun <- function(x) {
        require(dplyr)
        group_by(x, species) %>% summarize(ave = mean(abundance))}
