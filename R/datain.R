library(ggvis)
library(tidyr)
library(dplyr)

#' Create density plots for input data
#' 
#' @param datain data.frame with genenames in first columns and samples in following
#' @return ggvis
#'
plot_density <- function(datain) {
    # Rename first column to identifier
    datain  %>% rename_(identifier = as.symbol(colnames(datain)[1])) %>%
    # Convert to long
    gather(sample, value, -identifier) %>%
    # Create plot 
    ggvis(~value) %>% group_by(sample) %>% 
    layer_densities(stroke = ~sample, fill := NA)
}
