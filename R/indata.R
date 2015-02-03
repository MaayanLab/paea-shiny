library(ggvis)
library(tidyr)
library(dplyr)

#' Create density plots for input data
#' 
#' @param indata data.frame with genenames in first columns and samples in following
#' @return ggvis
#'
plot_density <- function(indata) {
    # Rename first column to identifier
    indata %>% rename_(identifier = as.symbol(colnames(indata)[1])) %>%
    # Convert to long
    gather(sample, value, -identifier) %>% 
    # Create plot
    ggvis(~value) %>% group_by(sample) %>% 
    layer_densities(stroke = ~sample, fill := NA)
}
