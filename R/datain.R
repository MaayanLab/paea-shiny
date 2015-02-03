library(ggvis)
library(tidyr)

#' Create density plots for input data
#' 
#' @param datain data.frame with genenames in first columns and samples in following
#' @return ggvis
#'
plot_density <- function(datain) {
    properties_y <- axis_props(labels=list(fontSize=12), title=list(fontSize=12, dy=-35))
    properties_x <- axis_props(labels=list(fontSize=12), title=list(fontSize=12, dx=-35))
    # Rename first column to identifier
    datain  %>% rename_(identifier = as.symbol(colnames(datain)[1])) %>%
    # Convert to long
    gather(sample, value, -identifier) %>%
    # Ugly and slow but works for now    
    as.data.frame() %>%
    # Create plot 
    ggvis(~value) %>% group_by(sample) %>%
    layer_densities(stroke = ~sample, fill := NA) %>%
    add_axis('y',  properties=properties_y) %>%
    add_axis('x',  properties=properties_x)
}
