library(ggvis)
library(tidyr)
library(dplyr)
library(data.table)

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


#' Check if datain is a valid es. Experimental.
#' 
#' @param datain data.frame
#' @return list with logical field valid, may change in the future
#'
datain_is_valid <- function(datain) {
    result <- list(valid=TRUE, messages=NULL)
    if(is.null(datain)) {
        result$valid <- FALSE
        result$message <- 'datain is null'
    } else if(!is.data.frame(datain)) {
        result$valid <- FALSE
        result$message <- 'datain is null'
    } else if (ncol(datain) < 5) {
        result$valid <- FALSE
        result$message <- 'not enough columns'
    } else {
        # Not pretty but should handle all flavours of data.frames 
        if(any(lapply(datain %>% select_(.dots=colnames(datain)[-1]), class) != 'numeric')) {
            result$valid <- FALSE
            result$message <- 'not numeric'
        }
        if(any(datain %>% select_(.dots=colnames(datain)[-1]) < 0)) {
            result$valid <- FALSE
            result$message <- 'negative values'
        }
    }
    result
}
