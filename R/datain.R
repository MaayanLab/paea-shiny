library(ggvis)
library(tidyr)
library(dplyr)
library(data.table)

#' Create density plots for input data
#' 
#' @param datain data.table with genenames in first columns and samples in following
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
    result <- list(valid=TRUE, message=NULL)
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
        if(any(lapply(datain %>% select_(-1), class) != 'numeric')) {
            result$valid <- FALSE
            result$message <- 'not numeric'
        } else if(any(datain %>% select_(-1) < 0)) {
            result$valid <- FALSE
            result$message <- 'negative values'
        }
    }
    result
}


#' log2 transform expression data
#' 
#' @param datain data.frame
#' @return data.frame where columns 2:ncol are log2 transformed
#'
datain_log2_transform <- function(datain) {
    data.table(
        datain %>% select_(1),
        datain %>% select_(-1) %>% mutate_each(funs(log2))
    )
}


#' quantile normalize expression data
#'
#' @param datain data.frame
#' @return data.frame where columns 2:ncol are quantile normalized
#'
datain_quantile_normalize <- function(datain, add_noise=FALSE) {
    add_noise <- if(add_noise) { function(x) x + runif(length(x), 0, 1e-12) } else { identity }
    
    setNames(
        data.table(
            datain %>% select_(1),
            datain %>% select_(-1) %>% as.matrix() %>%
            preprocessCore::normalize.quantiles() %>% 
            # Ugly workaround for issue with GeoDE 
            # TODO Remove as soon as possible 
            as.data.frame() %>% 
            mutate_each(funs(add_noise))
        ),
        colnames(datain)
    )
}


#' Apply preprocesing steps to expression data
#'
#' @param datain data.frame 
#' @param log2_transform logical
#' @param quantile_normalize logical
#' @return data.table
#'
datain_preprocess <- function(datain, log2_transform=FALSE, quantile_normalize=FALSE) {
    log2_f <- if(log2_transform) { datain_log2_transform } else { identity }
    quant_norm_f <- if(quantile_normalize) { datain_quantile_normalize } else { identity }
    
    datain %>% log2_f() %>% quant_norm_f()
}


