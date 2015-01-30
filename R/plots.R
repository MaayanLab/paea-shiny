library(ggvis)

#' Prepare chdir results for plotting
#' @param results numeric vector extracted from pchdir results
#' @param n numeric max number of genes to keep
#' @return data.frame
#'
prepare_results <- function(results, n=40) {
    results <- head(results, n)
    data.frame(
        g = factor(
            names(results),
            names(results)[order(abs(results), decreasing=TRUE)]
        ),
        v = results
    )
}

#' Plot top genes from chdirAnalysis
#' @param results data frame returned from prepare_results 
#' @return ggvis plot
#'
plot_top_genes <- function(results) {

    properties <- axis_props(
        axis=list(stroke=NULL), 
        ticks = list(stroke = NULL),
        labels=list(angle=-90, fontSize = 10, align='right' )
    )
    
    ggvis(results, ~g, ~v) %>% 
        ggvis::layer_bars(width = 0.75) %>%
        scale_numeric('y', domain = c(min(results$v), max(results$v))) %>%
        add_axis('y', grid=FALSE, title = 'Coefficient') %>%
        add_axis('x', grid=FALSE, offset = 10, title = '', properties = properties)
}