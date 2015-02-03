library(tidyr)
library(dplyr)

#' Join PAEA results with data description
#' @param paea_pvalues pvalues taken from PAEAAnalysis results
#' @param data_description tbl_df as returned from extract_description
#' @return tbl_df 
#'
prepare_paea_results <- function(paea_pvalues, data_description) {
    tbl_df(data.frame(
        set=colnames(paea_pvalues),
        neg_log_pval=as.vector(-log(paea_pvalues))
    )) %>% 
        tidyr::separate(set, into=c('id', 'category'), sep='_') %>% 
        mutate(id=as.numeric(id)) %>%
        left_join(data_description, by='id') %>%
        select(id, geo_accession,  gene, perturbation, species, category, neg_log_pval)
}

#' PAEAAnalysis wrapper. Redirects plots to /dev/null 
#'
#' @param chdirprops
#' @param gmt
#' @param gammas
#' @param casesensitive
#'

paea_analysis_wrapper <- function(chdirresults, gmtfile, gammas = c(1), casesensitive = FALSE){
    png('/dev/null')
    paea <- GeoDE::PAEAAnalysis(chdirresults, gmtfile, gammas, casesensitive, showprogress=TRUE)
    dev.off()
    paea
}
