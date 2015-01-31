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
        left_join(data_description) %>%
        select(geo_accession,  gene, perturbation, species, category, neg_log_pval)
}