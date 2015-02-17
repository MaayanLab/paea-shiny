#' Join PAEA results with data description
#' @param paea_pvalues pvalues taken from PAEAAnalysis results
#' @param data_description tbl_df as returned from extract_description
#' @return tbl_df 
#'
prepare_paea_results <- function(paea_pvalues, data_description) {
    dplyr::tbl_df(data.frame(
        set=colnames(paea_pvalues),
        neg_log_pval=as.vector(-log(paea_pvalues))
    )) %>% 
        tidyr::separate(set, into=c('id', 'category'), sep='_') %>% 
        dplyr::mutate(id=as.numeric(id)) %>%
        dplyr::left_join(data_description, by='id') %>%
        dplyr::select(id, geo_accession,  gene, perturbation, species, category, neg_log_pval)
}

#' PAEAAnalysis wrapper. Redirects plots to /dev/null 
#'
#' @param chdirresults see GeoDE::chdirAnalysis
#' @param gmtfile see GeoDE::chdirAnalysis
#' @param gammas see GeoDE::chdirAnalysis
#' @param casesensitive see GeoDE::chdirAnalysis
#' @return paea results
#'

paea_analysis_wrapper <- function(chdirresults, gmtfile, gammas = c(1), casesensitive = FALSE){
    png('/dev/null')
    paea <- GeoDE::PAEAAnalysis(chdirresults, gmtfile, gammas, casesensitive, showprogress=TRUE)
    dev.off()
    paea
}


#' Split chdir vector into negative and postive (plus zero) subvectors
#' 
#' @param chdirresults see GeoDE::chdirAnalysis
#' @return list with up and down fields
#'
split_chdirresults <- function(chdirresults) {
    # Split chdir results into up and down
    chdirresults_mask <- chdirresults[[1]][[1]][, 1] < 0
    chdirresults_up <-  list(list(chdirresults[[1]][[1]][!chdirresults_mask, , drop=FALSE]))
    chdirresults_down <- list(list(chdirresults[[1]][[1]][chdirresults_mask, , drop=FALSE]))
    list(up=chdirresults_up, down=chdirresults_down)
}


#' PAEAAnalysis dispatch function. Should handle separating chdirresults into up and down
#' and in future some filtering steps
#' 
#' @param chdirresults see GeoDE::chdirAnalysis
#' @param gmtfile see GeoDE::chdirAnalysis
#' @param gammas see GeoDE::chdirAnalysis
#' @param casesensitive see GeoDE::chdirAnalysis
#' @return list with up and down fields
#'
paea_analysis_dispatch <- function(chdirresults, gmtfile, gammas = c(1), casesensitive = FALSE){
    # Split chdir results into up and down
    chdirresults_mask <- chdirresults[[1]][[1]][, 1] < 0
    chdirresults_up <-  list(list(chdirresults[[1]][[1]][!chdirresults_mask, , drop=FALSE]))
    chdirresults_down <- list(list(chdirresults[[1]][[1]][chdirresults_mask, , drop=FALSE]))
    
    # Split gmt file into up and down
    gmtfile_mask <- unlist(lapply(gmtfile, function(x) { stringi::stri_endswith_fixed(x[1], '_downregulated') }))
    gmtfile_up <- gmtfile[!gmtfile_mask]
    gmtfile_down <- gmtfile[gmtfile_mask]
    
    list(
        up =  if(length(gmtfile_up) > 0 & length(chdirresults_up) > 0) {
            paea_analysis_wrapper(chdirresults_up, gmtfile_up,  gammas, casesensitive)
        },
        down =  if(length(gmtfile_down) > 0 & length(chdirresults_down) > 0) {
            paea_analysis_wrapper(chdirresults_down, gmtfile_down,  gammas, casesensitive)
        }
    )
}
