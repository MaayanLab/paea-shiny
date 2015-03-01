#' Join PAEA results with data description
#' @param paea_pvalues pvalues taken from PAEAAnalysis results
#' @param data_description tbl_df as returned from extract_description
#' @param pvalue_threshold default: 0.05
#' @return tbl_df 
#'
prepare_paea_results <- function(paea_pvalues, data_description, pvalue_threshold=0.05) {
    dplyr::tbl_df(data.frame(
        set=colnames(paea_pvalues),
        pvalue=as.vector(paea_pvalues)
    )) %>% 
        dplyr::filter(pvalue <= pvalue_threshold) %>%
        dplyr::mutate(neg_log10_pval = -log10(pvalue)) %>%
        tidyr::separate(set, into=c('id', 'category'), sep='_') %>% 
        dplyr::mutate(id=as.numeric(id)) %>%
        dplyr::left_join(data_description, by='id') %>%
        dplyr::select(id, category, geo_id:cell_type, neg_log10_pval)
}

#' PAEAAnalysis wrapper. Redirects plots to /dev/null 
#'
#' @param chdirresults see GeoDE::PAEAAnalysis 
#' @param gmtfile see GeoDE::PAEAAnalysis 
#' @param gammas see GeoDE::PAEAAnalysis 
#' @param casesensitive see GeoDE::PAEAAnalysis 
#' @return paea results
#'

paea_analysis_wrapper <- function(chdirresults, gmtfile, gammas = c(1), casesensitive = FALSE){
    if(length(gmtfile) > 0 & length(chdirresults) > 0) {
        png('/dev/null')
        paea <- GeoDE::PAEAAnalysis(chdirresults, gmtfile, gammas, casesensitive, showprogress=TRUE)
        dev.off()
        paea
    } else {
        warning('Cannot run paea with on empty input.')
        NULL
    }
}


#' Split chdir vector into negative and postive (plus zero) subvectors
#' 
#' @param chdirresults see GeoDE::PAEAAnalysis 
#' @return list with up and down fields
#'
split_chdirresults <- function(chdirresults) {
    # Split chdir results into up and down
    chdirresults_mask <- chdirresults[[1]][[1]][, 1] < 0
    chdirresults_up <-  list(list(chdirresults[[1]][[1]][!chdirresults_mask, , drop=FALSE]))
    chdirresults_down <- list(list(chdirresults[[1]][[1]][chdirresults_mask, , drop=FALSE]))
    list(up=chdirresults_up, down=chdirresults_down)
}


#' Split gmt file into up and down based on _downregulated suffix
#' 
#' @param gmtfile see GeoDE::chdirAnalysis
#' @return list with up and down sets 
#' 
split_gmtfile <- function(gmtfile) {
    gmtfile_mask <- unlist(lapply(gmtfile, function(x) { stringi::stri_endswith_fixed(x[1], '_downregulated') }))
    gmtfile_up <- gmtfile[!gmtfile_mask]
    gmtfile_down <- gmtfile[gmtfile_mask]
    list(up=gmtfile_up, down=gmtfile_down)
}


#' PAEAAnalysis dispatch function. Should handle separating chdirresults into up and down
#' and in future some filtering steps
#' 
#' @param chdirresults see GeoDE::PAEAAnalysis 
#' @param gmtfile see GeoDE::PAEAAnalysis 
#' @param gammas see GeoDE::PAEAAnalysis 
#' @param casesensitive see GeoDE::PAEAAnalysis 
#' @param strategy character one of {'split_both', ...}
#' @return list with paea results
#'
paea_analysis_dispatch <- function(chdirresults, gmtfile, gammas = c(1), casesensitive = FALSE, strategy='up_up+down_down'){    
    
    #' Split strategy string into individual components.
    #' Each component represents single paea run.
    #' 
    tasks <- unlist(stringi::stri_split_regex(strategy, '[+-]'))
    
    #' Preprocess query and background
    #'
    chdirresults_splitted <- split_chdirresults(chdirresults)
    gmtfile_splitted <- split_gmtfile(gmtfile)
    
    
    
    lapply(stringi::stri_split_fixed(tasks, '_'), function(task) {
        stopifnot(length(task) == 2)
        paea_analysis_wrapper(
            chdirresults=chdirresults_splitted[[task[1]]],
            gmtfile=gmtfile_splitted[[task[2]]],
            gammas=gammas,
            casesensitive=casesensitive
        )
    }) %>% setNames(tasks)
}
