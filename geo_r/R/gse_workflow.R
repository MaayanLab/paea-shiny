#' Download GSE file with annotations and extract expression data
#' 
#' @param geo_id character
#' @param destdir cache directory
#' @param outputdir place to store output files
#' @param control vector of sample ids
#' @param treatment vector of sample ids
#' @param description dataset description
#' @param dryrun logical should we run process_eset
#' @return vector of output files
#'
process_gse <- function(geo_id, destdir=NULL, outputdir=NULL, control=NULL, treatment=NULL, description=NULL, dryrun=FALSE) {    
    destdir <- if(is.null(destdir)) { tempdir() } else { destdir }
    outputdir <- if(is.null(outputdir)) { tempdir() } else { outputdir }
    
    stopifnot(length(destdir) == 1)
    stopifnot(length(outputdir) == 1)
    
    
    # Download / load GSE file
    gse <- GEOquery::getGEO(GEO=geo_id, destdir=destdir, AnnotGPL=TRUE, getGPL=TRUE)
    
    # Extract esets names
    output_names <- file.path(outputdir, paste(names(gse), 'rds', sep='.'))
    
    if(!dryrun) {
        # Process esets
        expr <- lapply(gse, process_eset, geo_id=geo_id, control=control, treatment=treatment)
    
        #Write results
        mapply(function(e, f) saveRDS(e, f), expr, output_names)
    }
    
    output_names 
}

#' Process eset
#' 
#' @param eset Biobase::ExpressionSet
#' @param control vector of sample ids
#' @param treatment vector of sample ids
#' @param description dataset description
#' @return data.table
#'
process_eset <- function(eset, geo_id=NULL, control=NULL, treatment=NULL, description=NULL) {
    exprs <- Biobase::exprs(eset) %>%
        as.data.table() %>%
        dplyr::tbl_dt() %>%
        dplyr::mutate(ID_REF=rownames(Biobase::exprs(eset)))
    
    annot <- Biobase::fData(eset) %>%
        as.data.table() %>%
        dplyr::tbl_dt() %>%
        dplyr::select(ID_REF=ID, IDENTIFIER=`Gene symbol`)
    
    combined <- exprs %>% 
        dplyr::left_join(annot, by='ID_REF') %>% 
        dplyr::select(ID_REF, IDENTIFIER, starts_with('GSM')) %>% 
        dplyr::filter(IDENTIFIER != '')
    
    attributes(combined)$gse_data <- list(geo_id=geo_id, control=control, treatment=treatment, description=description)
    
    combined 
}