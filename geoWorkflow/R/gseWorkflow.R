#' Download GSE file with annotations and extract expression data
#' 
#' @param geo_id character
#' @param destdir cache directory
#' @param outputdir place to store output files
#' @param control vector of sample ids
#' @param treatment vector of sample ids
#' @return vector of output files
#'
process_gse <- function(geo_id, destdir, outputdir, control=NULL, treatment=NULL) {
    # Download / load GSE file
    gse <- GEOquery::getGEO(GEO=geo_id, destdir=destdir, AnnotGPL=TRUE, getGPL=TRUE)
    
    # Extract esets names
    output_names <- file.path(outputdir, paste(names(gse), 'rds', sep='.'))
    
    # Process esets
    expr <- lapply(gse, process_eset)
    
    #Write results
    mapply(function(e, f) saveRDS(e, f), expr, output_names)
    output_names 
}

#' Process eset
#' 
#' @param eset Biobase::ExpressionSet
#' @param control vector of sample ids
#' @param treatment vector of sample ids
#' @return data.table
#'
process_eset <- function(eset, control=NULL, treatment=NULL) {
    exprs <- Biobase::exprs(eset) %>%
        as.data.table() %>%
        tbl_dt() %>%
        mutate(ID_REF=rownames(Biobase::exprs(eset)))
    
    annot <- Biobase::fData(eset) %>%
        as.data.table() %>%
        tbl_dt() %>%
        select(ID_REF=ID, IDENTIFIER=`Gene symbol`)
    
    combined <- exprs %>% 
        left_join(annot, by='ID_REF') %>% 
        select(ID_REF, IDENTIFIER, starts_with('GSM')) %>% 
        filter(IDENTIFIER != '')
    
    attributes(combined)$gse_data <- list(control=control, treatment=treatment)
    
    combined 
}