#' Count number of submissions per user
#' 
#' @param list of dataframes as returned from preprocess
#' @return data.frame with curator and n
#'
submissions_per_user <- function(dataset) {
    dataset$description %>% group_by(curator) %>% summarise(n=n())
}

#' Count number of submissions per cell type
#' 
#' @param list of dataframes as returned from preprocess
#' @return data.frame with cell_type and n
#'
submissions_per_cell_type <- function(dataset) {
    dataset$description %>% group_by(cell_type) %>% summarise(n=n())
}


#' Count number of submissions per organism
#' 
#' @param list of dataframes as returned from preprocess
#' @return data.frame with organism and n
#'
submissions_per_organism <- function(dataset) {
    dataset$description %>% group_by(organism) %>% summarise(n=n())
}
