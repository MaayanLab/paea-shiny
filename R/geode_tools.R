#' Extract ith element of characteristic
#' direction properties list from
#' chdirAnalysis results
#"
#' @param chdir chdirAnalysis output
#' @param i integer 
#' @return matrix #{number of genes} * 1 matrix
#'
chdir_props <- function(chdir, i=1) {
    chdir$chdirprops$chdir[[i]]
}


#' Extract ith element of the results list from
#' chdirAnalysis results
#'
#' @param chdir chdirAnalysis output
#' @param i integer 
#' @return vector of length <= #{number of genes}
#'
chdir_results <- function(chdir, i=1) {
    chdir$results[[i]]
}




