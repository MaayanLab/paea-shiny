#' Prepare input for Enrichr form
#'
#' @param chdir_results data.frame as returned from prepare_results
#' @return character 
#'
prepare_enrichr_input <- function(chdir_results) {
    paste(apply(chdir_results %>% dplyr::mutate(v = abs(v)), 1, paste, collapse=','), collapse = '\n')
}


#' Download file if required and return path to directory
#'
#' @param sigs_path character
#' @param choice character
#' @return character
#'
get_path <- function(sigs_path, choice) {
    if(stringi::stri_startswith_fixed(sigs_path, 'http')) {
        download.file(file.path(sigs_path, choice), file.path(tempdir(), choice))
        file.path(tempdir(), choice)
    } else {
        file.path(sigs_path, choice)
    }
}
