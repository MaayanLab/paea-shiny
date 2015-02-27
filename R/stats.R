#' Count number of submissions per user
#' 
#' @param list of dataframes as returned from preprocess
#' @return data.frame with curator and n
#'
submissions_per_user <- function(dataset) {
    dataset$description %>% group_by(curator) %>% summarise(n=n())
}
