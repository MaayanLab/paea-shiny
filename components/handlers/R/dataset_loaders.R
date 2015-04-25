#' Take list of input paths and load data using reactive expressions
#' 
#' @param datasets character
#' @return list of reactive expressions
#'
load_perturbations_data <- function(datasets) {
    load_reactive <- function(dataset) {
        force(dataset)
        shiny::reactive({ nasbMicrotaskViewerHelpers::preprocess(dataset$path, drop_duplicates=config$drop_duplicates) })
    }
    lapply(datasets, load_reactive)
}
