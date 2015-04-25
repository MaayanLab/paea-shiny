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

#' Get latest modification date
#'
#' @return character representation of the date
#'
get_last_modified <- function() {
    sort(
        sapply(
            list.files(recursive=TRUE),
            function(x) strftime(file.info(x)$mtime)
        ),
        decreasing=TRUE
    )[1]
}
