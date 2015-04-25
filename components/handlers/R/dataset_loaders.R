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


#' Load disease signatures
#'
#' @return list with sigs and choices fields
#'
load_disease_sigs <- function(sigs_list_path) {
    disease_sigs <- data.table::fread(sigs_list_path)
    disease_sigs_choices <- setNames(
        disease_sigs$file_name,
        paste(disease_sigs$disease, disease_sigs$cell_type, disease_sigs$geo_id, sep = ' | ')
    )
    list(sigs=disease_sigs, choices=disease_sigs_choices)
}
