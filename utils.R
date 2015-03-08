#' Convert list of params to list of def tags
#' 
#' @param params list
#' @return shiny::div
#'
render_params <- function(params) {
    shiny::div(lapply(
        names(params),
            function(x) {
                shiny::span(
                    shiny::tags$dt(
                        stringi::stri_trans_totitle(stringi::stri_replace_all_fixed(x, '_', ' '))),
                    shiny::tags$dd(paste(params[[x]], collapse = ', '))
                )
            }
        )
    )
}

#' Return disabled button
#' 
#' @return shiny::actionButton
#'
disabledActionButton <- function(...) {
    shiny::actionButton(..., disabled='true')
}