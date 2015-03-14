#' Convert list of params to list of def tags
#' 
#' @param params list
#' @return container html element containing (dt, dd) pair for each element of the list
#'
list_to_defs <- function(params, container=shiny::div) {
    params <- lapply(params, paste, collapse=', ')
    names(params) <- stringi::stri_trans_totitle(stringi::stri_replace_all_fixed(names(params), '_', ' '))
    container(lapply(
        names(params),
            function(x) {
                shiny::span(shiny::tags$dt(x), shiny::tags$dd(params[[x]]))
            }
        )
    )
}

#' Return disabled button
#' 
#' @param shiny::actionButton
#' @return shiny::actionButton
#'
disabledActionButton <- function(button) {
    button$attribs$disabled <- 'true'
    button
}


#' Return disabled numericInput
#' 
#' @param shiny::disabledNumericInput
#' @return shiny::disabledNumericInput
#'
disabledNumericInput <- function(numericInput) {
    numericInput$children[[2]]$attribs$disabled <- 'true'
    numericInput
}