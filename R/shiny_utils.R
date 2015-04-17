#' Convert list of params to list of def tags
#' 
#' @param params list
#' @return container html dl containing (dt, dd) pair for each element of the list
#'
list_to_defs <- function(params) {
    params <- lapply(params, paste, collapse=', ')
    names(params) <- stringi::stri_trans_totitle(stringi::stri_replace_all_fixed(names(params), '_', ' '))
    shiny::tags$dl(lapply(
        names(params),
        function(x) {
            shiny::span(shiny::tags$dt(x), shiny::tags$dd(params[[x]]))
        }
    ))
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


#' Create error handler function
#' 
#' @param shiny::reactiveValues
#' @return function which stores error in values$last_error
#'
shiny_error_handler <- function(values) {
    function(e) {
        values$last_error <- e
        logging::logerror(e$message)
    }
}


#' Take list with name and href element and return
#' <li><a href='href'>name</a></li>
#' 
#' @param navbar_tab list
#' @param this_href character
#' @return shiny::tags$li
#' 
shiny_list_to_li <- function(navbar_tab, this_href) {
    stopifnot(length(navbar_tab) == 2)

    anchor <- shiny::a(navbar_tab$name, href=navbar_tab$href)

    if(identical(this_href, navbar_tab$href)) {
        shiny::tags$li(class='active', anchor)
    } else {
        shiny::tags$li(anchor)
    }
}


#' Create "static" boostrap navbar
#' 
#' @param navbar_brand character page brand
#' @param navbar_tabs list of lists each containing name and href
#' @param this_href character current url
#' @return shiny::tags$nav
#'
shiny_static_navbar <- function(navbar_brand, navbar_tabs, this_href='/') {
    shiny::tags$nav(
        class='navbar navbar-default navbar-static-top',
        role='navigation',
        shiny::div(
            class='container',
            shiny::div(
                class='navbar-header',
                shiny::span(
                    class='navbar-brand',
                    navbar_brand
                )
            ),
            shiny::tags$ul(
                class='nav navbar-nav',
                lapply(navbar_tabs, shiny_list_to_li, this_href)
            )
        )
    )
}



