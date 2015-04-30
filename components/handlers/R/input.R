#' Send disable message to the frontend js
#'
#' @param id character css id selector excluding #
#' @param session shiny session
#'
disableButton <- function(id, session, uncheck=FALSE) {
    session$sendCustomMessage('button_status_message', list(id=id, disable=TRUE, uncheck=uncheck))
}


#' Send enable message to the frontend js
#'
#' @param id character css id selector excluding #
#' @param session shiny session
#'
enableButton <- function(id, session) {
    session$sendCustomMessage('button_status_message', list(id=id, disable=FALSE))
}
    
