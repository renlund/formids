#' @title Extract imputed data sets
#' @description Extract imputed data sets from a \code{mids} object
#' @note This functionality already exists in \code{mice::complete}!
#' @param object a mids object
#' @param m imputation index
#' @author Henrik Renlund
#' @export

mids_get <- function(object, m){
    if(!"mids" %in% class(object)) warning("[mids_get] object is not of class 'mids'")
    DF <- object$data
    for(K in names(object$imp)){
        indx <- is.na(DF[[K]])
        if(!any(indx)) next
        DF[[K]][indx] <- object$imp[[K]][[m]]
    }
    DF
}
