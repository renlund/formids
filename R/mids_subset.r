#' @title Proper subset of imputed data sets
#' @description Make a proper subset imputed data sets from a \code{mids} object
#' @param object a mids object
#' @param index the rows you want (can be a logical expression or character vector)
#' @author Henrik Renlund
#' @export

mids_subset <- function(object, index){
    if(!"mids" %in% class(object)) warning("[mids_subset] object is not of class 'mids'")
    if(is.logical(index)) index <- which(index)
    if(any(duplicated(index))) stop("[mids_subset] no duplicates")
    new_object <- object
    new_object$data <- object$data[index, ]
    for(K in names(object$imp)){
        tmp <- object$imp[[K]]
        if(is.null(tmp)) next
        replacer <- tmp[rownames(tmp) %in% row.names(new_object$data),]
        if(nrow(replacer)==0){
            new_object$imp[K] <- list(NULL)
        } else {
            new_object$imp[[K]] <- replacer
        }
    }
    class(new_object) <- c("mids_subset", class(object))
    new_object
}
