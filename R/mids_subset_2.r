#' @title Subset imputed data sets
#' @description Subset imputed data sets from a \code{mids} object.
#' If the subsetting index is proper, this function will call \code{mids_subset} and thus \code{index} can be integer, logical or character. For improper subsets the index must be integer, and the \code{object$data} will get its rownames set to \code{1:nrow(object$data)} (for ease of handling).
#' @param object a mids object
#' @param index the rows you want (must be integer for improper subsets)
#' @author Henrik Renlund
#' @export

mids_subset_2 <- function(object, index){
    if(!"mids" %in% class(object)) warning("[mids_subset] object is not of class 'mids'")
    n <- nrow(object$data)
    naro <- row.names(object$data)
    if(length(unique(index)) == length(index)) return(mids_subset(object, index))
    if(!is.numeric(index)) stop("[mids_subset_2] need numeric index")
    tryCatch(naro_int <- as.numeric(naro), warning = function(w) NULL)
    if(any(is.na(naro_int)) | any(naro_int != 1:n) ){
        warning("[mids_subset_2] row names should be the integers starting from 1. Will change this")
        for(K in names(object$imp)){
            tmp <- object$imp[[K]]
            if(is.null(tmp)) next
            row.names(object$imp[[K]]) <- which(naro %in% row.names(tmp))
        }
        row.names(object$data) <- 1:n
    }
    new_object <- object
    new_object$data <- object$data[index, ]

    for(K in names(object$imp)){
        tmp <- object$imp[[K]]
        if(is.null(tmp)) next
        replacer <- tmp[as.character(index[which(index %in% row.names(tmp))]), ]
        if(nrow(replacer)==0){
            new_object$imp[[K]] <- NULL
        } else {
            new_object$imp[[K]] <- replacer
        }
    }
    class(new_object) <- c("mids_subset", class(object))
    new_object
}
