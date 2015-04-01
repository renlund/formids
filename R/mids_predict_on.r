#' @title Predict on imputed data
#' @description Predict from an object (model) on a \code{mids} object.
#' @note It would be nice to include the equivalent of parameter \code{se.fit}
#' from other prediction methods.
#' @param object prediction object (=model, typically)
#' @param mids mids object to be predicted on.
#' @param ... arguments passed to predict
#' @author Henrik Renlund
#' @export

mids_predict_on <- function(object, mids, ...){
    if(!"mids" %in% class(mids)) warning("[mids_predict_on] object class not 'mids'")
    m <- mids$m
    temp <- matrix(NA_real_, nrow=nrow(mids$data), ncol=m)
    for(index in 1:m){
        temp[,index] <- predict(object = object, newdata = mids_get(object = mids, m = index), ...)
    }
    rowMeans(temp)
}
