#' @title Predict from imputed data on imputed data
#' @description Predict from imputed data sets from a \code{mids} object and a
#' logistic regerssion model
#' @param object a mids object
#' @param formula formula for the logistic regression
#' @param newmids data for which predictions are wanted
#' @note It would be very nice to be able to include \code{se.fit}...
#' @author Henrik Renlund
#' @export

mids_predict_logreg_2 <- function(object, formula, newmids){
    if(!"mids" %in% class(object)) warning("[mids_predict_logreg_2] object class not 'mids'")
    if(!"mids" %in% class(newmids)) warning("[mids_predict_logreg_2] object class not 'mids'")
    prob_function <- function(x) exp(x)/(1+exp(x))
    m <- newmids$m
    r <- nrow(newmids$data)
    linear <- matrix(NA_real_, nrow=r, ncol=m)
    probs <- matrix(NA_real_, nrow=r, ncol=m)
    for(k in 1:m){
        tmp <- mids_predict_logreg(object = object,
                                   formula = formula,
                                   newdata = mids_get(object = newmids, m=k))
        linear[,k] <- tmp$linear_pred
    }
    linear_pred <- rowMeans(linear)
    data.frame(
        linear_pred = linear_pred,
        prob_pred = prob_function(linear_pred)
    )
}
