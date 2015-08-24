#' @title Predict from imputed data
#' @description Predict from imputed data sets from a \code{mids} object and a
#' logistic regerssion model
#' @note It would be nice to include the equivalent of parameter \code{se.fit}
#' from other prediction methods.
#' @param object a mids object
#' @param formula formula for the logistic regression
#' @param newdata data for which predictions are wanted
#' @author Henrik
#' @import mice
#' @export

mids_predict_logreg <- function(object, formula, newdata){
    if(!"mids" %in% class(object)) warning("[mids_predict_logreg] object class not 'mids'")
    prob_function <- function(x) exp(x)/(1+exp(x))
    code <- paste0("glm(", formula, ", family='binomial')")
    mira <- eval(parse(text=paste0("with(object, ",code,")")))
    mipo <- mice::pool(object = mira)
    coef <- mipo$qbar
    newdata <- subset(newdata, TRUE) # NEW: somewhat inelegant way of making sure
    # that newdata does not have weird attributes
    # that makes a mess... (which has been know
    # to happen)
    mod_mat <- model.matrix(object = as.formula(formula), data = newdata)
    fail <- "[mids_predict_logreg] something is wrong ... "
    tryCatch(expr = sum(mod_mat[1,] * coef), warning = function(w) stop(fail), error = function(e) stop(fail))
    S <- rep(0, nrow(mod_mat))
    for(k in 1:ncol(mod_mat[,,drop=FALSE])){ # k = 1
        S <- S + coef[k] * mod_mat[, k]
    }
    ret <- data.frame(
        "linear_pred" = rep(NA_real_, nrow(newdata)),
        "prob_pred" = rep(NA_real_, nrow(newdata))
    )
    rownames(ret) <- rownames(newdata)
    index1 <- rownames(newdata)
    index2 <- rownames(mod_mat)

    ret$linear_pred[index1 %in% index2] <- S
    ret$prob_pred[index1 %in% index2] <- prob_function(S)
    ret
}
