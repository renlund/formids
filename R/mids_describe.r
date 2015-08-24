#' @title Describe an imputed data set
#' @description Describe complete case, missing and imputed via ucR::ucr.base.tab
#' @note Currently this function utilizes the \code{ucR::ucr.base.tab} function with
#' a certain parameters. It is unclear how easy it would be to allow the
#' passing of arguments from \code{mids_describe} to this function...
#' @param object a mids object
#' @param x.names character vector of variables of interest
#' @param file file to write latex output to (empty string by default)
#' @param force.factor a character vector of variables to be forced into factors
#' (e.g. event variables that are coded as 0/1)
#' @param ... arguments passed to \code{Hmisc::latex}
#' @param factorize force character vectors to be factors? (if FALSE these
#' variables are removed)
#' @param digits the number of digits (median and IQR for imputed
#' variable description)
#' @param silent hide message on removed variables?
#' @return LaTeX code for descriptive table
#' @author Henrik Renlund
#' @export

mids_describe <- function(object, x.names, file="", ..., force.factor=NULL, factorize = TRUE, digits=1, silent = TRUE){
    # fix/check argument ------------
    if(!"mids" %in% class(object)) warning("[mids_describe] object class not 'mids'")
    if(missing(x.names)) {
        x.names <- names(object$data)
    } else {
        if(!all(x.names %in% names(object$data))) stop("[mids_describe] bad x.names argument")
    }
    if(!is.null(force.factor))
        if(!all(force.factor %in% x.names))
            stop("[mids_describe] bad force.factor argument")
    raw <- subset(object$data, TRUE, select=x.names)
    # eliminate bad table variables ------
    for(K in x.names){ # K = x.names[1]
        if(any(class(raw[[K]]) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt", "POSIXt"))){
            raw[[K]] <- NULL
            if(!silent) message(paste0("[mids_describe] variable '", K, "' is a date and removed"))
            next
        }
        if(K %in% force.factor){
            if(!is.factor(raw[[K]])){
                raw[[K]] <- factor(raw[[K]])
            }
            next
        }
        if(is.character(raw[[K]])){
            if(factorize){
                raw[[K]] <- factor(raw[[K]])
            } else {
                raw[[K]] <- NULL
                if(!silent) message(paste0("[mids_describe] variable '", K, "' is a character and removed"))
            }
        }
    }

    while((miss_ind <- paste0(sample(c(letters,LETTERS), 10), collapse="")) %in% names(raw)){
        "This part just creates a variable name that does not already exist in the dataset"
    }
    raw[[miss_ind]] <- factor(ifelse(complete.cases(raw), "Complete", "Missing"))
    COCA <- subset(raw,  complete.cases(raw))
    MISS <- subset(raw, !complete.cases(raw))

    bt <- ucR::ucr.base.tab(data = raw,
                       group.name = miss_ind,
                       include.p = FALSE,
                       include.combined = FALSE,
                       show.missing = "in.row",
                       include.n = FALSE)
    bt_var <- bt$tab[,1]
    Imputation <- rep(NA_character_, length(bt_var))
    medShift_1 <- rep(NA_character_, length(bt_var))
    medShift_2 <- rep(NA_character_, length(bt_var))
    place <- function(s, x = bt$tab, num = TRUE){
        if(num){
            indx <- which(x %in% s)
        } else {
            indx <- grep(s, x)
            ##indx <- grep(s, strsplit(x = x, split = ":"))
        }
        if(length(indx) == 1) return(indx)
        indx <- which(substr(x, 1, length(s)) %in% s)
        if(length(indx) == 1) return(indx)
        stop("[mids_describe] cannot find variable location... (ish)")
    }
    for(K in names(object$imp)){ # K = names(object$imp)[6]   K = names(object$imp)[8]
        if(!K %in% x.names) next
        temp_var <- object$imp[[K]]
        if(is.null(temp_var)) next
        if(is.numeric(raw[[K]])){
            indx <- place(K)
            tmp <- as.numeric(as.matrix(object$imp[[K]]))
            Q2 <- round(median(tmp), digits)
            Q1 <- round(quantile(tmp, probs=c(0.27)), digits)
            Q3 <- round(quantile(tmp, probs=c(0.75)), digits)
            Imputation[indx] <- paste0(Q2, " (", Q1, " - ", Q3, ")")
            iqr_coca <- IQR(COCA[[K]])
            med_coca <- median(COCA[[K]])
            iqr_miss <- IQR(MISS[[K]], na.rm=TRUE)
            med_miss <- median(MISS[[K]], na.rm=TRUE)
            medShift_1[indx] <- round((med_miss - med_coca) / iqr_coca, digits+1 )
            medShift_2[indx] <- round((median(tmp) - med_miss) / iqr_miss, digits+1 )
            next
        }
        if(is.factor(raw[[K]]) | is.character(raw[[K]])){
            indx <- place(s = K, num = FALSE)
            tmp <- as.character(as.matrix(object$imp[[K]]))
            if(is.factor(raw[[K]])) tmp <- factor(tmp, levels=levels(raw[[K]]))
            tmp_tab <- round(table(tmp) / length(tmp), digits+2)
            Imputation[indx:(indx+length(tmp_tab)-1)] <- paste0(100*tmp_tab, "%")
        }
    }
    bt$tab <- cbind(bt$tab, "$\\delta_1$" = medShift_1, "Imputed values" = Imputation, "$\\delta_2$" = medShift_2)
    bt$extra.col.heads <- c(bt$extra.col.heads, " ", paste0("$", object$m, " \\times$ missing/variable"), " ")
    ucR::latex.ucr.base.tab(bt, file = file, ...)
}

