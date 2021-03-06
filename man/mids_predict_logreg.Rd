% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mids_predict_logreg.r
\name{mids_predict_logreg}
\alias{mids_predict_logreg}
\title{Predict from imputed data}
\usage{
mids_predict_logreg(object, formula, newdata)
}
\arguments{
\item{object}{a mids object}

\item{formula}{formula for the logistic regression}

\item{newdata}{data for which predictions are wanted}
}
\description{
Predict from imputed data sets from a \code{mids} object and a
logistic regerssion model
}
\note{
It would be nice to include the equivalent of parameter \code{se.fit}
from other prediction methods.
}
\author{
Henrik
}

