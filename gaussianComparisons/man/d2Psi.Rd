% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d2Psi.R
\name{d2Psi}
\alias{d2Psi}
\title{Second derivative of the log of the numerator of the full posterior function (Equation 9)}
\usage{
d2Psi(omega, g, y)
}
\arguments{
\item{omega}{A matrix which contains information about how a document is compared with all other documents.}

\item{g}{An unknown vector of real numbers}

\item{y}{A vector of outcomes of pairwise comparison of documents}
}
\value{
A matrix representing the second derivative of psi
}
\description{
Adds to functions that are already created (derivative of log prior, derivative of log likelihood)
}
\author{
Group <\email{group@wustl.edu}
}
