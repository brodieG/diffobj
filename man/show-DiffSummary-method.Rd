% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summmary.R
\name{show,DiffSummary-method}
\alias{show,DiffSummary-method}
\title{Display DiffSummary Objects}
\usage{
\S4method{show}{DiffSummary}(object)
}
\arguments{
\item{object}{a \code{DiffSummary} object}
}
\value{
NULL, invisbly
show(
  summary(diffChr(letters, letters[-c(5, 15)], format="raw", pager="off"))
)
}
\description{
Display DiffSummary Objects
}
