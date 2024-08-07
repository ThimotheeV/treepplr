% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treepplr_example.R
\name{tp_example}
\alias{tp_example}
\title{TreePPL example files}
\usage{
tp_example(path = NULL)
}
\arguments{
\item{path}{Optional. Name of example file. If NULL, returns a list of all example files.
in the \code{extdata} directory.}
}
\value{
A list of files or a path to one example file.
}
\description{
TreePPL example files
}
\examples{
tp_example()
tp_example("coin.json")
}
