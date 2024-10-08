% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getter.R
\name{tp_phyjson_tree}
\alias{tp_phyjson_tree}
\title{Create a phyjson tree object}
\usage{
tp_phyjson_tree(...)
}
\value{
A phyjson tree object (S3)
}
\description{
\code{tp_phyjson_tree} takes a variable number of argument and return a phyjson tree object.
}
\details{
This function takes a variable number of argument, so that users can pass as arguments
either independent lists for each parameter or a single structured list of list.
it's use like a list (name_arg = value_arg, etc)
}
