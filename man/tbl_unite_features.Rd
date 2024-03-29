% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tbl_unite_features}
\alias{tbl_unite_features}
\title{Unite Features}
\usage{
tbl_unite_features(
  data,
  unite_columns = NULL,
  new_column = NULL,
  sep = "@",
  to_factor = TRUE,
  remove = F
)
}
\arguments{
\item{data}{a `tibble`}

\item{unite_columns}{vector of columns to unite}

\item{new_column}{name of new column.  If `NULL` concats the unite coljumns}

\item{sep}{default `@`}

\item{remove}{if `TRUE` removes original columns}
}
\description{
Unite Features
}
\examples{
library(tidyverse)
tbl_unite_features(diamonds, unite_columns = c("cut", "color", "clarity"))
}
