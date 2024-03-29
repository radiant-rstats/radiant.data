% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{as_integer}
\alias{as_integer}
\title{Convert variable to integer avoiding potential issues with factors}
\usage{
as_integer(x)
}
\arguments{
\item{x}{Input variable}
}
\value{
Integer
}
\description{
Convert variable to integer avoiding potential issues with factors
}
\examples{
as_integer(rnorm(10))
as_integer(letters)
as_integer(as.factor(5:10))
as.integer(as.factor(5:10))
as_integer(c("a", "b"))
as_integer(c("0", "1"))
as_integer(as.factor(c("0", "1")))

}
