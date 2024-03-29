% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radiant.R
\name{add_description}
\alias{add_description}
\title{Convenience function to add a markdown description to a data.frame}
\usage{
add_description(df, md = "", path = "")
}
\arguments{
\item{df}{A data.frame or tibble}

\item{md}{Data description in markdown format}

\item{path}{Path to a text file with the data description in markdown format}
}
\description{
Convenience function to add a markdown description to a data.frame
}
\examples{
if (interactive()) {
  mt <- mtcars |> add_description(md = "# MTCARS\n\nThis data.frame contains information on ...")
  describe(mt)
}

}
\seealso{
See also \code{\link{register}}
}
