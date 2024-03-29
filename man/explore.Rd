% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore.R
\name{explore}
\alias{explore}
\title{Explore and summarize data}
\usage{
explore(
  dataset,
  vars = "",
  byvar = "",
  fun = c("mean", "sd"),
  top = "fun",
  tabfilt = "",
  tabsort = "",
  tabslice = "",
  nr = Inf,
  data_filter = "",
  arr = "",
  rows = NULL,
  envir = parent.frame()
)
}
\arguments{
\item{dataset}{Dataset to explore}

\item{vars}{(Numeric) variables to summarize}

\item{byvar}{Variable(s) to group data by}

\item{fun}{Functions to use for summarizing}

\item{top}{Use functions ("fun"), variables ("vars"), or group-by variables as column headers}

\item{tabfilt}{Expression used to filter the table (e.g., "Total > 10000")}

\item{tabsort}{Expression used to sort the table (e.g., "desc(Total)")}

\item{tabslice}{Expression used to filter table (e.g., "1:5")}

\item{nr}{Number of rows to display}

\item{data_filter}{Expression used to filter the dataset before creating the table (e.g., "price > 10000")}

\item{arr}{Expression to arrange (sort) the data on (e.g., "color, desc(price)")}

\item{rows}{Rows to select from the specified dataset}

\item{envir}{Environment to extract data from}
}
\value{
A list of all variables defined in the function as an object of class explore
}
\description{
Explore and summarize data
}
\details{
See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
}
\examples{
explore(diamonds, c("price", "carat")) \%>\% str()
explore(diamonds, "price:x")$tab
explore(diamonds, c("price", "carat"), byvar = "cut", fun = c("n_missing", "skew"))$tab

}
\seealso{
See \code{\link{summary.explore}} to show summaries
}
