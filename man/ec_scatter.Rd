% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ec_series.R
\name{ec_scatter}
\alias{ec_scatter}
\title{Scatter series}
\usage{
ec_scatter(df, ...)
}
\arguments{
\item{df}{A dataframe that can be referenced by all other arguments}

\item{...}{additional expressions providing series attributes and additional dimensions

Unnamed arguments will be used as additional data dimensions in the order provided.}
}
\description{
Generates a single scatter series or list of series from a dataframe.
}
\details{
https://echarts.apache.org/en/option.html#series-scatter
}
\seealso{
\code{\link[=ec_series]{ec_series()}}
}
