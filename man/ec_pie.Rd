% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ec_series.R
\name{ec_pie}
\alias{ec_pie}
\title{Pie series}
\usage{
ec_pie(df, ...)
}
\arguments{
\item{df}{A dataframe that can be referenced by all other arguments}

\item{...}{additional expressions providing series attributes and additional dimensions

Unnamed arguments will be used as additional data dimensions in the order provided.}
}
\description{
https://echarts.apache.org/en/option.html#series-pie
}
\examples{

data <- tibble::tibble(name = LETTERS[1:5], value = rnorm(5))
series <- ec_pie(data, value, data = list(name = name))

echartr(option = list(
  series = series
))

}
\seealso{
\code{\link[=ec_series]{ec_series()}}
}
