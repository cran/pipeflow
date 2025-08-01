% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aliases.R
\name{pipe_get_data}
\alias{pipe_get_data}
\title{Get data}
\usage{
pipe_get_data(pip)
}
\arguments{
\item{pip}{\code{Pipeline} object}
}
\value{
the output defined in the \code{data} step, which by default is
the first step of the pipeline
}
\description{
Get the data set for the pipeline
}
\examples{
p <- pipe_new("pipe", data = 1:2)
pipe_get_data(p)
pipe_set_data(p, 3:4)
pipe_get_data(p)
}
