% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aliases.R
\name{pipe_set_data_split}
\alias{pipe_set_data_split}
\title{Split-multiply pipeline by list of data sets}
\usage{
pipe_set_data_split(
  pip,
  dataList,
  toStep = character(),
  groupBySplit = TRUE,
  sep = "."
)
}
\arguments{
\item{pip}{\code{Pipeline} object}

\item{dataList}{\code{list} of data sets}

\item{toStep}{\code{string} step name marking optional subset of
the pipeline, to which the data split should be applied to.}

\item{groupBySplit}{\code{logical} whether to set step groups according
to data split.}

\item{sep}{\code{string} separator to be used between step name and
data set name when creating the new step names.}
}
\value{
new combined \code{Pipeline} with each sub-pipeline having set
one of the data sets.
}
\description{
This function can be used to apply the pipeline
repeatedly to various data sets. For this, the pipeline split-copies
itself by the list of given data sets. Each sub-pipeline will have
one of the data sets set as input data.
The step names of the sub-pipelines will be the original
step names plus the name of the data set.
}
\examples{
# Split by three data sets
dataList <- list(a = 1, b = 2, c = 3)
p <- pipe_new("pipe")
pipe_add(p, "add1", \(x = ~data) x + 1, keepOut = TRUE)
pipe_add(p, "mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
pipe_set_data_split(p, dataList)
p

p |> pipe_run() |> pipe_collect_out() |> str()

# Don't group output by split
p <- pipe_new("pipe")
pipe_add(p, "add1", \(x = ~data) x + 1, keepOut = TRUE)
pipe_add(p, "mult", \(x = ~data, y = ~add1) x * y, keepOut = TRUE)
pipe_set_data_split(p, dataList, groupBySplit = FALSE)
p

p |> pipe_run() |> pipe_collect_out() |> str()

# Split up to certain step
p <- pipe_new("pipe")
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "mult", \(x = ~data, y = ~add1) x * y)
pipe_add(p, "average_result", \(x = ~mult) mean(unlist(x)), keepOut = TRUE)
p
pipe_get_depends(p)[["average_result"]]

pipe_set_data_split(p, dataList, toStep = "mult")
p
pipe_get_depends(p)[["average_result"]]

p |> pipe_run() |> pipe_collect_out() |> str()
}
