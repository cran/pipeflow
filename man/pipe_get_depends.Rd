% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aliases.R
\name{pipe_get_depends}
\alias{pipe_get_depends}
\alias{pipe_get_depends_down}
\alias{pipe_get_depends_up}
\title{Get step dependencies}
\usage{
pipe_get_depends(pip)

pipe_get_depends_down(pip, step, recursive = TRUE)

pipe_get_depends_up(pip, step, recursive = TRUE)
}
\arguments{
\item{pip}{\code{Pipeline} object}

\item{step}{\code{string} name of step}

\item{recursive}{\code{logical} if \code{TRUE}, dependencies of dependencies
are also returned.}
}
\value{
\itemize{
\item \code{pipe_get_depends}: named list of dependencies for each step
\item \code{pipe_get_depends_down}: list of downstream dependencies
\item \code{pipe_get_depends_up}: list of downstream dependencies
}
}
\description{
Get step dependencies
}
\section{Methods}{

\itemize{
\item \code{pipe_get_depends}: get all dependencies for all steps defined
in the pipeline
\item \code{pipe_get_depends_down}: get all downstream dependencies of a
given step, by default descending recursively.
\item \code{pipe_get_depends_up}: get all upstream dependencies of a
given step, by default descending recursively.
}
}

\examples{
# pipe_get_depends
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
pipe_get_depends(p)

# pipe_get_depends_down
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
pipe_add(p, "mult3", \(x = ~add1) x * 3)
pipe_add(p, "mult4", \(x = ~add2) x * 4)
pipe_get_depends_down(p, "add1")
pipe_get_depends_down(p, "add1", recursive = FALSE)

# pipe_get_depends_up
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "add1", \(x = ~data) x + 1)
pipe_add(p, "add2", \(x = ~data, y = ~add1) x + y)
pipe_add(p, "mult3", \(x = ~add1) x * 3)
pipe_add(p, "mult4", \(x = ~add2) x * 4)
pipe_get_depends_up(p, "mult4")
pipe_get_depends_up(p, "mult4", recursive = FALSE)
}
