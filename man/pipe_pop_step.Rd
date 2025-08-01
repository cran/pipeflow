% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aliases.R
\name{pipe_pop_step}
\alias{pipe_pop_step}
\alias{pipe_pop_steps_after}
\alias{pipe_pop_steps_from}
\title{Pop steps from the pipeline}
\usage{
pipe_pop_step(pip)

pipe_pop_steps_after(pip, step)

pipe_pop_steps_from(pip, step)
}
\arguments{
\item{pip}{\code{Pipeline} object}

\item{step}{\code{string} name of step}
}
\value{
\code{string} the name of the step that was removed
}
\description{
Use this function to drop steps from the end of the pipeline.
}
\section{Methods}{

\itemize{
\item \code{pipe_pop_step}: drop last step from the pipeline
\item \code{pipe_pop_steps_after}: drop all steps after given steps
\item \code{pipe_pop_steps_from}: drop all steps from and including given steps
}
}

\examples{
# pipe_pop_step
p <- pipe_new("pipe", data = 1:2)
pipe_add(p, "f1", \(x = 1) x)
pipe_add(p, "f2", \(y = 1) y)
p
pipe_pop_step(p)
p

# pipe_pop_steps_after
pipe_add(p, "f2", \(y = 1) y)
pipe_add(p, "f3", \(z = 1) z)
p
pipe_pop_steps_after(p, "f1")
p

# pipe_pop_steps_from
pipe_add(p, "f2", \(y = 1) y)
pipe_add(p, "f3", \(z = 1) z)
p
pipe_pop_steps_from(p, "f1")
p
}
