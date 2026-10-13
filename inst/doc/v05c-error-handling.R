## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----define-pipeline------------------------------------------------------------------------------
library(pipeflow)

pip <- pip_new("analysis") |>
    pip_add("data", \(n = 5) seq_len(n)) |>
    pip_add("check", \(x = ~data, limit = 3) {
        if (max(x) > limit) stop("data exceeds limit")
        x
    }) |>
    pip_add("model", \(x = ~check) mean(x)) |>
    pip_add("total", \(x = ~data) sum(x))

## ----run-and-stop, error = TRUE-------------------------------------------------------------------
try({
pip_run(pip)

pip

pip_run_state(pip)
})

## ----step-error-fields----------------------------------------------------------------------------
err <- tryCatch(pip_run(pip, lgr = NULL), error = \(e) e)

class(err)
err$step

class(err$parent)
err$parent$message

## ----run-and-continue, warning = TRUE-------------------------------------------------------------
pip$reset()

suppressWarnings(pip_run(pip, on_error = "continue"))

pip

pip_run_state(pip)

## ----failed-output--------------------------------------------------------------------------------
out <- pip[["check", "out"]]

class(out)
out$message

## ----run-failed-warning---------------------------------------------------------------------------
w <- tryCatch(
    pip_run(pip, lgr = NULL, on_error = "continue"),
    warning = \(w) w
)

names(w$failed)
w$skipped

## ----add-report-step------------------------------------------------------------------------------
pip_add(
    pip,
    "report",
    \(total = ~total, m = ~try(model)) {
        if (inherits(m, "pipeflow_failure")) {
            paste("total:", total, "- no model:", m$message)
        } else {
            paste("total:", total, "- mean:", m)
        }
    }
)

suppressWarnings(pip_run(pip, on_error = "continue"))

pip[["report", "out"]]

## ----recover--------------------------------------------------------------------------------------
pip_set_params(pip, list(limit = 10))
pip_run(pip)

pip

## ----recovered-report-----------------------------------------------------------------------------
pip[["report", "out"]]

pip_run_state(pip)

## ----define-split-pipeline------------------------------------------------------------------------
d <- data.frame(g = c("a", "a", "b", "b", "c"), y = c(1, 2, 3, 5, 8))

pip <- pip_new("groups") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add("by_group", \(data = ~data) split(data, data$g), exec = "split") |>
    pip_add("spread", \(d = ~by_group) {
        if (nrow(d) < 2) stop("need at least 2 rows")
        diff(range(d$y))
    }) |>
    pip_add(
        "combine",
        \(s = ~try(spread)) {
            if (inherits(s, "pipeflow_failure")) {
                return(s$message)
            }
            unlist(s)
        },
        exec = "reduce"
    )

pip_set_params(pip, list(data = d))

## ----run-split-pipeline---------------------------------------------------------------------------
suppressWarnings(pip_run(pip, on_error = "continue"))

pip

err <- pip[["spread", "out"]]
class(err)
err$key
err$parent$message

## ----reduce-with-failure--------------------------------------------------------------------------
pip[["combine", "out"]]

## ----recover-split-pipeline-----------------------------------------------------------------------
pip_set_params(pip, list(data = d[d$g != "c", ]))
pip_run(pip)

pip[["combine", "out"]]

## ----include = FALSE----------------------------------------------------------
options(old)

