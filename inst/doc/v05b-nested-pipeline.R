## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----inner-pipeline-------------------------------------------------------------------------------
library(pipeflow)

inner <- pip_new("coefficients") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add(
        "fit",
        \(data = ~data, xVar = "x", yVar = "y") {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add("coefs", \(fit = ~fit) coefficients(fit))

inner

## ----outer-pipeline-------------------------------------------------------------------------------
# Helper to run inner pipeline
run_inner_pip <- function(pip, name, data) {
    pip$name <- sprintf("coefs for *%s*", name)

    # Set data subset for inner pipeline and run it
    pip_set_params(pip, list(data = data)) |> pip_run()

    pip[["coefs", "out"]]
}

outer <- pip_new("full analysis") |>
    pip_add("data", \(data = NULL) data) |>
    pip_add(
        "split_data", \(data = ~data, byVar = "by") {
            split(data, f = data[[byVar]])
        }
    ) |>
    pip_add(
        "inner_run",
        \(dataList = ~split_data, xVar = "x", yVar = "y") {
            p <- pip_clone(inner)

            # Forward parameters to inner
            pip_set_params(p, list(xVar = xVar, yVar = yVar))

            Map(
                f = run_inner_pip,
                name = names(dataList),
                data = dataList,
                MoreArgs = list(pip = p)
            )
        }
    ) |>
    pip_add(
        "combine",
        \(coefs = ~inner_run) as.data.frame(do.call(rbind, coefs))
    )

outer

## -------------------------------------------------------------------------------------------------
outer |>
    pip_set_params(
        list(
            data = iris,
            xVar = "Sepal.Length",
            yVar = "Sepal.Width",
            byVar = "Species"
        )
    ) |>
    pip_run()

## -------------------------------------------------------------------------------------------------
outer[["inner_run", "out"]]

## -------------------------------------------------------------------------------------------------
outer[["combine", "out"]]

## -------------------------------------------------------------------------------------------------
pip_set_params(outer, params = list(xVar = "Petal.Length"))

outer

## ----replace-inner-run----------------------------------------------------------------------------
outer |> pip_replace(
    "inner_run",
    \(dataList = ~split_data, ...) {
        p <- pip_clone(inner)

        # Forward all parameters (from outer and inner)
        all_params <- .self$get_params()
        pip_set_params(p, all_params, unknown = "ignore")

        Map(
            f = run_inner_pip,
            name = names(dataList),
            data = dataList,
            MoreArgs = list(pip = p)
        )
    },
    params = pip_get_params(inner) # <--- default parameters of inner pipeline
)

outer

## ----re-run-full-pipe-----------------------------------------------------------------------------
outer |>
    pip_set_params(
        list(
            data = iris,
            xVar = "Sepal.Length",
            yVar = "Sepal.Width",
            byVar = "Species"
        )
    ) |>
    pip_run()

## -------------------------------------------------------------------------------------------------
pip_set_params(outer, params = list(xVar = "Petal.Length"))

outer

## ----include = FALSE----------------------------------------------------------
options(old)

