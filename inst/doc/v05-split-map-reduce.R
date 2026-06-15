## ----knitr-setup, include = FALSE-----------------------------------------------------------------
require(pipeflow)

knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## -------------------------------------------------------------------------------------------------
library(pipeflow)

pip <- pip_new("my-pipeline") |>
    pip_add(
        "data",
        function(data = NULL) data
    ) |>
    pip_add(
        "fit",
        function(
            data = ~data,
            xVar = "x",
            yVar = "y"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add(
        "coefs",
        function(fit = ~fit) {
            coefficients(fit)
        }
    )

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
head(iris)

## -------------------------------------------------------------------------------------------------
pip |> pip_set_params(list(
    data = iris,
    xVar = "Sepal.Length",
    yVar = "Sepal.Width"
))

pip_run(pip)

## -------------------------------------------------------------------------------------------------
pip[["coefs", "out"]]

## -------------------------------------------------------------------------------------------------
run_pipeline_helper <- function(data) {
    pip |> pip_set_params(list(data = data))
    pip_run(pip)
    pip[["coefs", "out"]]
}

results <- lapply(split(iris, iris$Species), FUN = run_pipeline_helper)

## -------------------------------------------------------------------------------------------------
results

## -------------------------------------------------------------------------------------------------
pip <- pip_new("my-split-pip") |>
    pip_add(
        "data",
        function(data = NULL) data
    ) |>
    pip_add(
        "split_data",
        function(
            data = ~data,
            byVar = "by"
        ) {
            split(data, f = data[[byVar]])
        },
        exec = "split" # <-- set execution mode to "split"
    ) |>
    pip_add(
        "fit",
        function(
            data = ~split_data,
            xVar = "x",
            yVar = "y"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add(
        "coefs",
        function(fit = ~fit) {
            coefficients(fit)
        }
    )

## -------------------------------------------------------------------------------------------------
pip

## ----eval = FALSE---------------------------------------------------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = pip_get_graph(pip))

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 100, width = 500))
) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip |> pip_set_params(list(
    data = iris,
    xVar = "Sepal.Length",
    yVar = "Sepal.Width",
    byVar = "Species"
))

pip_run(pip)

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip[["coefs", "out"]]

## -------------------------------------------------------------------------------------------------
pip |> pip_add(
    "combine_coefs",
    function(coefs = ~coefs) {
        do.call(rbind, coefs)
    },
    exec = "reduce" # <-- set execution mode to "reduce"
)

## -------------------------------------------------------------------------------------------------
pip

## ----eval = FALSE---------------------------------------------------------------------------------
# do.call(visNetwork, args = pip_get_graph(pip))

## ----echo = FALSE---------------------------------------------------------------------------------
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 100, width = 650))
) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip_run(pip)

pip[["combine_coefs", "out"]]

## ----include = FALSE----------------------------------------------------------
options(old)

