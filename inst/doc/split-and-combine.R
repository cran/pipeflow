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

pip <- pipe_new(
        "my-pipeline"
    ) |>

    pipe_add(
        "fit",
        function(
            data = ~data,
            xVar = "x",
            yVar = "y"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>

    pipe_add(
        "coefs",
        function(
            fit = ~fit
        ) {
            coefficients(fit)
        },
        keepOut = TRUE
    )

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR")

## -------------------------------------------------------------------------------------------------
head(iris)

## -------------------------------------------------------------------------------------------------
pip$set_data(iris)
pip$set_params(list(xVar = "Sepal.Length", yVar = "Sepal.Width"))
pip$run()

## -------------------------------------------------------------------------------------------------
pip$collect_out()

## -------------------------------------------------------------------------------------------------
run_pipeline <- function(data) {
    pip$set_data(data)
    pip$run()
    pip$collect_out()
}

results <- lapply(split(iris, iris$Species), FUN = run_pipeline)

## -------------------------------------------------------------------------------------------------
results

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
splitData <- split(iris, iris$Species)
pip$set_data_split(splitData)

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
do.call(visNetwork, args = pip$get_graph()) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$run()

## -------------------------------------------------------------------------------------------------
pip$collect_out()

## -------------------------------------------------------------------------------------------------
pip$split()

## -------------------------------------------------------------------------------------------------
pip <- pipe_new(
        "my-pipeline"
    ) |>

    pipe_add(
        "fit",
        function(
            data = ~data,
            xVar = "x",
            yVar = "y"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>

    pipe_add(
        "coefs",
        function(
            fit = ~fit
        ) {
            coefficients(fit)
        }
    )  |>

    pipe_add(
        "combine_coefs",
        function(
            coefs = ~coefs
        ) {
            coefs |> do.call(rbind, args = _) |> as.data.frame()
        },
        keepOut = TRUE
    )

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip$set_data_split(split(iris, iris$Species), toStep = "coefs")
pip

## -------------------------------------------------------------------------------------------------
pip$get_depends()[["combine_coefs"]]

## -------------------------------------------------------------------------------------------------
do.call(visNetwork, args = pip$get_graph()) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$set_params(list(xVar = "Sepal.Length", yVar = "Sepal.Width"))
pip$run()

## -------------------------------------------------------------------------------------------------
pip$collect_out()

## ----include = FALSE----------------------------------------------------------
options(old)

