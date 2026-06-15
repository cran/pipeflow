## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----define-prepocessing-pipeline-----------------------------------------------------------------
library(pipeflow)

pip1 <- pip_new("preprocessing") |>
    pip_add(
        "data",
        function(data = airquality) data
    ) |>
    pip_add(
        "data_prep",
        function(data = ~data) {
            replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5 / 9)
        }
    ) |>
    pip_add(
        "standardize",
        function(
            data = ~data_prep,
            yVar = "Ozone"
        ) {
            data[, yVar] <- scale(data[, yVar])
            data
        }
    )

## -------------------------------------------------------------------------------------------------
pip1

## ----define-modeling-pipeline---------------------------------------------------------------------
pip2 <- pip_new("modeling") |>
    pip_add(
        "data",
        function(data = airquality) data
    ) |>
    pip_add(
        "fit",
        function(
            data = ~data,
            xVar = "Temp",
            yVar = "Ozone"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>
    pip_add(
        "plot",
        function(
            model = ~fit,
            data = ~data,
            xVar = "Temp",
            yVar = "Ozone",
            title = "Linear model fit"
        ) {
            require(ggplot2, quietly = TRUE)
            coeffs <- coefficients(model)
            ggplot(data) +
                geom_point(aes(.data[[xVar]], .data[[yVar]])) +
                geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
                labs(title = title)
        }
    )

## -------------------------------------------------------------------------------------------------
pip2

## -------------------------------------------------------------------------------------------------
pip <- pip_bind(pip1, pip2)

pip

## ----eval = FALSE---------------------------------------------------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = pip_get_graph(pip)) |>
#     visHierarchicalLayout(direction = "LR")

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 250, width = 500))
) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip |> pip_replace("data2", function(data = ~standardize) data)

pip

## ----echo = FALSE---------------------------------------------------------------------------------
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 100, width = 700))
) |>
    visHierarchicalLayout(direction = "LR")

## -------------------------------------------------------------------------------------------------
pip |> pip_replace("data2", function(data = ~ -1) data)

pip

## -------------------------------------------------------------------------------------------------
pip_run(pip)

## ----fig.alt = "model-plot"-----------------------------------------------------------------------
pip[["plot", "out"]]

## -------------------------------------------------------------------------------------------------
pip_set_params(pip, params = list(xVar = "Temp.Celsius"))

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 100, width = 700))
) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip_run(pip)

## ----fig.alt = "model-plot"-----------------------------------------------------------------------
pip[["plot", "out"]]

## ----cherry-pick----------------------------------------------------------------------------------
pip <- pip_new("cherry-picked-from-1-and-2") |>
    pip_add_from(pip1, "data") |>
    pip_add_from(pip1, "data_prep") |>
    pip_add_from(pip1, "standardize") |>
    pip_add_from(pip2, "fit") |>
    pip_add_from(pip2, "plot")

pip

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 250, width = 700))
) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## ----include = FALSE----------------------------------------------------------
options(old)

