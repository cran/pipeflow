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
library(ggplot2)

## ----define-prepocessing-pipeline-----------------------------------------------------------------
library(pipeflow)
library(ggplot2)

pip1 <- pipe_new(
        "preprocessing",
        data = airquality
    ) |>

    pipe_add(
        "data_prep",
        function(data = ~data) {
            replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5/9)
        }
    ) |>

    pipe_add(
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
pip2 <- pipe_new(
        "modeling",
        data = airquality
    ) |>

    pipe_add(
        "fit",
        function(
            data = ~data,
            xVar = "Temp",
            yVar = "Ozone"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>

    pipe_add(
        "plot",
        function(
            model = ~fit,
            data = ~data,
            xVar = "Temp",
            yVar = "Ozone",
            title = "Linear model fit"
        ) {
            coeffs <- coefficients(model)
            ggplot(data) +
                geom_point(aes(.data[[xVar]], .data[[yVar]])) +
                geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
                labs(title = title)
        }
    )

## -------------------------------------------------------------------------------------------------
pip2

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip1$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip2$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip <- pip1$append(pip2)

pip

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 250))) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$replace_step("data.modeling", function(data = ~standardize) data)

pip

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$replace_step("data.modeling", function(data = ~-1) data)

pip

## -------------------------------------------------------------------------------------------------
pip <- pip1$append(pip2, outAsIn = TRUE)

pip

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$get_step("data.modeling")[["fun"]]

## -------------------------------------------------------------------------------------------------
pip$run()

## -------------------------------------------------------------------------------------------------
pip$get_out("plot")

## -------------------------------------------------------------------------------------------------
pip$set_params(list(xVar = "Temp.Celsius"))

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$run()

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$get_out("plot")

## ----include = FALSE----------------------------------------------------------
options(old)

