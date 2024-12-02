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

## ----show-airquality------------------------------------------------------------------------------
head(airquality)

## ----define-pipeline------------------------------------------------------------------------------
library(pipeflow)
library(ggplot2)

pip <- Pipeline$new("my-pipeline", data = airquality)

## ----show-initial-pipeline------------------------------------------------------------------------
pip

## ----define-data-prep-step------------------------------------------------------------------------
pip$add(
    "data_prep",
    function(data = ~data) {
        replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5/9)
    }
)

## ----show-pipeline-after-data-prep-was-added------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip$add(
    "model_fit",
    function(
        data = ~data_prep,
        xVar = "Temp.Celsius"
    ) {
        lm(paste("Ozone ~", xVar), data = data)
    }
)

## -------------------------------------------------------------------------------------------------
pip$add(
    "model_plot",
    function(
        model = ~model_fit,
        data = ~data_prep,
        xVar = "Temp.Celsius",
        title = "Linear model fit"
    ) {
        coeffs <- coefficients(model)
        ggplot(data) +
            geom_point(aes(.data[[xVar]], .data[["Ozone"]])) +
            geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
            labs(title = title)
    }
)

## ----show-completed-pipeline----------------------------------------------------------------------
pip

## ----eval = FALSE---------------------------------------------------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = pip$get_graph()) |>
#     visHierarchicalLayout(direction = "LR")

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR")

## ----run-pipeline---------------------------------------------------------------------------------
pip$run()

## ----pipeline-after-run---------------------------------------------------------------------------
pip

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR")

## ----inspect-lm, message = FALSE------------------------------------------------------------------
pip$get_out("model_fit")

## ----inspect-plot, message = FALSE, warning = FALSE-----------------------------------------------
pip$get_out("model_plot")

## ----inspect-params-------------------------------------------------------------------------------
pip$get_params()

## ----set-xVar-------------------------------------------------------------------------------------
pip$set_params(list(xVar = "Solar.R"))
pip$get_params()

## ----show-pipeline-with-outdated-step-------------------------------------------------------------
pip

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
    visHierarchicalLayout(direction = "LR")

## ----run-pipeline-again---------------------------------------------------------------------------
pip$run()

## ----inspect-plot-again, message = FALSE, warning = FALSE-----------------------------------------
pip$get_out("model_plot")

## ----set-title------------------------------------------------------------------------------------
pip$set_params(list(title = "Some new title"))
pip

## ----inspect-plot-after-title-change, message = FALSE, warning = FALSE----------------------------
pip$run()$get_out("model_plot")

## -------------------------------------------------------------------------------------------------
pip$set_data(airquality[1:10, ])
pip

## ----inspect-plot-after-data-change, message = FALSE, warning = FALSE-----------------------------
pip$run()$get_out("model_plot")

## ----include = FALSE----------------------------------------------------------
options(old)

