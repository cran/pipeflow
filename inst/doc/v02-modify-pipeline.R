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

## ----define-pipeline, include = FALSE, echo = FALSE-----------------------------------------------
pip <- Pipeline$new("my-pipeline", data = airquality)
pip$add(
    "data_prep",
    function(data = ~data) {
        replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5/9)
    }
)
pip$add(
    "model_fit",
    function(
        data = ~data_prep,
        xVar = "Temp.Celsius"
    ) {
        lm(paste("Ozone ~", xVar), data = data)
    }
)
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
pip$set_params(list(xVar = "Solar.R"))
pip$set_params(list(title = "Some new title"))
pip$set_data(airquality[1:10, ])
pip$run()

## ----show-pipeline--------------------------------------------------------------------------------
pip

## ----show-data------------------------------------------------------------------------------------
pip$get_data() |> head(3)

## ----insert-step----------------------------------------------------------------------------------
pip$insert_after(
    afterStep = "data_prep",
    step = "standardize",
    function(
        data = ~data_prep,
        yVar = "Ozone"
    ) {
        data[, yVar] <- scale(data[, yVar])
        data
    }
)

## -------------------------------------------------------------------------------------------------
pip

## ----eval = FALSE, echo = as.logical(Sys.getenv("pipeflow.visNetwork", unset = "FALSE"))----------
# library(visNetwork)
# do.call(visNetwork, args = pip$get_graph()) |>
#     visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## ----echo = FALSE, eval = as.logical(Sys.getenv("pipeflow.visNetwork", unset = "FALSE"))----------
# library(visNetwork)
# do.call(visNetwork, args = c(pip$get_graph(), list(height = 300))) |>
#     visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$get_step("model_fit")[["fun"]]

## ----replace-model-fit-step-----------------------------------------------------------------------
pip$replace_step(
    "model_fit",
    function(
        data = ~standardize,        # <- changed data reference
        xVar = "Temp.Celsius",
        yVar = "Ozone"              # <- new y-variable
    ) {
        lm(paste(yVar, "~", xVar), data = data)
    }
)

## ----replace-model-plot-step----------------------------------------------------------------------
pip$replace_step(
    "model_plot",
    function(
        model = ~model_fit,
        data = ~standardize,         # <- changed data reference
        xVar = "Temp.Celsius",
        yVar = "Ozone",              # <- new y-variable
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
pip

## ----echo = FALSE, eval = getOption("pipeflow.visNetwork", default = FALSE)-----------------------
# library(visNetwork)
# do.call(visNetwork, args = c(pip$get_graph(), list(height = 100))) |>
#     visHierarchicalLayout(direction = "LR")

## -------------------------------------------------------------------------------------------------
pip$set_params(list(xVar = "Solar.R", yVar = "Wind"))
pip$run()

## -------------------------------------------------------------------------------------------------
pip$get_out("model_fit") |> coefficients()

## ----fig.alt = "model-plot"-----------------------------------------------------------------------
pip$get_out("model_plot")

## -------------------------------------------------------------------------------------------------
pip

## ----try-remove-step------------------------------------------------------------------------------
try(pip$remove_step("standardize"))

## ----remove-steps-recursively---------------------------------------------------------------------
pip$remove_step("standardize", recursive = TRUE)

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip$pop_step()

## -------------------------------------------------------------------------------------------------
pip

## ----include = FALSE----------------------------------------------------------
options(old)

