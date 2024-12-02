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

## ----pipeline-with-output-------------------------------------------------------------------------
library(pipeflow)
library(ggplot2)

pip <- pipe_new(
        "my-pipeline",
        data = airquality
    ) |>

    pipe_add(
        "data_prep",
        function(data = ~data) {
            replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5/9)
        }
    ) |>

    pipe_add(
        "data_summary",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            data[, c(xVar, yVar)]
        },
        keepOut = TRUE              # <- keep this
    ) |>

    pipe_add(
        "model_fit",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>

    pipe_add(
        "model_summary",
        function(
            fit = ~model_fit
        ) {
            summary(fit)
        },
        keepOut = TRUE              # <- keep this
    ) |>

    pipe_add(
        "model_plot",
        function(
            model = ~model_fit,
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone",
            title = "Linear model fit"
        ) {
            coeffs <- coefficients(model)
            ggplot(data) +
                geom_point(aes(.data[[xVar]], .data[["Ozone"]])) +
                geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
                labs(title = title)
        },
        keepOut = TRUE              # <- keep this
    )

## -------------------------------------------------------------------------------------------------
pip

## ----echo = FALSE, eval = getOption("pipeflow.visNetwork", default = FALSE)-----------------------
# library(visNetwork)
# do.call(visNetwork, args = c(pip$get_graph(), list(height = 300))) |>
#     visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip$run()

out <- pip$collect_out()

names(out)

## -------------------------------------------------------------------------------------------------
str(out, max.level = 1)

## ----pipeline-with-grouped-output-----------------------------------------------------------------
pip <- Pipeline$new("my-pipeline", data = airquality) |>

    pipe_add(
        "data_prep",
        function(data = ~data) {
            replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5/9)
        }
    ) |>

    pipe_add(
        "used_data",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            data[, c(xVar, yVar)]
        },
        keepOut = TRUE,
        group = "Data"                 # <- define 'Data' group here
    ) |>

    pipe_add(
        "model_fit",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>

    pipe_add(
        "model_summary",
        function(
            fit = ~model_fit
        ) {
            summary(fit)
        },
        keepOut = TRUE,
        group = "Model"                # <- define 'Model' group here
    ) |>

    pipe_add(
        "model_plot",
        function(
            model = ~model_fit,
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone",
            title = "Linear model fit"
        ) {
            coeffs <- coefficients(model)
            ggplot(data) +
                geom_point(aes(.data[[xVar]], .data[["Ozone"]])) +
                geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
                labs(title = title)
        },
        keepOut = TRUE,
        group = "Model"                # <- define 'Model' group here
    )

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip$run()

out <- pip$collect_out()

names(out)

## -------------------------------------------------------------------------------------------------
str(out, max.level = 2)

## ----include = FALSE----------------------------------------------------------
options(old)

