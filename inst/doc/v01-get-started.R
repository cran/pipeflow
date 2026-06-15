## ----knitr-setup, include = FALSE-----------------------------------------------------------------
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

pip <- pip_new("my-pip")

pip <- pip_add(pip,
    step = "data",
    fun = function(data = airquality) data
)

## ----show-initial-pipeline------------------------------------------------------------------------
pip

## ----define-data-prep-step------------------------------------------------------------------------
pip |> pip_add(
    "data_prep",
    function(x = ~data) {
        replace(x, "Temp.Celsius", (x[, "Temp"] - 32) * 5 / 9)
    }
)

## -------------------------------------------------------------------------------------------------
pip |> pip_add(
    "model_fit",
    function(
        data = ~data_prep,
        xVar = "Temp.Celsius"
    ) {
        lm(paste("Ozone ~", xVar), data = data)
    }
)
pip

## -------------------------------------------------------------------------------------------------
pip |> pip_add(
    "model_plot",
    function(
        model = ~model_fit,
        data = ~data_prep,
        xVar = "Temp.Celsius",
        xLab = "Temperature in degrees Celsius",
        title = "Linear model fit"
    ) {
        require(ggplot2, quietly = TRUE)
        coeffs <- coefficients(model)
        ggplot(data) +
            geom_point(aes(.data[[xVar]], .data[["Ozone"]])) +
            geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
            labs(title = title, x = xLab)
    }
)
pip

## ----eval = FALSE---------------------------------------------------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = pip_get_graph(pip)) |>
#     visHierarchicalLayout(direction = "LR")

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 100, width = 600))
) |>
    visHierarchicalLayout(direction = "LR")

## ----try-add-bad-step, error = TRUE---------------------------------------------------------------
try({
pip |> pip_add(
    "another_step",
    function(data = ~foo) {
        data
    }
)
})

## -------------------------------------------------------------------------------------------------
pip

## ----run-pipeline---------------------------------------------------------------------------------
pip_run(pip)

## ----pipeline-after-run---------------------------------------------------------------------------
pip

## ----echo = FALSE---------------------------------------------------------------------------------
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 100, width = 600))
) |>
    visHierarchicalLayout(direction = "LR")

## ----inspect-lm, message = FALSE------------------------------------------------------------------
pip[["model_fit", "out"]]

## ----inspect-plot, message = FALSE, warning = FALSE, fig.alt = "model-plot"-----------------------
pip[["model_plot", "out"]]

## ----inspect-params-------------------------------------------------------------------------------
pip_get_params(pip) |> str()

## ----set-xVar-------------------------------------------------------------------------------------
pip |>
    pip_set_params(list(xVar = "Solar.R", xLab = "Solar radiation in Langleys"))

pip_get_params(pip) |> str()

## ----show-pipeline-with-outdated-step-------------------------------------------------------------
pip

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_get_graph(pip), list(height = 100, width = 600))
) |>
    visHierarchicalLayout(direction = "LR")

## ----run-pipeline-again---------------------------------------------------------------------------
pip_run(pip)

## ----inspect-plot-again, message = FALSE, warning = FALSE, fig.alt = "model-plot"-----------------
pip[["model_plot", "out"]]

## ----set-title------------------------------------------------------------------------------------
pip |> pip_set_params(list(title = "Some new title"))
pip

## ----inspect-plot-after-title-change, message = FALSE, warning = FALSE, fig.alt = "model-plot"----
pip_run(pip)
pip[["model_plot", "out"]]

## -------------------------------------------------------------------------------------------------
small_airquality <- airquality[1:10, ]
pip |> pip_set_params(list(data = small_airquality))
pip

## ----inspect-plot-after-data-change, message = FALSE, warning = FALSE, fig.alt = "model-plot"-----
pip_run(pip)
pip[["model_plot", "out"]]

## ----set-unknown-parameters, warning = TRUE-------------------------------------------------------
pip |> pip_set_params(list(titel = "misspelled variable name", foo = "my foo"))

## ----include = FALSE----------------------------------------------------------
options(old)

