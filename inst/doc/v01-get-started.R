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

pip <- pip_add(
    pip,
    step = "data",
    fun = function(data = airquality) data
)

## ----show-initial-pipeline------------------------------------------------------------------------
pip

## ----define-data-prep-step------------------------------------------------------------------------
pip |> pip_add(
    "prep",
    function(df = ~data) {
        df[, "Temp.Celsius"] <- (df[, "Temp"] - 32) * 5 / 9
        df
    }
)

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip |> pip_add(
    "fit",
    function(data = ~prep,
             xVar = "Temp.Celsius") {
        lm(paste("Ozone ~", xVar), data = data)
    }
)

pip

## -------------------------------------------------------------------------------------------------
pip |> pip_add(
    "plot",
    function(model = ~fit,
             data = ~prep,
             xVar = "Temp.Celsius",
             xLab = "Temperature in degrees Celsius",
             title = "Linear model fit") {
        require(ggplot2, quietly = TRUE)
        coeffs <- coefficients(model)
        ggplot(data) +
            geom_point(aes(.data[[xVar]], .data[["Ozone"]])) +
            geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
            labs(title = title, x = xLab)
    }
)

## -------------------------------------------------------------------------------------------------
pip

## ----eval = FALSE---------------------------------------------------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = pip_graph(pip)) |>
#     visHierarchicalLayout(direction = "LR")

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_graph(pip), list(height = 100, width = 600))
) |>
    visHierarchicalLayout(direction = "LR")

## ----try-add-bad-step, error = TRUE---------------------------------------------------------------
try({
pip |> pip_add(
    "another_step",
    function(x = ~upsi) {
        x
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
    args = c(pip_graph(pip), list(height = 100, width = 600))
) |>
    visHierarchicalLayout(direction = "LR")

## ----inspect-lm, message = FALSE------------------------------------------------------------------
pip[["fit", "out"]]

## ----inspect-plot, message = FALSE, warning = FALSE, fig.alt = "model-plot"-----------------------
pip[["plot", "out"]]

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
    args = c(pip_graph(pip), list(height = 100, width = 600))
) |>
    visHierarchicalLayout(direction = "LR")

## ----run-pipeline-again---------------------------------------------------------------------------
pip_run(pip)

## ----inspect-plot-again, message = FALSE, warning = FALSE, fig.alt = "model-plot"-----------------
pip[["plot", "out"]]

## ----set-title------------------------------------------------------------------------------------
pip |> pip_set_params(list(title = "Some new title"))
pip

## ----inspect-plot-after-title-change, message = FALSE, warning = FALSE, fig.alt = "model-plot"----
pip_run(pip)
pip[["plot", "out"]]

## -------------------------------------------------------------------------------------------------
small_airquality <- airquality[1:10, ]
pip |> pip_set_params(list(data = small_airquality))
pip

## ----inspect-plot-after-data-change, message = FALSE, warning = FALSE, fig.alt = "model-plot"-----
pip_run(pip)
pip[["plot", "out"]]

## ----set-identical-values-------------------------------------------------------------------------
pip |> pip_set_params(list(title = "Some new title", data = small_airquality))
pip

## ----set-identical-values-force-------------------------------------------------------------------
pip |> pip_set_params(list(title = "Some new title"), force = TRUE)
pip

## ----set-unknown-parameters, warning = TRUE-------------------------------------------------------
pip |> pip_set_params(list(title = "warn me", foo = "my foo"))

pip_get_params(pip)$title

## ----ignore-unknown-parameters--------------------------------------------------------------------
pip |> pip_set_params(list(foo = "ignore me"), unknown = "ignore")

pip_get_params(pip)$title

## ----error-on-unknown-parameters, error = TRUE----------------------------------------------------
try({
pip |> pip_set_params(
    list(title = "Not set", foo = "my foo"),
    unknown = "error"
)
pip_get_params(pip)$title
})

## ----include = FALSE----------------------------------------------------------
options(old)

