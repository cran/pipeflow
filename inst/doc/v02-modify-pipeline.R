## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----define-pipeline, include = FALSE, echo = FALSE-----------------------------------------------
library(pipeflow)
pip <- pip_new("my-pip") |>
    pip_add(
        "data",
        function(data = airquality[1:10, ]) data
    ) |>
    pip_add(
        "prep",
        function(df = ~data) {
            df[, "Temp.Celsius"] <- (df[, "Temp"] - 32) * 5 / 9
            df
        }
    ) |>
    pip_add(
        "fit",
        function(data = ~prep,
                 xVar = "Temp.Celsius") {
            lm(paste("Ozone ~", xVar), data = data)
        }
    ) |>
    pip_add(
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

pip |> pip_set_params(
    list(
        xVar = "Solar.R",
        xLab = "Solar radiation in Langleys",
        title = "Some new title"
    )
)

pip_run(pip, lgr = NULL)

## ----show-pipeline--------------------------------------------------------------------------------
pip

## ----show-data------------------------------------------------------------------------------------
pip_get_params(pip)[["data"]] |> head(3)

## ----insert-step----------------------------------------------------------------------------------
pip |> pip_add(
    "standardize",
    function(data = ~prep,
             yVar = "Ozone") {
        data[, yVar] <- scale(data[, yVar])
        data
    },
    after = "prep"
)

## -------------------------------------------------------------------------------------------------
pip

## ----eval = FALSE, echo = nzchar(Sys.getenv("IN_PKGDOWN"))----------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = pip_graph(pip)) |>
#     visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## ----echo = FALSE, eval = nzchar(Sys.getenv("IN_PKGDOWN"))----------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = c(pip_graph(pip), list(height = 300))) |>
#     visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip[["fit", "fun"]]

## ----replace-model-fit-step-----------------------------------------------------------------------
pip |> pip_replace(
    "fit",
    function(data = ~standardize, # <- changed data reference
        xVar = "Temp.Celsius",
        yVar = "Ozone" # <- new y-variable
    ) {
        lm(paste(yVar, "~", xVar), data = data)
    }
)

## ----replace-model-plot-step----------------------------------------------------------------------
pip |> pip_replace(
    "plot",
    function(model = ~fit,
             data = ~standardize, # <- changed data reference
             xVar = "Temp.Celsius",
             yVar = "Ozone", # <- new y-variable
             title = "Linear model fit") {
        coeffs <- coefficients(model)
        ggplot(data) +
            geom_point(aes(.data[[xVar]], .data[[yVar]])) +
            geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
            labs(title = title)
    }
)

## -------------------------------------------------------------------------------------------------
pip

## ----echo = FALSE, eval = nzchar(Sys.getenv("IN_PKGDOWN"))----------------------------------------
# do.call(visNetwork, args = c(pip_graph(pip), list(height = 100))) |>
#     visHierarchicalLayout(direction = "LR")

## -------------------------------------------------------------------------------------------------
pip_set_params(pip, params = list(xVar = "Solar.R", yVar = "Wind"))
pip_run(pip)

## -------------------------------------------------------------------------------------------------
pip[["fit", "out"]] |> coefficients()

## ----fig.alt = "model-plot", warning = FALSE, message = FALSE-------------------------------------
pip[["plot", "out"]]

## -------------------------------------------------------------------------------------------------
pip

## ----try-remove-step------------------------------------------------------------------------------
try(pip_remove(pip, "standardize"))

## ----remove-steps-recursively---------------------------------------------------------------------
pip_remove(pip, "standardize", force = TRUE)

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
last_step <- tail(pip[["step"]], 1)
pip_remove(pip, last_step)

## -------------------------------------------------------------------------------------------------
pip

## ----include = FALSE----------------------------------------------------------
options(old)

