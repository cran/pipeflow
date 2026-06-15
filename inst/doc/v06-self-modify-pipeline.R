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
        "residual_shapiro_p_value",
        function(fit = ~fit) {
            residuals <- residuals(fit)
            shapiro.test(residuals)$p.value
        }
    ) |>
    pip_add(
        "plot",
        function(
            fit = ~fit,
            pointColor = "black"
        ) {
            require(ggplot2, quietly = TRUE)
            data <- data.frame(
                fitted = predict(fit),
                residuals = residuals(fit)
            )

            ggplot(data, aes(x = fitted, y = residuals)) +
                geom_point(shape = 21, color = pointColor) +
                geom_hline(yintercept = 0, linetype = "dashed") +
                theme_minimal()
        }
    )

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
class(pip)

## -------------------------------------------------------------------------------------------------
ls(pip)

## -------------------------------------------------------------------------------------------------
data.class(pip$pipeline)

pip$pipeline

## ----fig.alt = "residual-plot"--------------------------------------------------------------------
pip |> pip_set_params(list(data = airquality, xVar = "Ozone", yVar = "Temp"))

pip_run(pip)

pip[["plot", "out"]]

## ----fig.alt = "residual-plot"--------------------------------------------------------------------
if (pip[["residual_shapiro_p_value", "out"]] < 0.05) {
    pip |>
        pip_set_params(list(pointColor = "red")) |>
        pip_run()
}

pip[["plot", "out"]]

## -------------------------------------------------------------------------------------------------
pip |> pip_replace(
    "residual_shapiro_p_value",
    function(
        fit = ~fit,
        .self = NULL
    ) {
        residuals <- residuals(fit)
        p <- shapiro.test(residuals)$p.value

        if (p < 0.05) {
            .self |> pip_set_params(list(pointColor = "blue"))
        }

        p
    }
)

## ----fig.alt = "residual-plot2"-------------------------------------------------------------------
pip_run(pip)

pip[["plot", "out"]]

## -------------------------------------------------------------------------------------------------
pip <- pip_new("my-pipeline") |>
    pip_add("init", function(xInit = 0) xInit) |>
    pip_add("f1", function(x = ~init) x + 1) |>
    pip_add("f2", function(x = ~f1) x + 2) |>
    pip_add("f3", function(x = ~f2) x + 3)

## -------------------------------------------------------------------------------------------------
pip_run(pip)

pip

## -------------------------------------------------------------------------------------------------
pip |> pip_replace(
    "f2",
    function(
        x = ~f1,
        .self = NULL
    ) {
        if (x > 10) {
            .self |> pip_replace("f3", function(x = ~f1) x * 3)
            return(x / 2)
        }
        x + 2
    }
)

## -------------------------------------------------------------------------------------------------
pip |>
    pip_set_params(list(xInit = 15)) |>
    pip_run()

pip

## -------------------------------------------------------------------------------------------------
pip[["f3", "fun"]]

## -------------------------------------------------------------------------------------------------
pip <- pip_new("my-pipeline") |>
    pip_add("init", function(xInit = 0) xInit) |>
    pip_add("f1", function(x = ~init) x + 1) |>
    pip_add(
        "f2",
        function(
            x = ~f1,
            .self = NULL
        ) {
            if (x > 10) {
                .self |>
                    pip_add(
                        "f2a",
                        function(x = ~f1) x + 21,
                        after = "f1",
                    ) |>
                    pip_add(
                        "f2b",
                        function(x = ~f2a) x + 22,
                        after = "f2a"
                    ) |>
                    pip_replace(
                        "f3",
                        function(x = ~f2b) {
                            x + 30
                        }
                    ) |>
                    pip_remove("f2")
            }
            x + 2
        }
    ) |>
    pip_add("f3", function(x = ~f2) x + 3)

## -------------------------------------------------------------------------------------------------
pip_run(pip)

pip

## -------------------------------------------------------------------------------------------------
pip |>
    pip_set_params(list(xInit = 11)) |>
    pip_run()

pip

## -------------------------------------------------------------------------------------------------
pip |>
    pip_set_params(list(xInit = 11)) |>
    pip_run()

pip

## -------------------------------------------------------------------------------------------------
pip <- pip_new("my-pipeline") |>
    pip_add("init", function(xInit = 0) xInit) |>
    pip_add("f1", function(x = ~init) x + 1) |>
    pip_add(
        "f2",
        function(
            x = ~f1,
            .self = NULL
        ) {
            if (x > 10) {
                .self |>
                    pip_add(
                        "f2a",
                        function(x = ~f1) x + 21,
                        after = "f1",
                    ) |>
                    pip_add(
                        "f2b",
                        function(x = ~f2a) x + 22,
                        after = "f2a"
                    ) |>
                    pip_replace(
                        "f3",
                        function(x = ~f2b) {
                            x + 30
                        }
                    ) |>
                    pip_remove("f2")
                return(.self) # <-- return modified pipeline
            }
            x + 2
        }
    ) |>
    pip_add("f3", function(x = ~f2) x + 3)

## -------------------------------------------------------------------------------------------------
pip |>
    pip_set_params(list(xInit = 11)) |>
    pip_run(recursive = TRUE)

## -------------------------------------------------------------------------------------------------
pip

