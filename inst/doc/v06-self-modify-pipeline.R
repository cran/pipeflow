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

## -------------------------------------------------------------------------------------------------
library(pipeflow)
library(ggplot2)

pip <- pipe_new("my-pipeline") |>

    pipe_add(
        "fit",
        function(data = ~data, xVar = "x", yVar = "y")
        {
            lm(paste(yVar, "~", xVar), data = data)
        }
    ) |>

    pipe_add(
        "residual_shapiro_p_value",
        function(fit = ~fit)
        {
            residuals <- residuals(fit)
            p <- shapiro.test(residuals)$p.value
            p
        },
        keepOut = TRUE
    ) |>

    pipe_add(
        "plot",
        function(fit = ~fit, pointColor = "black")
        {
            data <- data.frame(
                fitted = predict(fit),
                residuals = residuals(fit)
            )

            ggplot(data, aes(x = fitted, y = residuals)) +
                geom_point(shape = 21, color = pointColor) +
                geom_hline(yintercept = 0, linetype = "dashed") +
                theme_minimal()
        },
        keepOut = TRUE
    )

## -------------------------------------------------------------------------------------------------
pip

## ----fig.alt = "residual-plot"--------------------------------------------------------------------
pip$set_data(airquality)
pip$set_params(list(xVar = "Ozone", yVar = "Temp"))
pip$run()$collect_out()

## ----fig.alt = "residual-plot"--------------------------------------------------------------------
if (pip$get_out("residual_shapiro_p_value") < 0.05) {
    pip$set_params(list(pointColor = "red"))
    pip$run()$collect_out()
}

## -------------------------------------------------------------------------------------------------
pip$replace_step(
    "residual_shapiro_p_value",
    function(
        fit = ~fit,
        .self = NULL
    ) {
        residuals <- residuals(fit)
        p <- shapiro.test(residuals)$p.value

        if (!is.null(.self) && p < 0.05) {
            .self$set_params(list(pointColor = "red"))
        }

        p
    },
    keepOut = TRUE
)

## ----fig.alt = "residual-plot"--------------------------------------------------------------------
pip$set_data(airquality)
pip$set_params(list(xVar = "Ozone", yVar = "Temp", .self = pip))
pip$run()$collect_out()

## -------------------------------------------------------------------------------------------------
pip <- pipe_new("my-pipeline") |>

    pipe_add(
        "f1",
        function(x = ~data) {
            x + 1
        }
    ) |>

    pipe_add(
        "f2",
        function(x = ~f1) {
            x + 2
        }
    ) |>

    pipe_add(
        "f3",
        function(x = ~f2) {
            x + 3
        }
    )

## -------------------------------------------------------------------------------------------------
pip$set_data(1)$run()
pip

## -------------------------------------------------------------------------------------------------
pip <- pipe_new("my-pipeline") |>

    pipe_add(
        "f1",
        function(x = ~data) {
            x + 1
        }
    ) |>

    pipe_add(
        "f2",
        function(x = ~f1, .self = NULL)
        {
            if (x > 10 && !is.null(.self))
            {
                .self$replace_step(
                    "f3",
                    function(x = ~f1) {
                        x - 3
                    }
                )
            }
            x + 2
        }
    ) |>

    pipe_add(
        "f3",
        function(x = ~f2) {
            x + 3
        }
    )

## -------------------------------------------------------------------------------------------------
pip$set_params(list(.self = pip))
pip$set_data(1)$run()
pip

## -------------------------------------------------------------------------------------------------
pip$set_data(10)$run()
pip

## -------------------------------------------------------------------------------------------------
pip <- pipe_new(
        "my-pipeline"
    ) |>

    pipe_add(
        "f1",
        function(x = ~data) {
            x + 1
        }
    ) |>

    pipe_add(
        "f2",
        function(x = ~f1, .self = NULL)
        {
            if (x > 10 && !is.null(.self)) {
                .self$insert_after(
                    afterStep = "f1",
                    step = "f2a",
                    function(x = ~f1) {
                        x + 21
                    }
                )
                .self$insert_after(
                    afterStep = "f2a",
                    step = "f2b",
                    function(x = ~f2a) {
                        x + 22
                    }
                )
                .self$replace_step(
                    "f3",
                    function(x = ~f2b) {
                        x + 30
                    }
                )
                .self$remove_step("f2")
                return(.self)
            }
            x + 2
        }
    ) |>

    pipe_add(
        "f3",
        function(x = ~f2) {
            x + 3
        }
    )


## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip$set_params(list(.self = pip))
pip$set_data(10)$run()

## -------------------------------------------------------------------------------------------------
pip

## ----include = FALSE----------------------------------------------------------
options(old)

