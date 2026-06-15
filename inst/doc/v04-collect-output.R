## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----pipeline-with-output-------------------------------------------------------------------------
library(pipeflow)

pip <- pip_new("my-pip") |>
    pip_add(
        "data",
        function(data = airquality) data
    ) |>
    pip_add("data_prep",
        function(data = ~data) {
            replace(data, "Temp.Celsius", (data[, "Temp"] - 32) * 5 / 9)
        },
        tags = "data"                   # <-- set 'data' tag
    ) |>
    pip_add(
        "data_summary",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            format(summary(data[, c(xVar, yVar)])) |>
                as.data.frame(row.names = NA)
        },
        tags = c("data", "summary")     # <-- set 'data' and 'summary' tags
    ) |>
    pip_add(
        "data_plot",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            require(ggplot2, quietly = TRUE)
            ggplot(data) +
                geom_point(aes(.data[[xVar]], .data[[yVar]])) +
                labs(title = "Data")
        },
        tags = c("data", "plot")        # <-- set 'data' and 'plot' tags
    ) |>
    pip_add(
        "model_fit",
        function(
            data = ~data_prep,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            lm(paste(yVar, "~", xVar), data = data)
        },
        tags = c("model", "fit")        # <-- set 'model' and 'fit' tags
    ) |>
    pip_add(
        "model_summary",
        function(fit = ~model_fit) {
            summary(fit) |>
                coefficients() |>
                as.data.frame()
        },
        tags = c("model", "summary")    # <-- set 'model' and 'summary' tags
    ) |>
    pip_add(
        "model_plot",
        function(
            model = ~model_fit,
            data_plot = ~data_plot,
            xVar = "Temp.Celsius",
            yVar = "Ozone"
        ) {
            coeffs <- coefficients(model)
            data_plot +
                geom_abline(intercept = coeffs[1], slope = coeffs[2]) +
                labs(title = "Linear model fit")
        },
        tags = c("model", "plot")       # <-- set 'model' and 'plot' tags
    )

## -------------------------------------------------------------------------------------------------
pip

## -------------------------------------------------------------------------------------------------
pip_run(pip)

pip

## ----message = FALSE, warning = FALSE-------------------------------------------------------------
pip[["data_plot", "out"]]

## ----message = FALSE, warning = FALSE-------------------------------------------------------------
pip[["model_plot", "out"]]

## -------------------------------------------------------------------------------------------------
out <- pip_collect_out(pip)

names(out)

## -------------------------------------------------------------------------------------------------
pip_view(pip, tags = "plot")

## ----grouped-plots, message = FALSE, warning = FALSE----------------------------------------------
pip |>
    pip_view(tags = "plot") |>
    pip_collect_out() |>
    gridExtra::grid.arrange(grobs = _, nrow = 2)

## -------------------------------------------------------------------------------------------------
grouped <- list(
    data  = pip_view(pip, tags = "data")  |> pip_collect_out(),
    model = pip_view(pip, tags = "model") |> pip_collect_out()
)

names(grouped[["data"]])
names(grouped[["model"]])

## -------------------------------------------------------------------------------------------------
pip_set_params(pip, params = list(xVar = "Solar.R", yVar = "Wind"))

pip

## -------------------------------------------------------------------------------------------------
pip |> pip_view(filter = list(depends = "model_fit", state = "outdated"))

## -------------------------------------------------------------------------------------------------
pip |>
    pip_view(filter = list(step = "^data", state = "outdated"), fixed = FALSE)

## -------------------------------------------------------------------------------------------------
v <- pip |> pip_view(filter = list(state = "outdated"))
v

v2 <- v |> pip_view(tags = "plot")
v2

## -------------------------------------------------------------------------------------------------
v2 |> pip_run()

## -------------------------------------------------------------------------------------------------
pip

## ----include = FALSE----------------------------------------------------------
options(old)

