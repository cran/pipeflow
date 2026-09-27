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
    pip_add("data", \(x = 1:5) x) |>
    pip_add("prep", \(x = ~data) x * 2, tags = "data") |>
    pip_add("data_summary", \(x = ~prep) range(x),
        tags = c("data", "summary")
    ) |>
    pip_add("model_fit", \(x = ~prep, k = 2) x * k, tags = c("model", "fit")) |>
    pip_add("model_summary", \(x = ~model_fit) sum(x),
        tags = c("model", "summary")
    )

## -------------------------------------------------------------------------------------------------
(pip_run(pip, lgr = NULL))

## -------------------------------------------------------------------------------------------------
pip_collect(pip)

## -------------------------------------------------------------------------------------------------
pip_view(pip, tags = "summary") |> # or pip[tags %like% "summary"]
    pip_collect()

pip_view(pip, tags = "model") |>
    pip_collect()

## -------------------------------------------------------------------------------------------------
pip[step %in% c("data", "prep")] |> pip_tag("Introduction")
pip[step == "model_fit"] |> pip_tag("Model")
pip[step %like% "summary"] |> pip_tag("Summary")

pip

## -------------------------------------------------------------------------------------------------
report <- list(
    Introduction = pip_view(pip, tags = "Introduction") |> pip_collect(),
    Model = pip_view(pip, tags = "Model") |> pip_collect(),
    Summary = pip_view(pip, tags = "Summary") |> pip_collect()
)

str(report)

## -------------------------------------------------------------------------------------------------
byTags <- pip_collect(pip, by = "tags")
report2 <- byTags[c("Introduction", "Model", "Summary")]

str(report2)

## -------------------------------------------------------------------------------------------------
pip_collect(pip, by = "tags", as.table = TRUE)

## -------------------------------------------------------------------------------------------------
pip_collect(pip, by = "depends", as.table = TRUE)

## ----include = FALSE----------------------------------------------------------
options(old)

