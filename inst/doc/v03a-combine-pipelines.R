## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----define-prepocessing-pipeline-----------------------------------------------------------------
library(pipeflow)

pip1 <- pip_new("preprocess") |>
    pip_add("data", \(x = 1:5) x) |>
    pip_add("prep", \(x = ~data) x + 1) |>
    pip_add("standardize", \(x = ~prep, scale = 2) x * scale)

## ----define-modelling-pipeline--------------------------------------------------------------------
pip2 <- pip_new("model") |>
    pip_add("data", \(x = 1:5) x) |>
    pip_add("fit", \(x = ~data, k = 2, b = 0) x * k + b) |>
    pip_add("predict", \(x = ~fit) paste0("pred: ", x))

## -------------------------------------------------------------------------------------------------
pip <- rbind(pip1, pip2)

pip

## ----eval = FALSE---------------------------------------------------------------------------------
# library(visNetwork)
# do.call(visNetwork, args = pip_graph(pip)) |>
#     visHierarchicalLayout(direction = "LR")

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_graph(pip), list(height = 250, width = 500))
) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip |> pip_replace("data2", \(x = ~standardize) x)

pip

## ----echo = FALSE---------------------------------------------------------------------------------
do.call(
    visNetwork,
    args = c(pip_graph(pip), list(height = 100, width = 700))
) |>
    visHierarchicalLayout(direction = "LR")

## -------------------------------------------------------------------------------------------------
pip |> pip_replace("data2", \(x = ~ -1) x)

pip

## -------------------------------------------------------------------------------------------------
pip_run(pip)

## -------------------------------------------------------------------------------------------------
pip[["predict", "out"]]

## -------------------------------------------------------------------------------------------------
pip_set_params(pip, params = list(k = 3))

## ----echo = FALSE---------------------------------------------------------------------------------
library(visNetwork)
do.call(
    visNetwork,
    args = c(pip_graph(pip), list(height = 100, width = 700))
) |>
    visHierarchicalLayout(direction = "LR", sortMethod = "directed")

## -------------------------------------------------------------------------------------------------
pip_run(pip)

## -------------------------------------------------------------------------------------------------
pip[["predict", "out"]]

## ----include = FALSE----------------------------------------------------------
options(old)

