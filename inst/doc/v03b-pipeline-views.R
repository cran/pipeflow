## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----setup-pipeline-------------------------------------------------------------------------------
library(pipeflow)

pip <- pip_new("my-pip") |>
    pip_add("load", \(n = 5) seq_len(n), tags = c("io", "daily")) |>
    pip_add("clean", \(x = ~load) x * 2, tags = c("io", "core")) |>
    pip_add("fit", \(x = ~clean) sum(x), tags = c("model", "core")) |>
    pip_add("report", \(x = ~fit) paste("result:", x), tags = "report")

## ----first-run------------------------------------------------------------------------------------
(pip_run(pip, lgr = NULL))

## ----create-view----------------------------------------------------------------------------------
pip_view(pip, tags = "core")

## ----combine-filters------------------------------------------------------------------------------
pip_view(pip, tags = "core", state = "done")

pip_view(pip, step = c("clean", "fit"))

## ----union-filters--------------------------------------------------------------------------------
pip_view(pip, tags = "report", step = "clean", join = "union")

## ----regex-filters--------------------------------------------------------------------------------
pip_view(pip, step = "^f", fixed = FALSE)

## ----filter-depends-------------------------------------------------------------------------------
pip_reset(pip) # reset to initial state

pip_view(pip, depends = "load", state = "new")

## ----select-steps---------------------------------------------------------------------------------
pip_run(pip, lgr = NULL)
pip[c("load", "fit")]

pip[1:3]

## ----boolean-filters------------------------------------------------------------------------------
pip[tags %like% "core"]

pip[step %in% c("clean", "fit") & state == "done"]

## ----negative-indices-----------------------------------------------------------------------------
pip[-2]

## ----copy-and-columns-----------------------------------------------------------------------------
pip2 <- pip[]

pip[, c("step", "tags")]

pip[c("load", "fit"), "out"]

## ----extract-pipeline-----------------------------------------------------------------------------
pip[c("fit", "report"), view = FALSE]

## ----compose-views--------------------------------------------------------------------------------
v1 <- pip_view(pip, tags = "io") # load, clean
v1

v2 <- v1 |> pip_view(tags = "core") # clean only
v2

## ----view-meta-field------------------------------------------------------------------------------
pip[["view"]] # NULL — not a view

w <- pip
w[["view"]] <- c("load", "clean")
w

w[["view"]] <- NULL # back to the full pipeline
w

## ----view-after-changes---------------------------------------------------------------------------
q <- pip_clone(pip)
v <- pip_view(q, tags = "core")
v

# A new "core" step is not added to the existing view
pip_add(q, "check", \(x = ~clean) all(x > 0), tags = "core", after = "clean")

# A renamed step stays in the view under its new name
pip_rename(q, from = "fit", to = "model")
v

# Removed steps drop out of the view
pip_remove(q, "model", force = TRUE)
v

## ----run-view-------------------------------------------------------------------------------------
pip_reset(pip)
pip_run(pip_view(pip, step = "report"))

## ----restore-options, include = FALSE-----------------------------------------
options(old)

