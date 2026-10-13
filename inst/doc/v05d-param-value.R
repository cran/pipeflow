## ----knitr-setup, include = FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(
    comment = "#",
    prompt = FALSE,
    tidy = FALSE,
    cache = FALSE,
    collapse = TRUE
)

old <- options(width = 100L)

## ----quantity-class-------------------------------------------------------------------------------
library(pipeflow)

quantity <- function(value, unit) {
    structure(list(value = value, unit = unit), class = "quantity")
}

print.quantity <- function(x, ...) {
    cat("<quantity>", x$value, x$unit, "\n")
    invisible(x)
}

quantity(120, "km")

## ----speed-pipeline-------------------------------------------------------------------------------
p <- pip_new("speed") |>
    pip_add("speed", \(distance = 1, time = 1) distance / time)

pip_set_params(
    p,
    list(distance = quantity(120, "km"), time = quantity(2, "h"))
)

## ----speed-without-method-------------------------------------------------------------------------
tryCatch(
    pip_run(p, lgr = NULL),
    error = \(e) conditionMessage(e)
)

## ----quantity-method------------------------------------------------------------------------------
pip_param_value.quantity <- function(x, ...) x$value

## ----register-method------------------------------------------------------------------------------
registerS3method("pip_param_value", "quantity", pip_param_value.quantity)

## ----speed-with-method----------------------------------------------------------------------------
pip_run(p, lgr = NULL)
p[["speed", "out"]]

## ----stored-params--------------------------------------------------------------------------------
pip_get_params(p)

## ----defaults-and-dots----------------------------------------------------------------------------
p2 <- pip_new("defaults") |>
    pip_add(
        "describe",
        \(x = quantity(3, "m"), ...) {
            c(x = class(x), dots = sapply(list(...), class))
        },
        params = list(extra = quantity(1, "s"))
    )

pip_run(p2, lgr = NULL)
p2[["describe", "out"]]

## ----step-outputs---------------------------------------------------------------------------------
p3 <- pip_new("outputs") |>
    pip_add("make", \(v = 3) quantity(v, "m")) |>
    pip_add("inspect", \(q = ~make) class(q))

pip_run(p3, lgr = NULL)
p3[["inspect", "out"]]

## ----checked-class--------------------------------------------------------------------------------
checked <- function(value, unit) {
    structure(list(value = value, unit = unit), class = "checked")
}

pip_param_value.checked <- function(x, ...) {
    if (!x$unit %in% c("m", "km")) {
        stop("unknown unit '", x$unit, "'")
    }
    x$value
}
registerS3method("pip_param_value", "checked", pip_param_value.checked)

p4 <- pip_new("errors") |>
    pip_add("double", \(x = 1) x * 2) |>
    pip_add("increment", \(y = ~double) y + 1)

pip_set_params(p4, list(x = checked(1, "ft")))

## ----method-error-stop----------------------------------------------------------------------------
tryCatch(
    pip_run(p4, lgr = NULL),
    error = \(e) conditionMessage(e)
)

## ----method-error-continue------------------------------------------------------------------------
suppressWarnings(
    pip_run(p4, lgr = NULL, on_error = "continue")
)

pip_data(p4)[, c("step", "state")]

## ----method-error-condition-----------------------------------------------------------------------
conditionMessage(p4[["double", "out"]])

## ----include = FALSE----------------------------------------------------------
options(old)

