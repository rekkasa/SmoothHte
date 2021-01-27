#' Fit Loess HTE
#'
#' @description
#' Fit a loess type of model to a dataframe
#'
#' @param data            A dataframe containing at least a column named
#'                        "riskLinearPredictor" and column named "outcome".
#'                        Currently, only binary outcomes are supported.
#' @param loessSettings   A list of settings generated from [createLoessSettings()]
#'
#' @export

fitLoessHte <- function(
  data,
  loessSettings
) {
  if (is.null(data$propensityLinearPredictor)) {
    smoothFit <- loess(
      formula     = outcome ~ riskLinearPredictor,
      data        = data,
      weighths    = loessSettings$weights,
      model       = loessSettings$model,
      span        = loessSettings$span,
      degree      = loessSettings$degree,
      parametric  = loessSettings$parametric,
      drop.square = loessSettings$drop.square,
      normalize   = loessSettings$normalize,
      family      = loessSettings$family,
      method      = loessSettings$method,
      control     = loessSettings$control
    )

    f <- function(x, smoothFit) {
      dat <- data.frame(
        riskLinearPredictor = x
      )
      predict(
        smoothFit,
        newdata = dat
      )
    }
  }

  return(
    list(
      predict = f,
      smooth = smoothFit
    )
  )
}
