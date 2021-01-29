#' Fit Loess HTE
#'
#' @description
#' Fit a loess type of model to a dataframe
#'
#' @param data            A dataframe containing at least a column named
#'                        "riskLinearPredictor" and column named "outcome".
#'                        Currently, only binary outcomes are supported.
#' @param settings   A list of settings generated from [createLoessSettings()]
#'
#' @export

fitLoessHte <- function(
  data,
  settings
) {
  if (is.null(data$propensityLinearPredictor)) {
    smoothFit <- loess(
      formula     = outcome ~ riskLinearPredictor,
      data        = data,
      weighths    = settings$weights,
      model       = settings$model,
      span        = settings$span,
      degree      = settings$degree,
      parametric  = settings$parametric,
      drop.square = settings$drop.square,
      normalize   = settings$normalize,
      family      = settings$family,
      method      = settings$method,
      control     = settings$control
    )
  }

  return(smoothFit)
}


#' Fit Restricted Cubic Spline HTE
#'
#' @description
#' Fit a restricted cubic spline smooth type of model to a dataframe
#'
#' @param data        A dataframe containing at least a column named
#'                    "riskLinearPredictor" and column named "outcome".
#'                    Currently, only binary outcomes are supported.
#' @param settings    A list of settings generated from [createRcsSettings()]
#'
#' @export

fitRcsHte <- function(
  data,
  settings
) {
  if (is.null(data$propensityLinearPredictor)) {
    # nKnots <- settings$nKnots
    smoothFit <- rms::lrm(
      formula           = outcome ~ rms::rcs(riskLinearPredictor, 5),
      data              = data,
      method            = settings$method,
      model             = settings$model,
      x                 = settings$x,
      y                 = settings$y,
      linear.predictors = settings$linear.predictors,
      se.fit            = settings$se.fit,
      penalty           = settings$penalty,
      tol               = settings$tol,
      strata.penalty    = settings$strata.penalty,
      var.penalty       = settings$var.penalty,
      scale             = settings$scale
    )
  }
  return(smoothFit)
}



#' Predict with smooth fit
#'
#' @description
#' Predict individualized benefit for new data based on a smooth fit.
#'
#' @param smoothFit       An estimated smooth fit
#' @param x               The new data on which the prediction is made
#'
#' @export

predictSmooth <- function(
  smoothFit,
  x
) {
  ret <- predict(
    smoothFit,
    newdata = data.frame(
      riskLinearPredictor = x
    )
  )

  return(ret)
}
