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

  attr(smoothFit, "smoothClass") <- "loess"

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
  attr(smoothFit, "smoothClass") <- "rcs"

  return(smoothFit)
}






#' Fit Restricted Cubic Spline HTE
#'
#' @description
#' Fit a local likelihood model for risk-based smoothing of absolute benefit
#'
#' @param data        A dataframe containing at least a column named
#'                    "riskLinearPredictor" and column named "outcome".
#'                    Currently, only binary outcomes are supported.
#' @param settings    A list of settings generated from [createLocfitSettings()]
#'
#' @export

fitLocfitHte <- function(
  data,
  settings
) {
  smoothFit <- locfit::locfit(
    formula = outcome ~ locfit::lp(riskLinearPredictor),
    data    = data,
    kern    = settings$kern,
    kt      = settings$kt,
    family  = settings$family,
    link    = settings$link,
    maxk    = settings$maxk,
    mint    = settings$mint,
    maxit   = settings$maxit,
    debug   = settings$debug
  )
  attr(smoothFit, "smoothClass") <- "locfit"

  return(smoothFit)
}



#' @export
#' @importFrom dplyr %>%
fitStrataHte <- function(
  data,
  nStrata
) {
  ret <- data %>%
  dplyr::mutate(riskStratum = dplyr::ntile(riskLinearPredictor, 4))

  rseeData <- ret %>%
    dplyr::group_by(riskStratum, treatment) %>%
    dplyr::summarise(
      x = sum(outcome),
      n = n(),
      m = mean(outcome)
    ) %>%
    dplyr::group_by(riskStratum) %>%
    dplyr::summarise(
      estimate = m[1] - m[2],
      lower = unlist(
        PropCIs::diffscoreci(
          unlist(x[1]),
          unlist(n[1]),
          unlist(x[2]),
          unlist(n[2]),
          conf.level = .95
        )
      )[1],
      upper = unlist(
        PropCIs::diffscoreci(
          unlist(x[1]),
          unlist(n[1]),
          unlist(x[2]),
          unlist(n[2]),
          conf.level = .95
        )
      )[2]
    )

  meanRisk <- ret %>%
    dplyr::group_by(riskStratum) %>%
    dplyr::summarise(
      meanRisk = mean(plogis(riskLinearPredictor))
    )

  ret <- rseeData %>%
    dplyr::left_join(meanRisk)

  quants <- quantile(
    data$riskLinearPredictor,
    c(0, .25, .5, .75, 1)
  )
  quants[1] <- -Inf
  quants[length(quants)] <- Inf
  names(quants) <- NULL

  return(
    list(
      data = ret,
      quantiles = quants
    )
  )
}



#' Predict with smooth fit
#'
#' @description
#' Predict individualized benefit for new data based on a smooth fit.
#'
#' @param smoothFit       An estimated smooth fit
#' @param p               The new data on which the prediction is made
#'
#' @export

predictSmooth <- function(
  smoothFit,
  p
) {
  x <- log(p / (1 - p))

  ret <- predict(
    smoothFit,
    newdata = data.frame(
      riskLinearPredictor = x
    )
  )

  if (attr(smoothFit, "smoothClass") == "rcs") {
    ret <- exp(ret) / (1 + exp(ret))
  }

  return(ret)
}


#' @export
predictBenefit <- function(
  p,
  smoothControl,
  smoothTreatment
) {

  if (attr(smoothTreatment, "smoothClass") != attr(smoothControl, "smoothClass")) {
    stop("Smooth fits not of the same smoothClass")
  }

  predictSmooth(smoothControl, p) - predictSmooth(smoothTreatment, p)

}
