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
  smoothFit <- stats::loess(
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



fitLocfitHte <- function(
  data,
  settings
) {



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
  if (settings$nKnots == 3) {
    smoothFit <- rms::lrm(
      formula = outcome ~ treatment +
        rms::rcs(riskLinearPredictor, 3) +
        treatment * rms::rcs(riskLinearPredictor, 3),
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
  } else if (settings$nKnots == 4) {
    smoothFit <- rms::lrm(
      formula = outcome ~ treatment +
        rms::rcs(riskLinearPredictor, 4) +
        treatment * rms::rcs(riskLinearPredictor, 4),
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
  } else if (settings$nKnots == 5) {
    smoothFit <- rms::lrm(
      formula = outcome ~ treatment +
        rms::rcs(riskLinearPredictor, 5) +
        treatment * rms::rcs(riskLinearPredictor, 5),
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

  aic <- 2*length(smoothFit$coefficients) + smoothFit$deviance
  smoothFit$aic <- aic[2]

  attr(smoothFit, "smoothClass") <- "rcs"
  attr(smoothFit, "type") <- paste("RCS with", settings$nKnots, "knots")

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
    formula = outcome ~ locfit::lp(riskLinearPredictor, by = treatment),
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



#' Fit risk-stratified heterogeneity of treatment effect
#' @description
#' Fits a risk-stratified approach for assessing treatment effect heterogeneity
#' @param data        A dataframe containing at least a column named
#'                    "riskLinearPredictor" and column named "outcome".
#'                    Currently, only binary outcomes are supported.
#' @param settings    A list of settings generated from [createStratifiedSettings()]
#' @export
#' @importFrom dplyr %>%
fitStratifiedHte <- function(
  data,
  settings
) {
  ret <- data %>%
  dplyr::mutate(riskStratum = dplyr::ntile(riskLinearPredictor, 4))

  rseeData <- ret %>%
    dplyr::group_by(riskStratum, treatment) %>%
    dplyr::summarise(
      x = sum(outcome),
      n = dplyr::n(),
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
      meanRisk = mean(stats::plogis(riskLinearPredictor))
    )

  ret <- rseeData %>%
    dplyr::left_join(meanRisk)

  quants <- stats::quantile(
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



#' Predict smooth benefit
#' @description
#' Predicts the absolute benefit based on two smooth fits
#' @param p                 The new data on which the prediction is made
#' @param smoothControl     The smooth fit in the control arm
#' @param smoothTreatment   The smooth fit in the treatment arm
#' @export
predictSmoothBenefit <- function(
  p,
  smoothFit
) {

  x <- log(p / (1 - p))

  pred0 <- predict(
    smoothFit,
    newdata = data.frame(
      treatment           = 0,
      riskLinearPredictor = x
    )
  )

  pred1 <- predict(
    smoothFit,
    newdata = data.frame(
      treatment           = 1,
      riskLinearPredictor = x
    )
  )

  smoothClass <- attr(smoothFit, "smoothClass")
  if (smoothClass == "loess" | smoothClass == "locfit") {
    ret <- pred0 - pred1
  } else {
    ret <- plogis(pred0) - plogis(pred1)
  }

  # ret <- plogis(pred0) - plogis(pred1)


  return(ret)
}


#' Predict risk-stratified benefits
#' @description
#' Predicts absolute benefit using the risk-stratified approach to treatment
#' effect heterogeneity
#' @param p                  The new data on which the prediction is made
#' @param stratifiedHte      The estimated risk-stratified fit. See [fitStratifiedHte()]
#' @export
predictStratifiedBenefit <- function(
  p,
  stratifiedHte
) {

  customRank <- function(x, quantiles) {
    v <- c(x, quantiles)
    return(rank(v)[1] - 1)
  }

  quantiles <- stats::plogis(stratifiedHte$quantiles)
  k = sapply(p, customRank, quantiles = quantiles)
  res <- unlist(stratifiedHte$data$estimate[k])

  return(res)
}


#' Fit model-based heterogeneity of treatment effect
#' @description
#' Fits a model-based approach to treatment effect heterogeneity
#' @param data        A dataframe containing at least a column named
#'                    "riskLinearPredictor" and column named "outcome".
#'                    Currently, only binary outcomes are supported.
#' @param settings    A list of settings generated from [createModelBasedSettings()]
#' @export
fitModelBasedHte <- function(
  data,
  settings
) {

  settings$args$data <- data

  if (settings$type == "treatment") {
    modelFormula <- "offset(riskLinearPredictor) + treatment - 1"
    # if (!is.null(settings$adjustmentCovariates)) {
    #   covariates <- c(settings$adjustmentCovariates, "treatment")
    #   modelFormula <- paste(covariates, collapse = "+")
    # }
  } else if (settings$type == "risk") {
    modelFormula <- paste(
      c(
        "treatment",
        "riskLinearPredictor",
        "riskLinearPredictor*treatment"
      ),
      collapse = "+"
    )
  }

  if (settings$model == "logistic") {
    settings$fun <- "glm"
    settings$args$family = "binomial"
  }


  settings$args$formula <- paste("outcome", modelFormula, sep = "~")
  fit <- do.call(
    eval(parse(text = settings$fun)),
    settings$args
  )

  attr(fit, "smoothClass") <- "modelBased"
  attr(fit, "type") <- settings$type
  return(fit)

}



#' Predict model-based treatment effects
#' @description
#' Predicts absolute benefit using the model-based approach to treatment effect
#' heterogeneity
#' @param p                   The new data on which the prediction is made
#' @param modelBasedFit       The estimated model-based fit. See [fitModelBasedHte()]
#' @export
predictBenefitModelBasedHte <- function(
  p,
  modelBasedFit
) {

  type <- attr(modelBasedFit, "type")

  lp <- log(p / (1 - p))   # Risk linear predictor
  if (type == "treatment") {
    treatmentEffect <- stats::coefficients(modelBasedFit)["treatment"]
    benefit <- stats::plogis(lp) - stats::plogis(lp + treatmentEffect)
  } else {
    p0 <- stats::plogis(
      stats::predict(
        modelBasedFit,
        newdata = data.frame(treatment = 0, riskLinearPredictor = lp)
      )
    )
    p1 <- stats::plogis(
      stats::predict(
        modelBasedFit,
        newdata = data.frame(treatment = 1, riskLinearPredictor = lp)
      )
    )
    benefit <- p0 - p1
  }

  return(benefit)
}



#' Fit adaptive approach
#'
#' @description
#' Fits an adaptive approach for assessing treatment effect heterogeneity
#'
#' @param data        The data on which the adaptive approach will be applied.
#'                    Requires a column called `riskLinearPredictor` with the
#'                    linear predictors of the existing prediction model
#' @param settings    A list of settings for making smooth benefit predictions
#'
#' @export
fitAdaptive <- function(
  data,
  settings
) {

  selectedAic   <- Inf
  selectedModel <- NULL

  for (i in seq_along(settings)) {

    tmpType <- class(settings[[i]])[2]

    if (tmpType == "modelBased") {
      tmpModel <- fitModelBasedHte(
        data     = data,
        settings = settings[[i]]
      )
    } else if (tmpType == "rcs") {
      tmpModel <- fitRcsHte(
        data     = data,
        settings = settings[[i]]
      )
    }

    if (selectedAic > tmpModel$aic) {
      selectedModel <- tmpModel
      selectedAic   <- tmpModel$aic
    }
  }

  return(selectedModel)
}


#' Predict LOESS benefit
#'
#' @description
#' Predicts absolute benefit based on two loess fits, one in each treatment arm
#'
#' @param p   The baseline risk probability
#' @param s0  The fit in the untreated arm
#' @param s1  The fit in the treated arm
#'
#' @export
predictBenefitLoess <- function(
  p,
  s0,
  s1
) {

  x <- log(p / (1 - p))

  pred0 <- predict(
      s0,
      newdata = data.frame(riskLinearPredictor = x)
    )

  pred1 <- predict(
      s1,
      newdata = data.frame(riskLinearPredictor = x)
    )

  return(pred0 - pred1)
}
