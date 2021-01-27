#' Create Loess Settings
#'
#' @description
#' Creates the settings for fitting a loess fit.
#'
#' @param weights      Optional weights for each case.
#' @param model        Should the model frame be returned?
#' @param span         The parameter alpha which controls the degree of smoothing.
#' @param degree       the degree of the polynomials to be used, normally 1 or 2.
#'                     (Degree 0 is also allowed, but see the ‘Note’.)
#' @param parametric   Should any terms be fitted globally rather than locally?
#                      Terms can be specified by name, number or as a logical
#'                     vector of the same length as the number of predictors.
#' @param drop.square  For fits with more than one predictor and degree = 2,
#'                     should the quadratic term be dropped for particular
#'                     predictors? Terms are specified in the same way as for
#'                     parametric.
#' @param normalize    Should the predictors be normalized to a common scale if
#'                     there is more than one? The normalization used is to set
#'                     the 10% trimmed standard deviation to one. Set to false
#'                     for spatial coordinate predictors and others known to be
#'                     on a common scale.
#' @param family       If "gaussian" fitting is by least-squares, and if
#'                     "symmetric" a re-descending M estimator is used with
#'                     Tukey's biweight function. Can be abbreviated.
#' @param method       Fit the model or just extract the model frame. Can be
#'                     abbreviated.
#' @param control      Control parameters.
#'
#' @export

createLoessSettings <- function(
  weights     = numeric(),
  model       = FALSE,
  span        = .75,
  degree      = 2,
  parametric  = FALSE,
  drop.square = FALSE,
  normalize   = TRUE,
  family      = "gaussian",
  method      = "loess",
  control     = loess.control()
) {
  analysis <- list()
  for (name in names(formals(createLoessSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}
