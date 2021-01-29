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



#' Create Restricted Cubic Spline Settings
#'
#' @description
#' Creates the settings for fitting a restricted cubic spline smooth estimate
#' of absolute benefit
#'
#' @param nKnots                Number of knots. Default is 5. The minimum value
#'                              is 3.
#' @param method                Name of fitting function. Only allowable choice
#'                              at present is lrm.fit.
#' @param model                 Causes the model frame to be returned in the fit
#'                              object
#' @param x                     Causes the expanded design matrix (with missings
#'                              excluded) to be returned under the name x. For
#'                              print, an object created by lrm.
#' @param y                     causes the response variable (with missings
#'                              excluded) to be returned under the name y.
#' @param linear.predictors     Causes the predicted X beta (with missings
#'                              excluded) to be returned under the name
#'                              linear.predictors. When the response variable
#'                              has more than two levels, the first intercept is
#'                              used.
#' @param se.fit                Causes the standard errors of the fitted values
#'                              to be returned under the name se.fit.
#' @param penalty               The penalty factor subtracted from the log
#'                              likelihood is 0.5 β' P β, where β is the vector
#'                              of regression coefficients other than intercept(s),
#'                              and P is penalty factors * penalty.matrix and
#'                              penalty.matrix is defined below. The default is
#'                              penalty=0 implying that ordinary unpenalized
#'                              maximum likelihood estimation is used. If penalty
#'                              is a scalar, it is assumed to be a penalty factor
#'                              that applies to all non-intercept parameters in
#'                              the model. Alternatively, specify a list to
#'                              penalize different types of model terms by
#'                              differing amounts. The elements in this list are
#'                              named simple, nonlinear, interaction and
#'                              nonlinear.interaction. If you omit elements on
#'                              the right of this series, values are inherited
#'                              from elements on the left. Examples:
#'                              penalty=list(simple=5, nonlinear=10) uses a
#'                              penalty factor of 10 for nonlinear or interaction
#'                              terms. penalty=list(simple=0, nonlinear=2,
#'                              nonlinear.interaction=4) does not penalize linear
#'                              main effects, uses a penalty factor of 2 for
#'                              nonlinear or interaction effects (that are not both),
#'                              and 4 for nonlinear interaction effects.
#' @param tol                   Singularity criterion (see lrm.fit)
#' @param strata.penalty        Scalar penalty factor for the stratification
#'                              factor, for the experimental strat variable
#' @param var.penalty           The type of variance-covariance matrix to be
#'                              stored in the var component of the fit when
#'                              penalization is used. The default is the inverse
#'                              of the penalized information matrix. Specify
#'                              var.penalty="sandwich" to use the sandwich
#'                              estimator (see below under var), which limited
#'                              simulation studies have shown yields variances
#'                              estimates that are too low.
#' @param scale                 Set to TRUE to subtract means and divide by
#'                              standard deviations of columns of the design
#'                              matrix before fitting, and to back-solve for the
#'                              un-normalized covariance matrix and regression
#'                              coefficients. This can sometimes make the model
#'                              converge for very large sample sizes where for
#'                              example spline or polynomial component variables
#'                              create scaling problems leading to loss of precision
#'                              when accumulating sums of squares and crossproducts.
#' @export

createRcsSettings <- function(
  nKnots            = 5,
  method            = "lrm.fit",
  model             = FALSE,
  x                 = FALSE,
  y                 = FALSE,
  linear.predictors = TRUE,
  se.fit            = FALSE,
  penalty           = 0,
  tol               = 1e-7,
  strata.penalty    = 0,
  var.penalty       = "simple",
  scale             = FALSE
) {
  analysis <- list()
  for (name in names(formals(createRcsSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}
