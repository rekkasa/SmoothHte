#' Create Loess Settings
#'
#' @description
#' Creates the settings for fitting a loess fit.
#'
#' @param span         The parameter alpha which controls the degree of smoothing.
#' @param degree       the degree of the polynomials to be used, normally 1 or 2.
#'                     (Degree 0 is also allowed, but see the ‘Note’.)
#'
#' @export

createLoessSettings <- function(
  span   = .5,
  degree = 1
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
  nKnots            = 3,
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




#' Create Locfit settings
#'
#' @description
#' Create the settings for fitting a local likelihood approach for smooth
#' risk-based prediction of absolute benefit.
#'
#' @param kern          Weight function, default = "tcub". Other choices are
#'                      "rect", "trwt", "tria", "epan", "bisq" and "gauss".
#'                      Choices may be restricted when derivatives are required;
#'                      e.g. for confidence bands and some bandwidth selectors.
#' @param kt            Kernel type, "sph" (default); "prod". In multivariate
#'                      problems, "prod" uses a simplified product model which
#'                      speeds up computations.
#' @param family        Local likelihood family; "gaussian"; "binomial";
#'                      "poisson"; "gamma" and "geom". Density and rate
#'                      estimation families are "dens", "rate" and "hazard"
#'                      (hazard rate). If the family is preceded by a 'q' (for
#'                      example, family="qbinomial"), quasi-likelihood variance
#'                      estimates are used. Otherwise, the residual variance (rv)
#'                      is fixed at 1. The default family is "qgauss" if a
#'                      response y is provided; "density" if no response is
#'                      provided.
#' @param link          Link function for local likelihood fitting. Depending on
#'                      the family, choices may be "ident", "log", "logit",
#'                      "inverse", "sqrt" and "arcsin".
#' @param maxk          Controls space assignment for evaluation structures. For
#'                      the adaptive evaluation structures, it is impossible to
#'                      be sure in advance how many vertices will be generated.
#'                      If you get warnings about ‘Insufficient vertex space’,
#'                      Locfit's default assigment can be increased by increasing
#'                      maxk. The default is maxk=100.
#' @param mint          Points for numerical integration rules. Default 20.
#' @param maxit         Maximum iterations for local likelihood estimation.
#'                      Default 20.
#' @param debug         If > 0; prints out some debugging information
#'
#' @export

createLocfitSettings <- function(
  kern    = "tricube",
  kt      = "sph",
  family  = "binomial",
  link    = "default",
  maxk    = 100,
  mint    = 20,
  maxit   = 20,
  debug   = 0
) {
  analysis <- list()
  for (name in names(formals(createLocfitSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}


#' Create stratified analysis settings
#' @description
#' Creates the settings for fitting a risk-stratified analysis
#' @param nStrata    The number of strata in which the population will be
#'                   divided. Currently, only equal-sized risk strata are
#'                   supported.
#' @export
createStratifiedSettings <- function(
  nStrata = 4
) {
  analysis <- list()
  for (name in names(formals(createStratifiedSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)
}


#' Create model based settings
#' @description
#' Create the settings for a model-based approach to HTE. Either a
#' covariate-adjusted constant treatment effect model or a model with the baseline
#' risk linear predictor can be considered.
#'
#' @param adjustmentCovariates    The covariates to be used for adjustment, if a
#'                                constant relative treatment effect is assumed
#' @param model                   The model-type assumed. Currently, only
#'                                "logistic" is available
#' @param type                    Can be either "risk" or "treatment". If "risk"
#'                                a column called "riskLinearPredictor" is
#'                                required in the data. If "treatment" a column
#'                                "treatment" is required in the data.
#'
#' @export

createModelBasedSettings <- function(
  adjustmentCovariates = NULL,
  model = "logistic",
  type = "risk"
) {

  analysis <- list()
  for (name in names(formals(createModelBasedSettings))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- "args"

  return(analysis)

}
