#' Calculate the C-statistic for benefit
#'
#' @description Calculates the C-statistic for benefit for the evaluation of
#' discrimination
#'
#' @param data        A dataframe with columns ...
#' @param method      Can be "rank" ....
#' @export

calculateCForBenefit <- function(
  data,
  method = "rank"
) {

  pairs <- createPairs(
    data   = data,
    method = method
  )

  res <- AUC.trinary(
    xb.hat = pairs$pairPrediction,
    y      = pairs$pairOutcome
  )

  return(res$AUC)
}




#' Calculate the calibration for benefit
#'
#' @author
#'   Carolien Maas
#'
#' @description
#'   Calculates the ICI, E50 and E90 for benefit, using matched
#'   patient pairs
#'
#' @param data      A dataframe with columns `treatment`, `outcome` and
#'                  `predictedBenefit`
#' @param method    The method for matching. Currently, only `rank` is supported
#'
#' @export

calculateCalibrationForBenefit <- function(
  data,
  method = "rank"
) {

  pairs <- createPairs(
    data   = data,
    method = method
  )

  loessCalibrate <- stats::loess(
    pairOutcome ~ pairPrediction,
    data = pairs,
    statistics = "none"
  )

  tauSmoothed <- loessCalibrate$fitted

  res <- list(
    ici = mean(abs(tauSmoothed - pairs$pairPrediction)),
    e50 = stats::median(abs(tauSmoothed - pairs$pairPrediction)),
    e90 = as.numeric(stats::quantile(abs(tauSmoothed - pairs$pairPrediction), probs = .9))
  )

  return(res)

}
