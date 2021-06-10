#' Calculate AUC
#'
#' @author
#'   David van Klaveren
#' @description
#'   Calculates AUC for binary outcomes
#'
#' @param xb.hat     A vector with the predictions
#' @param y          A vector with the observed outcomes

AUC <- function(xb.hat,y){
  max.y <- max(y)
  n <- as.numeric(length(xb.hat))
  n1 <- as.numeric(sum(y == max.y))
  mean.rank <- mean(rank(xb.hat)[y == max.y])
  AUC <- (mean.rank - (n1 + 1) / 2) / (n - n1)
  comparable <- as.numeric(n1 * (n - n1) * 2)
  concordant <- AUC * comparable
  return(
    list(
      AUC        = AUC,
      comparable = comparable,
      concordant = concordant
    )
  )
}


#' Calculate AUC
#'
#' @author
#'   David van Klaveren
#' @description
#'   Calculates AUC for trinary outcomes like the ones generated from matching
#'   patients
#'
#' @param xb.hat     A vector with the predictions
#' @param y          A vector with the observed outcomes

AUC.trinary <- function(xb.hat,y){
  AUC1 <- AUC(xb.hat,y)   # all pairs with max outcome
  sel <- y!=max(y)   # all pairs without max outcome
  AUC2 <- AUC(xb.hat[sel], y[sel])
  comparable <- AUC1$comparable + AUC2$comparable
  concordant <- AUC1$concordant + AUC2$concordant
  AUC <- concordant / comparable
  return(
    list(
      AUC        = AUC,
      comparable = comparable,
      concordant = concordant
    )
  )
}

#' Create matched pairs
#'
#' @author Carolien Maas
#'
#' @description
#'   Creates matched pairs for the calculation of discrimination and
#'   calibration for benefit
#'
#' @param data      A dataframe with columns `treatment`, `outcome` and
#'                  `predictedBenefit`
#' @export
createPairs <- function(data){

  tau.hat <- data$predictedBenefit
  Y.obs <- data$outcome
  W <- data$treatment

  ind.A <- which(W==0)              # A is control treatment
  order.A <- order(tau.hat[ind.A])  # order predicted treatment benefit from low to high
  ind.A <- ind.A[order.A]           # list of indices from ordered predicted treatment benefit

  ind.B <- which(W==1)              # B is active treatment
  order.B <- order(tau.hat[ind.B])
  ind.B <- ind.B[order.B]

  ##### make vectors of equal length by randomly deleting some observations
  n.A <- length(ind.A)
  n.B <- length(ind.B)
  n.pairs <- min(n.A, n.B)
  if (n.A != n.B & n.pairs == n.A){
    # delete some observations randomly from B
    deleted.samples <- sample(order.B, n.B-n.pairs)
    ind.B <- ind.B[-deleted.samples]
  }
  else if (n.A != n.B & n.pairs == n.B){
    # delete some observations randomly from A
    deleted.samples <- sample(order.A, n.A-n.pairs)
    ind.A <- ind.A[-deleted.samples]
  }

  # Calculation of predicted treatment benefit in matched pairs
  tau.hat.A <- tau.hat[ind.A]
  tau.hat.B <- tau.hat[ind.B]
  tau.hat.avg <- (tau.hat.A+tau.hat.B)/2

  # Calculation of observed treatment benefit in matched pairs
  Y.obs.A <- Y.obs[ind.A]
  Y.obs.B <- Y.obs[ind.B]
  tau.obs <- Y.obs.A-Y.obs.B

  return(
    tibble(
      pairOutcome = tau.obs,
      pairPrediction = tau.hat.avg
    )
  )
}
