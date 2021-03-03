# Author: David van Klaveren


AUC <- function(xb.hat,y){
  max.y <- max(y)
  n <- length(xb.hat)
  n1 <- sum(y == max.y)
  mean.rank <- mean(rank(xb.hat)[y == max.y])
  AUC <- (mean.rank - (n1 + 1) / 2) / (n - n1)
  comparable <- n1 * (n - n1) * 2
  concordant <- AUC * comparable
  return(
    list(
      AUC        = AUC,
      comparable = comparable,
      concordant = concordant
    )
  )
}

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
