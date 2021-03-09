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
#' @description
#'   Creates matched pairs for the calculation of discrimination and
#'   calibration for benefit
#'
#' @param data      A dataframe with columns `treatment`, `outcome` and
#'                  `predictedBenefit`
#' @param method    The method for matching. Currently, only `rank` is supported
#' @export

createPairs <- function(
  data,
  method = "rank"
) {
  if (method == "rank") {

    numberOfPairs <- min(sum(data$treatment), sum(!data$treatment))

    grouped <- data %>%
      dplyr::group_by(treatment) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        rankOutcomes = purrr::map(
          .x = data,
          .f = ~tibble(
            outcome          = .x$outcome,
            predictedBenefit = .x$predictedBenefit,
            rank             = rank(.x$predictedBenefit)
          ) %>%
            arrange(rank) %>%
            slice(1:numberOfPairs)
        )
      ) %>%
      tidyr::unnest(rankOutcomes) %>%
      dplyr::select(-data) %>%
      dplyr::ungroup()

    pairOutcome <- grouped %>% filter(treatment == 0) %>% pull(outcome) -
      grouped %>% filter(treatment == 1) %>% pull(outcome)
    pairPrediction <- grouped %>% filter(treatment == 0) %>% pull(predictedBenefit) / 2 +
      grouped %>% filter(treatment == 1) %>% pull(predictedBenefit) / 2

  }

  return(
    dplyr::tibble(
      pairOutcome   = pairOutcome,
      pairPrediction = pairPrediction
    )
  )

}
