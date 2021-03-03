#' Calculate the C-statistic for benefit
#'
#' @description Calculates the C-statistic for benefit for the evaluation of
#' discrimination
#'
#' @param data        A dataframe with columns ...
#' @param smoothFit   A smooth model for predicting absolute benefit
#' @param method      Can be "rank" ....
#' @export

calculateCForBenefit <- function(
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

    pairOutcomes <- grouped %>% filter(treatment == 0) %>% pull(outcome) -
      grouped %>% filter(treatment == 1) %>% pull(outcome)
    pairPrediction <- grouped %>% filter(treatment == 0) %>% pull(predictedBenefit) / 2 +
      grouped %>% filter(treatment == 1) %>% pull(predictedBenefit) / 2

    res <- AUC.trinary(
      xb.hat = pairPrediction,
      y      = pairOutcomes
    )
  }
  return(res$AUC)
}


