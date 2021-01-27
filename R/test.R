# library(SimulateHte)
# library(dplyr)
#
# databaseSettings <- createDatabaseSettings(
#   numberOfObservations = 2e4,
#   numberOfCovariates = 4,
#   covariateDistributionSettings = list(
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings(),
#     createNormalDistributionSettings()
#   )
# )
#
#
# baselineRiskSettings <- createBaselineRiskSettings(
#   type = "binary",
#   modelSettings = createModelSettings(
#     constant = -1.5,
#     modelMatrix = diag(4),
#     transformationSettings = list(
#       identity,
#       identity,
#       identity,
#       identity
#     ),
#     coefficients = rep(.8, 4)
#   )
# )
#
# propensitySettings <- createPropensitySettings(
#   type = "binary",
#   modelSettings = createModelSettings(
#     constant = 0,
#     modelMatrix = diag(0)
#   )
# )
#
# treatmentEffectSettings <- createTreatmentEffectSettings(
#   type = "lp",
#   modelSettings = createModelSettings(
#     constant = -0.52
#   )
# )
#
#
#
# dd <- runDataGeneration(
#   databaseSettings,
#   propensitySettings,
#   baselineRiskSettings,
#   treatmentEffectSettings
# )  %>%
#   select(-propensityLinearPredictor)
#
# dd0 <- dd %>%
#   filter(treatment == 0)
#
# dd1 <- dd %>%
#   filter(treatment == 1)
#
# dd00 <- dd0 %>%
#   select(-c(riskLinearPredictor)) %>%
#   rename("riskLinearPredictor" = "observedRiskLinearPredictor")
#
# dd11 <- dd1 %>%
#   select(-c(riskLinearPredictor)) %>%
#   rename("riskLinearPredictor" = "observedRiskLinearPredictor")
#
#
# q <- quantile(dd$observedRiskLinearPredictor, c(.025, .975))
# x <- seq(q[1], q[2], .001)
# s0 = fitLoessHte(dd00, createLoessSettings())
# s1 = fitLoessHte(dd11, createLoessSettings())
# plot(
#   plogis(x),
#   plogis(s0$predict(x = x, smoothFit = s0$smooth)) - plogis(s1$predict(x = x, s1$smooth)),
#   pch = "."
# )
