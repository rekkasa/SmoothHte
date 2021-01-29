# library(SimulateHte)
# library(dplyr)
# library(SmoothHte)
# library(rms)
#
# databaseSettings <- createDatabaseSettings(
#   numberOfObservations = 1e4,
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
#     constant = -0.5108256
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
# mod <- glm(
#   outcome ~ x1 + x2 + x3 + x4 + treatment,
#   data = dd,
#   family = "binomial"
# )
#
# predictData <- dd %>%
#   mutate(treatment = 0)
#
# dd <- dd %>%
#   dplyr::mutate(
#     riskLinearPredictor = predict(mod, newdata = predictData)
#   )
#
# dd0 <- dd %>%
#   filter(treatment == 0)
#
# dd1 <- dd %>%
#   filter(treatment == 1)
#
# q <- quantile(dd$riskLinearPredictor, c(.025, .975))
# x <- seq(q[1], q[2], .001)
# s0 = fitLoessHte(dd0, createLoessSettings())
# s1 = fitLoessHte(dd1, createLoessSettings())
#
# r0 <- fitRcsHte(dd0, createRcsSettings())
# r1 <- fitRcsHte(dd1, createRcsSettings())
#
# plot(
#   plogis(x),
#   predictSmooth(s0, x) - predictSmooth(s1, x),
#   pch = ".",
#   ylim = c(0, .2)
# )
# lines(
#   plogis(x),
#   plogis(predictSmooth(r0, x)) - plogis(predictSmooth(r1, x)),
#   col = "blue"
# )
# lines(
#   plogis(x),
#   plogis(x) - plogis(x -0.5108256),
#   col = "red",
#   lty = 2,
#   lwd = 4
# )
# histSpike(plogis(dd$riskLinearPredictor), add=T, side=1, nint=300, frac=.1)
