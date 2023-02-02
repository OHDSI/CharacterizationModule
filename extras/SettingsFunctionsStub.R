createCharacterizationModuleSpecifications <- function(targetIds,
                                                       outcomeIds,
                                                       dechallengeStopInterval = 30,
                                                       dechallengeEvaluationWindow = 30,
                                                       riskWindowStart = 1,
                                                       startAnchor = "cohort start",
                                                       riskWindowEnd = 0,
                                                       endAnchor = "cohort end",
                                                       covariateSettings = FeatureExtraction::createDefaultCovariateSettings()) {
  timeToEventSettings <- Characterization::createTimeToEventSettings(
    targetIds = targetIds,
    outcomeIds = outcomeIds
  )

  dechallengeRechallengeSettings <- Characterization::createDechallengeRechallengeSettings(
    targetIds = targetIds,
    outcomeIds = outcomeIds,
    dechallengeStopInterval = dechallengeStopInterval,
    dechallengeEvaluationWindow = dechallengeEvaluationWindow
  )

  aggregateCovariateSettings <- Characterization::createAggregateCovariateSettings(
    targetIds = targetIds,
    outcomeIds = outcomeIds,
    riskWindowStart = riskWindowStart,
    startAnchor = startAnchor,
    riskWindowEnd = riskWindowEnd,
    endAnchor = endAnchor,
    covariateSettings = covariateSettings
  )

  analysis <- Characterization::createCharacterizationSettings(
    timeToEventSettings = list(timeToEventSettings),
    dechallengeRechallengeSettings = list(dechallengeRechallengeSettings),
    aggregateCovariateSettings = list(aggregateCovariateSettings)
  )

  specifications <- list(
    module = "%module%",
    version = "%version%",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = analysis
  )
  class(specifications) <- c("CharacterizationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}
