createCharacterizationModuleSpecifications <- function(
    targetIds,
    outcomeIds, # a vector of ids
    outcomeWashoutDays = c(365), # same length as outcomeIds with the outcomeWashout
    minPriorObservation = 365,
    dechallengeStopInterval = 30,
    dechallengeEvaluationWindow = 30,
    riskWindowStart = c(1, 1),
    startAnchor = c("cohort start", "cohort start"),
    riskWindowEnd = c(0, 365),
    endAnchor = c("cohort end", "cohort end"),
    minCharacterizationMean = 0.01,
    covariateSettings = FeatureExtraction::createCovariateSettings(
      useDemographicsGender = T,
      useDemographicsAge = T,
      useDemographicsAgeGroup = T,
      useDemographicsRace = T,
      useDemographicsEthnicity = T,
      useDemographicsIndexYear = T,
      useDemographicsIndexMonth = T,
      useDemographicsTimeInCohort = T,
      useDemographicsPriorObservationTime = T,
      useDemographicsPostObservationTime = T,
      useConditionGroupEraLongTerm = T,
      useDrugGroupEraOverlapping = T,
      useDrugGroupEraLongTerm = T,
      useProcedureOccurrenceLongTerm = T,
      useMeasurementLongTerm = T,
      useObservationLongTerm = T,
      useDeviceExposureLongTerm = T,
      useVisitConceptCountLongTerm = T,
      useConditionGroupEraShortTerm = T,
      useDrugGroupEraShortTerm = T,
      useProcedureOccurrenceShortTerm = T,
      useMeasurementShortTerm = T,
      useObservationShortTerm = T,
      useDeviceExposureShortTerm = T,
      useVisitConceptCountShortTerm = T,
      endDays = 0,
      longTermStartDays =  -365,
      shortTermStartDays = -30
    ),
    caseCovariateSettings = Characterization::createDuringCovariateSettings(
      useConditionGroupEraDuring = T,
      useDrugGroupEraDuring = T,
      useProcedureOccurrenceDuring = T,
      useDeviceExposureDuring = T,
      useMeasurementDuring = T,
      useObservationDuring = T,
      useVisitConceptCountDuring = T
    ),
    casePreTargetDuration = 365,
    casePostOutcomeDuration = 365,
    incremental = T
) {
  # input checks

  if(!inherits(outcomeIds, "numeric")){
    stop("outcomeIds must be a numeric or a numeric vector")
  }

  if(!inherits(outcomeWashoutDays, "numeric")){
    stop("outcomeWashoutDays must be a numeric or a numeric vector")
  }
  if(length(outcomeIds) != length(outcomeWashoutDays)){
    stop("outcomeWashoutDaysVector and outcomeIds must be same length")
  }
  if(length(minPriorObservation) != 1){
    stop("minPriorObservation needs to be length 1")
  }
  if(length(riskWindowStart) != length(startAnchor) |
     length(riskWindowEnd) != length(startAnchor) |
     length(endAnchor) != length(startAnchor))
     {
       stop("Time-at-risk settings must be same length")
  }

  # group the outcomeIds with the same outcomeWashoutDays
  outcomeWashoutDaysVector <- unique(outcomeWashoutDays)
  outcomeIdsList <- lapply(
    outcomeWashoutDaysVector,
    function(x){
      ind <- which(outcomeWashoutDays == x)
      unique(outcomeIds[ind])
    }
  )


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

  aggregateCovariateSettings <- list()

  for(i in 1:length(riskWindowStart)){
    for(j in 1:length(outcomeIdsList)){
      aggregateCovariateSettings[[length(aggregateCovariateSettings) + 1]] <- Characterization::createAggregateCovariateSettings(
        targetIds = targetIds,
        outcomeIds = outcomeIdsList[[j]],
        minPriorObservation = minPriorObservation,
        outcomeWashoutDays = outcomeWashoutDaysVector[j],
        riskWindowStart = riskWindowStart[i],
        startAnchor = startAnchor[i],
        riskWindowEnd = riskWindowEnd[i],
        endAnchor = endAnchor[i],
        covariateSettings = covariateSettings,
        caseCovariateSettings = caseCovariateSettings,
        casePreTargetDuration = casePreTargetDuration,
        casePostOutcomeDuration = casePostOutcomeDuration
      )
    }
  }


  analysis <- Characterization::createCharacterizationSettings(
    timeToEventSettings = list(timeToEventSettings),
    dechallengeRechallengeSettings = list(dechallengeRechallengeSettings),
    aggregateCovariateSettings = aggregateCovariateSettings
  )

  specifications <- list(
    module = "%module%",
    version = "%version%",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = list(
      analysis = analysis,
      minCharacterizationMean = minCharacterizationMean,
      incremental = incremental
      )
  )
  class(specifications) <- c("CharacterizationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}
