# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of CharacterizationModule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Adding library references that are required for Strategus
library(CohortGenerator)
library(DatabaseConnector)
library(keyring)
library(ParallelLogger)
library(SqlRender)

# Adding RSQLite so that we can test modules with Eunomia
library(RSQLite)

# Module methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

execute <- function(jobContext) {
  rlang::inform("Validating inputs")
  inherits(jobContext, "list")

  if (is.null(jobContext$settings$analysis)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }

  workFolder <- jobContext$moduleExecutionSettings$workSubFolder # does this exist?

  rlang::inform("Executing Characterization")
  moduleInfo <- getModuleInfo()

  # run the models
  Characterization::runCharacterizationAnalyses(
    connectionDetails = jobContext$moduleExecutionSettings$connectionDetails,
    targetDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    targetTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
    outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
    cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
    characterizationSettings = jobContext$settings$analysis,
    databaseId = jobContext$moduleExecutionSettings$databaseId,
    saveDirectory = workFolder,
    tablePrefix = moduleInfo$TablePrefix,
    minCellCount = jobContext$moduleExecutionSettings$minCellCount,
    incremental = jobContext$settings$incremental,
    threads = parallel::detectCores(),
    minCharacterizationMean = jobContext$settings$minCharacterizationMean
  )

  # move results from work folder to output folder
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

  csvFileLoc <- file.path(workFolder,'results')
  csvFiles <- dir(csvFileLoc)
  for(csvFile in csvFiles){
    message(paste0('Exporting csv file ', csvFile))
    file.copy(
      from = file.path(csvFileLoc, csvFile),
      to = file.path(resultsFolder, csvFile)
    )
  }

  # Export the resultsDataModelSpecification.csv
  resultsDataModel <- CohortGenerator::readCsv(
    file = system.file(
      "settings/resultsDataModelSpecification.csv",
      package = "Characterization"
    ),
    warnOnCaseMismatch = FALSE
  )

  # add the prefix to the tableName column
  resultsDataModel$tableName <- paste0(moduleInfo$TablePrefix, resultsDataModel$tableName)

  CohortGenerator::writeCsv(
    x = resultsDataModel,
    file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
    warnOnCaseMismatch = FALSE,
    warnOnFileNameCaseMismatch = FALSE,
    warnOnUploadRuleViolations = FALSE
  )

}

createDataModelSchema <- function(jobContext) {
  checkmate::assert_class(jobContext$moduleExecutionSettings$resultsConnectionDetails, "ConnectionDetails")
  checkmate::assert_string(jobContext$moduleExecutionSettings$resultsDatabaseSchema)
  connectionDetails <- jobContext$moduleExecutionSettings$resultsConnectionDetails
  moduleInfo <- getModuleInfo()
  tablePrefix <- moduleInfo$TablePrefix
  resultsDatabaseSchema <- jobContext$moduleExecutionSettings$resultsDatabaseSchema
  # Workaround for issue https://github.com/tidyverse/vroom/issues/519:
  readr::local_edition(1)
  Characterization::createCharacterizationTables(
    connectionDetails = connectionDetails,
    resultSchema = resultsDatabaseSchema,
    targetDialect = connectionDetails$dbms,
    tablePrefix = moduleInfo$TablePrefix
  )
}
