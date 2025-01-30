# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
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
defaultSettings <- function(
    binary = TRUE,
    includeAllOutcomes = FALSE,
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    requireTimeAtRisk = FALSE,
    minTimeAtRisk = 0,
    riskWindowStart = 0,
    startAnchor = "cohort start",
    riskWindowEnd = 365,
    endAnchor = "cohort start") {
  result <- createStudyPopulationSettings(
    binary = binary,
    includeAllOutcomes = includeAllOutcomes,
    firstExposureOnly = firstExposureOnly,
    washoutPeriod = washoutPeriod,
    removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
    priorOutcomeLookback = priorOutcomeLookback,
    requireTimeAtRisk = requireTimeAtRisk,
    minTimeAtRisk = minTimeAtRisk,
    riskWindowStart = riskWindowStart,
    startAnchor = startAnchor,
    riskWindowEnd = riskWindowEnd,
    endAnchor = endAnchor
  )

  return(result)
}


test_that("createStudyPopulation correct class", {
  populationSettings <- createStudyPopulationSettings()
  expect_s3_class(populationSettings, "populationSettings")
})

test_that("createStudyPopulation binary", {
  populationSettings <- createStudyPopulationSettings(
    binary = TRUE,
  )
  expect_true(populationSettings$binary)

  populationSettings <- createStudyPopulationSettings(
    binary = FALSE,
  )
  expect_false(populationSettings$binary)
})

test_that("createStudyPopulation includeAllOutcomes", {
  expect_error(
    createStudyPopulationSettings(includeAllOutcomes = NULL)
  )

  expect_error(
    createStudyPopulationSettings(includeAllOutcomes = 12)
  )

  populationSettings <- createStudyPopulationSettings(
    includeAllOutcomes = TRUE,
  )
  expect_true(populationSettings$includeAllOutcomes)

  populationSettings <- createStudyPopulationSettings(
    includeAllOutcomes = FALSE,
  )
  expect_false(populationSettings$includeAllOutcomes)
})

test_that("createStudyPopulation firstExposureOnly", {
  expect_error(
    createStudyPopulationSettings(firstExposureOnly = NULL)
  )

  expect_error(
    createStudyPopulationSettings(firstExposureOnly = 12)
  )

  populationSettings <- createStudyPopulationSettings(
    firstExposureOnly = TRUE,
  )
  expect_true(populationSettings$firstExposureOnly)

  populationSettings <- createStudyPopulationSettings(
    firstExposureOnly = FALSE,
  )
  expect_false(populationSettings$firstExposureOnly)
})


test_that("createStudyPopulation washoutPeriod", {
  expect_error(
    createStudyPopulationSettings(washoutPeriod = NULL)
  )

  expect_error(
    createStudyPopulationSettings(washoutPeriod = -1)
  )

  rand <- sample(1000, 1)
  populationSettings <- createStudyPopulationSettings(
    washoutPeriod = rand,
  )
  expect_equal(populationSettings$washoutPeriod, rand)
})


test_that("createStudyPopulation removeSubjectsWithPriorOutcome", {
  expect_error(
    createStudyPopulationSettings(removeSubjectsWithPriorOutcome = NULL)
  )

  expect_error(
    createStudyPopulationSettings(removeSubjectsWithPriorOutcome = 12)
  )

  populationSettings <- createStudyPopulationSettings(
    removeSubjectsWithPriorOutcome = TRUE,
  )
  expect_true(populationSettings$removeSubjectsWithPriorOutcome)

  populationSettings <- createStudyPopulationSettings(
    removeSubjectsWithPriorOutcome = FALSE,
  )
  expect_false(populationSettings$removeSubjectsWithPriorOutcome)
})

test_that("createStudyPopulation priorOutcomeLookback", {
  expect_error(
    createStudyPopulationSettings(priorOutcomeLookback = NULL)
  )

  expect_error(
    createStudyPopulationSettings(priorOutcomeLookback = -1)
  )

  rand <- sample(1000, 1)
  populationSettings <- createStudyPopulationSettings(
    priorOutcomeLookback = rand,
  )
  expect_equal(populationSettings$priorOutcomeLookback, rand)
})

test_that("createStudyPopulation requireTimeAtRisk", {
  expect_error(
    createStudyPopulationSettings(requireTimeAtRisk = NULL)
  )

  expect_error(
    createStudyPopulationSettings(requireTimeAtRisk = 12)
  )

  populationSettings <- createStudyPopulationSettings(
    requireTimeAtRisk = TRUE,
  )
  expect_true(populationSettings$requireTimeAtRisk)

  populationSettings <- createStudyPopulationSettings(
    requireTimeAtRisk = FALSE,
  )
  expect_false(populationSettings$requireTimeAtRisk)
})

test_that("createStudyPopulation minTimeAtRisk", {
  expect_error(
    createStudyPopulationSettings(minTimeAtRisk = NULL)
  )

  expect_error(
    createStudyPopulationSettings(minTimeAtRisk = -1)
  )

  rand <- 365 + sample(365, 1)
  populationSettings <- createStudyPopulationSettings(
    minTimeAtRisk = rand,
    riskWindowEnd = 1000
  )
  expect_equal(populationSettings$minTimeAtRisk, rand)
})


test_that("createStudyPopulation riskWindowStart", {
  expect_error(
    createStudyPopulationSettings(riskWindowStart = NULL)
  )


  rand <- sample(1000, 1)
  populationSettings <- createStudyPopulationSettings(
    riskWindowStart = rand,
    riskWindowEnd = rand + 365
  )
  expect_equal(populationSettings$riskWindowStart, rand)
})

test_that("createStudyPopulation riskWindowEnd", {
  expect_error(
    createStudyPopulationSettings(riskWindowEnd = NULL)
  )


  rand <- sample(1000, 1)
  populationSettings <- createStudyPopulationSettings(
    riskWindowEnd = rand,
    minTimeAtRisk = 1
  )
  expect_equal(populationSettings$riskWindowEnd, rand)
})

test_that("createStudyPopulation startAnchor", {
  expect_error(
    createStudyPopulationSettings(startAnchor = NULL)
  )

  expect_error(
    createStudyPopulationSettings(startAnchor = "blabla")
  )

  rand <- c("cohort end", "cohort start")[sample(2, 1)]
  populationSettings <- createStudyPopulationSettings(
    startAnchor = rand,
  )
  expect_equal(populationSettings$startAnchor, rand)
})

test_that("createStudyPopulation endAnchor", {
  expect_error(
    createStudyPopulationSettings(endAnchor = NULL)
  )

  expect_error(
    createStudyPopulationSettings(endAnchor = "blabla")
  )

  rand <- c("cohort end", "cohort start")[sample(2, 1)]
  populationSettings <- createStudyPopulationSettings(
    endAnchor = rand,
  )
  expect_equal(populationSettings$endAnchor, rand)
})


# Test unit for the creation of the study population. The firstExposureOnly,
# washout, requireTimeAtRisk are checked. Additionally, error messages are checked.

test_that("population creation parameters", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  studyPopulation <- createStudyPopulation(
    plpData = plpData,
    outcomeId = outcomeId,
    populationSettings = defaultSettings()
  )

  expect_s3_class(studyPopulation, "data.frame")

  nrOutcomes1 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes1, 0)

  # firstExposureOnly test (should have no effect on simulated data)
  studyPopulation <- createStudyPopulation(
    plpData = plpData,
    outcomeId = outcomeId,
    populationSettings = defaultSettings(firstExposureOnly = TRUE)
  )

  nrOutcomes2 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes2, 0)
  expect_equal(nrOutcomes1, nrOutcomes2)

  # requireTimeAtRisk
  studyPopulation <- createStudyPopulation(
    plpData = plpData,
    outcomeId = outcomeId,
    populationSettings = defaultSettings(requireTimeAtRisk = TRUE)
  )

  nrOutcomes3 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes3, 0)
  expect_true(nrOutcomes3 <= nrOutcomes1)

  # expect warning due to no people left with
  # minTimeAtRisk of 999999
  expect_warning(
    createStudyPopulation(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = defaultSettings(requireTimeAtRisk = TRUE, minTimeAtRisk = 99999)
    )
  )

  studyPopulation <- createStudyPopulation(
    plpData = plpData,
    outcomeId = outcomeId,
    populationSettings = defaultSettings(washoutPeriod = 365)
  )
  nrOutcomes4 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes4, 0)
  expect_true(nrOutcomes4 <= nrOutcomes1)

  # washoutPeriod <0 error
  expect_error(
    createStudyPopulation(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = defaultSettings(washoutPeriod = -1)
    )
  )

  # priorOutcomeLookback < 0 error
  expect_error(
    createStudyPopulation(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = defaultSettings(
        priorOutcomeLookback = -1,
        removeSubjectsWithPriorOutcome = TRUE
      )
    )
  )

  expect_error(
    createStudyPopulation(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = defaultSettings(
        minTimeAtRisk = -1,
        requireTimeAtRisk = TRUE
      )
    )
  )

  # Incorrect startAnchor
  expect_error(
    createStudyPopulation(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = defaultSettings(
        startAnchor = "cohort stard"
      )
    )
  )

  # Incorrect endAnchor
  expect_error(
    createStudyPopulation(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = defaultSettings(
        endAnchor = "cohort ent"
      )
    )
  )


  # check outcomes that only have partial timeatrisk are included:

  outcomes <- data.frame(
    rowId = c(1, 1, 1, 4, 5),
    outcomeId = c(1, 1, 1, 1, 2),
    outcomeCount = rep(1, 5),
    daysToEvent = c(-30, 30, 180, 60, 4)
  )
  cohorts <- data.frame(
    rowId = 1:20,
    subjectId = 1:20,
    targetId = rep(2, 20),
    time = rep(365, 20),
    ageYear = rep(18, 20),
    gender = rep(8507, 20),
    cohortStartDate = rep("2012-04-12", 20),
    daysFromObsStart = rep(740, 20),
    daysToCohortEnd = rep(1, 20),
    daysToObsEnd = c(40, rep(900, 19))
  )
  pPlpData <- plpData
  pPlpData$outcomes <- outcomes
  pPlpData$cohorts <- cohorts

  attr(pPlpData$cohorts, "metaData") <- list(attrition = data.frame(
    outcomeId = 1, description = "test",
    targetCount = 20, uniquePeople = 20,
    outcomes = 3
  ))

  pop <- createStudyPopulation(
    plpData = pPlpData,
    outcomeId = 1,
    populationSettings = defaultSettings(
      includeAllOutcomes = TRUE,
      requireTimeAtRisk = TRUE,
      minTimeAtRisk = 365
    )
  )

  # person 1 and 4 should be retruned
  expect_equal(pop$rowId[pop$outcomeCount > 0], c(1, 4))

  pop2 <- createStudyPopulation(
    plpData = pPlpData,
    outcomeId = 1,
    populationSettings = defaultSettings(
      includeAllOutcomes = TRUE,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 99999,
      requireTimeAtRisk = TRUE,
      minTimeAtRisk = 365
    )
  )

  # person 4 only as person 1 has it before
  expect_equal(pop2$rowId[pop2$outcomeCount > 0], c(4))

  pop3 <- createStudyPopulation(
    plpData = pPlpData,
    outcomeId = 1,
    populationSettings = defaultSettings(
      includeAllOutcomes = FALSE,
      firstExposureOnly = FALSE,
      removeSubjectsWithPriorOutcome = FALSE,
      priorOutcomeLookback = 99999,
      requireTimeAtRisk = TRUE,
      minTimeAtRisk = 365
    )
  )

  # 4 only should be retruned
  expect_equal(pop3$rowId[pop3$outcomeCount > 0], c(4))

  # creates min warning due to no data...
  pop5 <- createStudyPopulation(
    plpData = pPlpData,
    outcomeId = 1,
    populationSettings = defaultSettings(
      binary = TRUE,
      includeAllOutcomes = FALSE,
      firstExposureOnly = FALSE,
      washoutPeriod = 0,
      removeSubjectsWithPriorOutcome = FALSE,
      priorOutcomeLookback = 99999,
      requireTimeAtRisk = TRUE,
      minTimeAtRisk = 303,
      riskWindowStart = 62,
      startAnchor = "cohort start",
      riskWindowEnd = 365,
      endAnchor = "cohort start"
    )
  )

  # should have no outcomes
  expect_equal(is.null(pop5), TRUE)
})

test_that("Providing an existing population and skipping population creation works", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  popSize <- 400
  newPopulation <- population[sample.int(nrow(population), popSize), ]

  tinyPlpData$population <- newPopulation

  tempResults <- runPlp(
    plpData = tinyPlpData,
    outcomeId = 2,
    analysisId = "1",
    analysisName = "existing population",
    populationSettings = createStudyPopulationSettings(),
    splitSettings = createDefaultSplitSetting(),
    modelSettings = setLassoLogisticRegression(),
    executeSettings = createExecuteSettings(
      runSplitData = TRUE,
      runPreprocessData = FALSE,
      runModelDevelopment = TRUE
    ),
    saveDirectory = file.path(saveLoc, "existingPopulation")
  )

  trainPredictions <- tempResults$prediction %>%
    dplyr::filter(.data$evaluationType == "Train") %>%
    nrow()
  testPredictions <- tempResults$prediction %>%
    dplyr::filter(.data$evaluationType == "Test") %>%
    nrow()
  expect_equal(popSize, trainPredictions + testPredictions)
})
