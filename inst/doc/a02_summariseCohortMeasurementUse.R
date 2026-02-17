## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  message = FALSE, 
  warning = FALSE,
  fig.width = 7
)

CDMConnector::requireEunomia()

## -----------------------------------------------------------------------------
library(MeasurementDiagnostics)
library(dplyr)
library(omopgenerics) 
library(CohortConstructor)

cdm <- mockMeasurementDiagnostics()
cdm

## -----------------------------------------------------------------------------
result <- summariseCohortMeasurementUse(
  codes = list("measurement_codelist" = c(3001467L, 45875977L)),
  cohort = cdm$my_cohort
)

# Inspect structure
result |> glimpse()

## -----------------------------------------------------------------------------
tableMeasurementValueAsConcept(result)

## -----------------------------------------------------------------------------
tableMeasurementValueAsNumber(result)

## -----------------------------------------------------------------------------
result_any <- summariseCohortMeasurementUse(
  codes = list("measurement_codelist" = c(3001467L, 45875977L)),
  cohort = cdm$my_cohort,
  timing = "any"
)

result_cohort_start <- summariseCohortMeasurementUse(
  codes = list("measurement_codelist" = c(3001467L, 45875977L)),
  cohort = cdm$my_cohort,
  timing = "cohort_start_date"
)

tableMeasurementSummary(result_any)
tableMeasurementSummary(result_cohort_start)

## -----------------------------------------------------------------------------
cdm$measurement_cohort <- conceptCohort(
  cdm = cdm,
  conceptSet = list("measurement_codelist" = c(3001467L, 45875977L)),
  name = "measurement_cohort"
)
cohortCodelist(cdm$measurement_cohort)

## -----------------------------------------------------------------------------
result <- summariseCohortMeasurementUse(
  cohort = cdm$measurement_cohort,
  timing = "cohort_start_date"
)
tableMeasurementValueAsNumber(result)

## -----------------------------------------------------------------------------
result <- summariseCohortMeasurementUse(
  cohort = cdm$measurement_cohort,
  bySex = TRUE,
  byConcept = FALSE,
  timing = "any",
  checks = "measurement_summary"
)
tableMeasurementSummary(result)

