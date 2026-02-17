## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  message = FALSE, 
  warning = FALSE,
  fig.width = 7
)

CDMConnector::requireEunomia()

## ----message=FALSE, warning=FALSE---------------------------------------------
library(duckdb)
library(omopgenerics)
library(CDMConnector)
library(dplyr)

## ----message=TRUE, warning=FALSE----------------------------------------------
con <- dbConnect(duckdb(), dbdir = eunomiaDir())
cdm <- cdmFromCon(
  con = con, cdmSchem = "main", writeSchema = "main", cdmName = "Eunomia"
)
cdm 

## -----------------------------------------------------------------------------
respiratory_function_codes <- newCodelist(list(
  "respiratory_function" = c(4052083L, 4133840L, 3011505L)
))
respiratory_function_codes

## -----------------------------------------------------------------------------
library(CodelistGenerator)
code_use <- summariseCodeUse(respiratory_function_codes, cdm)
tableCodeUse(code_use)

## -----------------------------------------------------------------------------
library(MeasurementDiagnostics)

respiratory_function_measurements <- summariseMeasurementUse(
  cdm = cdm,
  codes = respiratory_function_codes
)

## -----------------------------------------------------------------------------
respiratory_function_measurements |> 
  glimpse()

## -----------------------------------------------------------------------------
tableMeasurementSummary(respiratory_function_measurements)

## -----------------------------------------------------------------------------
results <- summariseMeasurementUse(
  cdm = cdm,
  codes = respiratory_function_codes,
  byConcept = FALSE,
  byYear = FALSE,
  bySex = TRUE,
  ageGroup = list(
    age_group_narrow = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
    age_group_broad = list(c(0, 17), c(18, 64), c(65, 150))
  ),
  checks = "measurement_value_as_number"
)

# Show results stratified by broad age group
results |>
  filterStrata(age_group_broad != "overall") |>
  tableMeasurementValueAsNumber(
    header = "age_group_broad",
    groupColumn = character(),
    hide = c("age_group_narrow", "sex", "variable_level")
  )


## -----------------------------------------------------------------------------
results <- summariseMeasurementUse(
  cdm = cdm,
  codes = respiratory_function_codes,
  estimates = list(
    measurement_summary = c("min", "q25", "median", "q75", "max"),
    measurement_value_as_number = c(
      "min", "q25", "median", "q75", "max",
      "count_missing", "percentage_missing"
    ),
    measurement_value_as_concept = c("count", "percentage")
  )
)

results |>
  tableMeasurementValueAsNumber()


## -----------------------------------------------------------------------------
results <- summariseMeasurementUse(
  cdm = cdm,
  codes = respiratory_function_codes,
  estimates = NULL,
  histogram = list(
    "days_between_measurements" = list(
      '0 to 100' = c(0, 100), '100 to 200' = c(101, 200),
      '201 to 300' = c(201, 300), '301 to Inf' = c(301, Inf)
    ),
    "measurements_per_subject" = list(
      '0 to 1' = c(0, 1), '2 to 3' = c(2, 3),
      '4 to 5' = c(4, 5), '6 to 7' = c(6, 7),
      '8+' = c(8, 1000)
    )
  )
)

results |>
  plotMeasurementSummary(
    x = "variable_level", 
    plotType = "barplot",
    colour = "variable_level"
  )

## -----------------------------------------------------------------------------
results |>
  plotMeasurementSummary(
    x = "variable_level", 
    y = "measurements_per_subject",
    plotType = "barplot",
    colour = "variable_level"
  )

