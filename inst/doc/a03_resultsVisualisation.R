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
library(ggplot2)

cdm <- mockMeasurementDiagnostics()

# Example codelist we'll use in the examples
alkaline_phosphatase_codes <- list("alkaline_phosphatase" = c(3001467L, 45875977L))

## -----------------------------------------------------------------------------
result <- summariseMeasurementUse(
  cdm = cdm,
  codes = alkaline_phosphatase_codes,
  bySex = TRUE,
  byYear = FALSE,
  byConcept = FALSE,
  histogram = list(
    days_between_measurements = list(
      "0-30" = c(0, 30), "31-90" = c(31, 90), "91-365" = c(91, 365), "366+" = c(366, Inf)
    ),
    measurements_per_subject = list(
      "0" = c(0, 0), "1" = c(1, 1), "2-3" = c(2, 3), "4+" = c(4, 1000)
    ),
    value_as_number = list(
      "low" = c(0, 5.999), "mid" = c(6, 10.999), "high" = c(11, Inf)
    )
  )
)

## -----------------------------------------------------------------------------
# 1. Measurement summary table (timings / counts)
tableMeasurementSummary(
  result, 
  header = c("codelist_name", "sex"),
  hide = c("cdm_name", "domain_id")
)

# 2. Numeric-value summary table (values recorded as numbers)
tableMeasurementValueAsNumber(result)

# 3. Concept-value summary table (values recorded as concepts)
tableMeasurementValueAsConcept(result)

## -----------------------------------------------------------------------------
result |>
  plotMeasurementSummary(
    x = "codelist_name",
    y = "days_between_measurements",
    plotType = "boxplot"
  )

## -----------------------------------------------------------------------------
result |>
  plotMeasurementSummary(
    x = "sex",
    y = "measurements_per_subject",
    plotType = "boxplot",
    colour = "sex",
    facet = NULL
  ) +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
result |>
  plotMeasurementSummary(
    plotType = "densityplot",
    colour = "sex", 
    facet = NULL
  )

## -----------------------------------------------------------------------------
result |>
  plotMeasurementSummary(
    y = "measurements_per_subject",
    plotType = "densityplot",
    colour = "sex", 
    facet = NULL
  )

## -----------------------------------------------------------------------------
result |>
  plotMeasurementSummary(
    x = "variable_level",
    plotType = "barplot",
    colour = "variable_level", 
    facet = "sex"
  )

## -----------------------------------------------------------------------------
result |>
  plotMeasurementSummary(
    y = "measurements_per_subject",
    plotType = "barplot",
    colour = "sex", 
    facet = "variable_level"
  )

## -----------------------------------------------------------------------------
result |> 
  plotMeasurementValueAsNumber(
    x = "sex",
    plotType = "boxplot",
    facet = "unit_concept_name",
    colour = "sex"
  )

## -----------------------------------------------------------------------------
result |> 
  plotMeasurementValueAsNumber(
    plotType = "densityplot",
    facet = "unit_concept_name",
    colour = "sex"
  )

## -----------------------------------------------------------------------------
result |> 
  plotMeasurementValueAsNumber(
    x = "unit_concept_name",
    plotType = "barplot",
    facet = c("sex"),
    colour = "variable_level"
  )

## -----------------------------------------------------------------------------
result |>
  plotMeasurementValueAsConcept(
    x = "count",
    y = "variable_level",
    facet = "cdm_name",
    colour = "sex"
  ) +
  ylab("Value as Concept Name")

## -----------------------------------------------------------------------------
result |>
  plotMeasurementValueAsConcept(
    x = "variable_level",
    y = "percentage",
    facet = "cdm_name",
    colour = "sex"
  ) +
  xlab("Value as Concept Name") 

## ----eval=FALSE---------------------------------------------------------------
# library(OmopViewer)
# exportStaticApp(result = result, directory = tempdir())

