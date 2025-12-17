## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", message = FALSE, warning = FALSE,
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
repiratory_function_codes <- newCodelist(list("respiratory function" = c(4052083, 4133840, 3011505)))
repiratory_function_codes

## -----------------------------------------------------------------------------
library(CodelistGenerator)
code_use <- summariseCodeUse(repiratory_function_codes, cdm)
tableCodeUse(code_use)

## -----------------------------------------------------------------------------
library(MeasurementDiagnostics)

repiratory_function_measurements <- summariseMeasurementUse(cdm, repiratory_function_codes)

## -----------------------------------------------------------------------------
repiratory_function_measurements |> 
  glimpse()

## -----------------------------------------------------------------------------
settings(repiratory_function_measurements) |> 
  pull("result_type") |> 
  unique()

## -----------------------------------------------------------------------------
tableMeasurementValueAsNumber(repiratory_function_measurements)

## -----------------------------------------------------------------------------
tableMeasurementValueAsConcept(repiratory_function_measurements)

## -----------------------------------------------------------------------------
tableMeasurementSummary(repiratory_function_measurements)

