library(magrittr)
library(purrr)
library(dplyr)
library(gsm.core)
library(gsm.mapping)
library(gsm.kri)
library(gsm.reporting)
load_all()

# 0. Input data
# ----

# Create a data specification from the data mapping workflows.
lDataSpec <- CombineSpecs(
  gsm.core::MakeWorkflowList(
    strPath = system.file('workflow', '1_mappings', package = 'gsm.simaerep'),
    strPackage = NULL
  )
)

# Ingest each data domain and apply its data specification.
lRawData <- Ingest(
  list(
    Raw_STUDY = clindata::ctms_study,
    Raw_SITE = clindata::ctms_site,
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_VISIT = clindata::rawplus_visdt,
    Raw_AE = clindata::rawplus_ae,
    Raw_PD = clindata::ctms_protdev
  ),
  lDataSpec,
  'Raw'
)

# 1. Mapped data
# ----

lMappingWorkflows <- gsm.core::MakeWorkflowList(
  strPath = system.file('workflow', '1_mappings', package = 'gsm.simaerep'),
  strPackage = NULL
)

lMappedData <- lMappingWorkflows %>%
  gsm.core::RunWorkflows(lRawData)

# 2. Analysis Data
# ----

lAnalysisWorkflows <- gsm.core::MakeWorkflowList(
  strPath = system.file('workflow', '2_metrics', package = 'gsm.simaerep'),
  strPackage = NULL
)

lAnalysisData <- lAnalysisWorkflows %>%
  gsm.core::RunWorkflows(lMappedData)

# 3. Reporting data
# ----

lReportingWorkflows <- c(
  gsm.core::MakeWorkflowList(c('Metrics', 'Groups'), strPackage = 'gsm.reporting'),
  gsm.core::MakeWorkflowList(
    strPath = system.file('workflow', '3_reporting', package = 'gsm.simaerep'),
    strPackage = NULL
  )
)

lReportingData <- lReportingWorkflows %>%
  gsm.core::RunWorkflows(
    c(
      lMappedData, # mapped CTMS data
      list(
        lWorkflows = lAnalysisWorkflows, # metric metadata
        lAnalyzed = lAnalysisData # analysis data
      )
    )
  )

# 4. Output
# ----

load_all()
lModuleWorkflows <- gsm.core::MakeWorkflowList(
  strPath = system.file('workflow', '4_modules', package = 'gsm.simaerep'),
  strPackage = NULL
)

gsm.core::RunWorkflows(
  lModuleWorkflows,
  lReportingData
)
