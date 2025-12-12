mapping <- gsm.core::MakeWorkflowList(
  strNames = NULL,
  strPath = system.file("workflow/1_mappings", package = "gsm.simaerep"),
  strPackage = NULL
)

lRaw <- list(
  Raw_SUBJ = clindata::rawplus_dm,
  Raw_AE = clindata::rawplus_ae,
  Raw_VISIT = clindata::rawplus_visdt,
  Raw_PD = clindata::ctms_protdev,
  Raw_ENROLL = clindata::rawplus_enroll,
  Raw_SITE = clindata::ctms_site,
  Raw_STUDY = clindata::ctms_study,
  Raw_SDRGCOMP = clindata::rawplus_sdrgcomp
)

lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping))

lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping, lData = lIngest)

kri_wf <- gsm.core::MakeWorkflowList(
  strNames = NULL,
  strPath = system.file("workflow/2_metrics", package = "gsm.simaerep"),
  strPackage = NULL
)

lAnalysis <- gsm.core::RunWorkflows(lWorkflows = kri_wf, lData = lMapped)

dfMetrics <- gsm.reporting::MakeMetric(lWorkflows = kri_wf)

reporting_wf <- gsm.core::MakeWorkflowList(
  strNames = NULL,
  strPath = system.file("workflow/3_reporting", package = "gsm.simaerep"),
  strPackage = NULL
)


lReport <- gsm.core::RunWorkflows(
  lWorkflows = reporting_wf,
  lData = c(
    lMapped,
    list(
      lAnalyzed = lAnalysis,
      lWorkflows = kri_wf
    )
  )
)
