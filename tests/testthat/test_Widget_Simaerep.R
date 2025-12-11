
test_that("Widget_Simaerep creates a valid HTML widget", {

    dfInput <- Input_CumCount(
        dfSubjects = clindata::rawplus_dm,
        dfNumerator = clindata::rawplus_ae,
        dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
        strSubjectCol = "subjid",
        strGroupCol = "invid",
        strGroupLevel = "Site",
        strNumeratorDateCol = "aest_dt",
        strDenominatorDateCol = "visit_dt"
    )

    dfAnalyzed <- Analyze_Simaerep(dfInput)
    dfFlagged <- Flag_Simaerep(dfAnalyzed, vThreshold = c(-0.99, -0.95, 0.95, 0.99))

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

    mapping_wf <- gsm.core::MakeWorkflowList(
      strNames = NULL,
      strPath = system.file("workflow/1_mappings", package = "gsm.simaerep"),
      strPackage = NULL
    )

    lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))

    lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)

    dfGroups <- dplyr::bind_rows(
      lMapped$Mapped_STUDY,
      lMapped$Mapped_SITE,
      lMapped$Country
    )

    metrics_wf <- gsm.core::MakeWorkflowList(
      strNames = NULL,
      strPath = system.file("workflow/2_metrics", package = "gsm.simaerep"),
      strPackage = NULL
    )

    lMetric <- gsm.reporting::MakeMetric(lWorkflows = metrics_wf)[1, ]

    # Create site list widget
    w <- Widget_Simaerep(
        dfInput = dfInput,
        dfFlagged = dfFlagged,
        dfGroups = dfGroups,
        lMetric = lMetric
    )

    expect_s3_class(w, "htmlwidget")

    vColors <- c("0" = "#9ED782", "1" = "#FEAA01", "2" = "#FF5858", "-1" = "#FEAA01", "-2" = "#FF5858")

    w <- Widget_Simaerep(
      dfInput = dfInput,
      dfFlagged = dfFlagged,
      dfGroups = dfGroups,
      lMetric = lMetric,
      vColors = vColors
    )

    expect_true(stringr::str_detect(attr(w, "output_label"), "Simaerep"))

})
