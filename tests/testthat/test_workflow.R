test_that("yaml workflow produces same table as R function", {

    mapping <- gsm::MakeWorkflowList(
        strNames = NULL,
        strPath = system.file("workflow/1_mappings", package = "gsm.simaerep"),
        strPackage = NULL
    )

    lRaw <- gsm::UseClindata(
        list(
            "Raw_SUBJ" = "clindata::rawplus_dm",
            "Raw_AE" = "clindata::rawplus_ae",
            "Raw_VISIT" = "clindata::rawplus_visdt",
            "Raw_PD" = "clindata::ctms_protdev"
        )
    )

    lMapped <- gsm::RunWorkflows(lWorkflows = mapping, lData = lRaw)

    kri_wf <- gsm::MakeWorkflowList(
        strNames = NULL,
        strPath = system.file("workflow/2_metrics", package = "gsm.simaerep"),
        strPackage = NULL
    )

    lResults <- gsm::RunWorkflows(lWorkflows = kri_wf, lData = lMapped)

  dfInputPD <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol  = "visit_dt"
  )

  dfInputAE <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "siteid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol  = "visit_dt"
  )

  expect_equal(dfInputAE, lResults$Analysis_kri0001)
  expect_equal(dfInputPD, lResults$Analysis_kri0002)

})
