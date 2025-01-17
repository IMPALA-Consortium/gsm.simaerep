test_that("yaml workflow produces same table as R function", {

    mapping <- gsm::MakeWorkflowList(
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
      Raw_SITE = clindata::ctms_site  %>%
        rename(studyid = protocol) %>%
        rename(invid = pi_number) %>%
        rename(InvestigatorFirstName = pi_first_name) %>%
        rename(InvestigatorLastName = pi_last_name) %>%
        rename(City = city) %>%
        rename(State = state) %>%
        rename(Country = country) %>%
        rename(Status = site_status),
      Raw_STUDY = clindata::ctms_study %>%
        rename(studyid = protocol_number) %>%
        rename(Status = status)
    )

    lMapped <- gsm::RunWorkflows(lWorkflows = mapping, lData = lRaw)

    kri_wf <- gsm::MakeWorkflowList(
        strNames = NULL,
        strPath = system.file("workflow/2_metrics", package = "gsm.simaerep"),
        strPackage = NULL
    )

    lAnalysis <- gsm::RunWorkflows(lWorkflows = kri_wf, lData = lMapped)

    dfMetrics <- gsm::MakeMetric(lWorkflows = kri_wf)

  dfInputPD <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::ctms_protdev %>% rename(subjid = subjectenrollmentnumber),
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol  = "visit_dt",
    # we are not testing "assign", b/c this introduced random element
    strOrphanedMethod = "filter"
  )

  dfInputAE <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = clindata::rawplus_ae,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol  = "visit_dt"
  )

  expect_equal(dfInputAE, lAnalysis$Analysis_kri0001$Analysis_Input)
  expect_equal(dfInputPD, lAnalysis$Analysis_kri0003$Analysis_Input)

  dfAnalyzedAE <- Analyze_Simaerep(dfInputAE)
  dfAnalyzedPD <- Analyze_Simaerep(dfInputPD)

  # we can only check the non-random elements for equality
  expect_equal(
    select(dfAnalyzedAE, - MetricExpected, - OverReportingProbability, - UnderReportingProbability, - Score),
    select(
      lAnalysis$Analysis_kri0001$Analysis_Analyzed,
      - MetricExpected, - OverReportingProbability, - UnderReportingProbability, - Score
    )
  )

  expect_equal(
    select(dfAnalyzedPD, - MetricExpected, - OverReportingProbability, - UnderReportingProbability, - Score),
    select(
      lAnalysis$Analysis_kri0003$Analysis_Analyzed,
      - MetricExpected, - OverReportingProbability, - UnderReportingProbability, - Score
    )
  )

  dfFlaggedAE <- Flag_Simaerep(dfAnalyzedAE, vThreshold = c(-0.99, -0.95, 0.95, 0.99))
  dfFlaggedPD <- Flag_Simaerep(dfAnalyzedPD, vThreshold = c(-0.99, -0.95, 0.95, 0.99))

  n_sites_flaggedAE <- sum(dfFlaggedAE$Flag > 0)
  n_sites_flaggedPD <- sum(dfFlaggedPD$Flag > 0)

  n_sites_flaggedAE_wflow <- sum(lAnalysis$Analysis_kri0001$Analysis_Flagged$Flag > 0)
  n_sites_flaggedPD_wflow <- sum(lAnalysis$Analysis_kri0003$Analysis_Flagged$Flag > 0)

  tolerance = dplyr::n_distinct(dfFlaggedAE$GroupID) * 0.02

  expect_true(between(n_sites_flaggedAE_wflow, n_sites_flaggedAE - tolerance, n_sites_flaggedAE + tolerance))
  expect_true(between(n_sites_flaggedPD_wflow, n_sites_flaggedPD - tolerance, n_sites_flaggedPD + tolerance))

  sites_flaggedAE2 <- dfFlaggedAE %>%
    filter(Flag == 2) %>%
    pull(GroupID)

  sites_flaggedAE <- dfFlaggedAE %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedPD2 <- dfFlaggedPD %>%
    filter(Flag == 2) %>%
    pull(GroupID)

  sites_flaggedPD <- dfFlaggedPD %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedAE_wflow <- lAnalysis$Analysis_kri0001$Analysis_Flagged %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedAE2_wflow <- lAnalysis$Analysis_kri0001$Analysis_Flagged %>%
    filter(Flag == 2) %>%
    pull(GroupID)

  sites_flaggedPD_wflow <- lAnalysis$Analysis_kri0003$Analysis_Flagged %>%
    filter(Flag > 0) %>%
    pull(GroupID)

  sites_flaggedPD2_wflow <- lAnalysis$Analysis_kri0003$Analysis_Flagged %>%
    filter(Flag > 2) %>%
    pull(GroupID)

  expect_true(all(sites_flaggedAE2 %in% sites_flaggedAE_wflow))
  expect_true(all(sites_flaggedAE2_wflow %in% sites_flaggedAE))

  expect_true(all(sites_flaggedPD2 %in% sites_flaggedPD_wflow))
  expect_true(all(sites_flaggedPD2_wflow %in% sites_flaggedPD))

})
