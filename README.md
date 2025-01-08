
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gsm.simaerep

<!-- badges: start -->

[![R-CMD-check](https://github.com/IMPALA-Consortium/gsm.simaerep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/IMPALA-Consortium/gsm.simaerep/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This is a [{simaerep}](https://github.com/openpharma/simaerep/)
extension for the [{gsm}](https://github.com/Gilead-BioStats/gsm)
package.

This is in development the MVP will contain the following:

`gsm`-style functions as detailed
[here](https://gilead-biostats.github.io/gsm/articles/DataModel.html):

- Input_CumCount() patient-level cumulative count from source data
- Analyze_Simaerep() will execute `simaerep`
- Flag_Simaerep() will flag based on `simaerep` statistics
- report functions

required module configuration file:

- `inst/workflow/4_modules/gsm.simaerep.yaml`

## Installation

You can install the development version of gsm.simaerep from
[GitHub](https://github.com/IMPALA-Consortium/gsm.simaerep/) with:

``` r
# install.packages("pak")
pak::pak("Gilead-BioStats/clindata")
pak::pak("Gilead-BioStats/gsm")
pak::pak("IMPALA-Consortium/gsm.simaerep")
```

## Example

`simaerep` expects the cumulative count of numerator events per
denominator event per subject as input.

In this example we we are calculating the cumulative AE count per visit
per patient per site.

``` r
library(gsm.simaerep)

dfInput <- Input_CumCount(
   dfSubjects = clindata::rawplus_dm,
   dfNumerator = clindata::rawplus_ae,
   dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
   strSubjectCol = "subjid",
   strGroupCol = "siteid",
   strGroupLevel = "Site",
   strNumeratorDateCol = "aest_dt",
   strDenominatorDateCol  = "visit_dt"
 )

dfInput %>%
  dplyr::filter(max(Numerator) > 1, .by = "SubjectID") %>%
  head(25) %>%
  knitr::kable()
```

| SubjectID | GroupID | GroupLevel | Numerator | Denominator |
|:----------|:--------|:-----------|----------:|------------:|
| 0486      | 10      | Site       |         0 |           1 |
| 0486      | 10      | Site       |         0 |           2 |
| 0486      | 10      | Site       |         0 |           3 |
| 0486      | 10      | Site       |         0 |           4 |
| 0486      | 10      | Site       |         0 |           5 |
| 0486      | 10      | Site       |         0 |           6 |
| 0486      | 10      | Site       |         0 |           7 |
| 0486      | 10      | Site       |         0 |           8 |
| 0486      | 10      | Site       |         2 |           9 |
| 0486      | 10      | Site       |         2 |          10 |
| 0486      | 10      | Site       |         2 |          11 |
| 0486      | 10      | Site       |         2 |          12 |
| 0486      | 10      | Site       |         2 |          13 |
| 0486      | 10      | Site       |         2 |          14 |
| 0486      | 10      | Site       |         2 |          15 |
| 0486      | 10      | Site       |         2 |          16 |
| 0486      | 10      | Site       |         2 |          17 |
| 0486      | 10      | Site       |         2 |          18 |
| 0486      | 10      | Site       |         2 |          19 |
| 0486      | 10      | Site       |         2 |          20 |
| 0486      | 10      | Site       |         2 |          21 |
| 0489      | 10      | Site       |         0 |           1 |
| 0489      | 10      | Site       |         0 |           2 |
| 0489      | 10      | Site       |         2 |           3 |
| 0489      | 10      | Site       |         2 |           4 |

``` r

dfAnalyzed <- Analyze_Simaerep(dfInput)

dfAnalyzed %>%
  head() %>%
  knitr::kable()
```

| GroupID | MetricExpected | MetricGroup | OverReportingProbability | UnderReportingProbability |
|:---|---:|---:|---:|---:|
| 10 | 0.1628333 | 0.0205128 | 0.000 | 1.000 |
| 100 | 0.1628293 | 0.1463415 | 0.596 | 0.404 |
| 101 | 0.1706719 | 0.0625000 | 0.169 | 0.831 |
| 102 | 0.1747246 | 0.1159420 | 0.371 | 0.629 |
| 103 | 0.1725116 | 0.0930233 | 0.245 | 0.755 |
| 104 | 0.1639505 | 0.1373626 | 0.403 | 0.597 |

``` r

dfFlagged <- Flag_Simaerep(dfAnalyzed, vThreshold = c(0.95, 0.99))

dfFlagged %>%
  head() %>%
  knitr::kable()
```

| GroupID | MetricExpected | MetricGroup | OverReportingProbability | UnderReportingProbability | Flag |
|:---|---:|---:|---:|---:|---:|
| 10 | 0.1628333 | 0.0205128 | 0.000 | 1.000 | -2 |
| 100 | 0.1628293 | 0.1463415 | 0.596 | 0.404 | 0 |
| 101 | 0.1706719 | 0.0625000 | 0.169 | 0.831 | 0 |
| 102 | 0.1747246 | 0.1159420 | 0.371 | 0.629 | 0 |
| 103 | 0.1725116 | 0.0930233 | 0.245 | 0.755 | 0 |
| 104 | 0.1639505 | 0.1373626 | 0.403 | 0.597 | 0 |

## Quality Control

Since {gsm} is designed for use in a
[GCP](https://en.wikipedia.org/wiki/Good_clinical_practice) framework,
we have conducted extensive quality control as part of our development
process. In particular, we do the following during early development:

- **Unit Tests** - Unit tests are written for all core functions.
- **Workflow Tests** - Additional unit tests confirm that core workflows
  behave as expected.
- **Function Documentation** - Detailed documentation for each exported
  function with examples is maintained with Roxygen.
- **Package Checks** - Standard package checks are run using GitHub
  Actions and must be passing before PRs are merged.
- **Continuous Integration** - Continuous integration is provided via
  GitHub Actions.
- **Code Formatting** - Code is formatted with {styler} before each
  release.
- **Contributor Guidelines** - Detailed contributor guidelines including
  step-by-step processes for code development and releases are provided
  as a vignette.

### Parking

As development progresses, we will also conduct the following quality
control steps:

- **Code Examples** - Cookbook Vignette provides examples for code
  usage.
- **Qualification Workflow** - All assessments have been Qualified as
  described in the Qualification Workflow Vignette. A Qualification
  Report Vignette is generated and attached to each release.
- **Code Review** - Code review is conducted using GitHub Pull Requests
  (PRs), and a log of all PRs is included in the Qualification Report
  Vignette.
- **Data Specifications** - Machine-readable data specifications are
  maintained for all KRIs. Specifications are automatically added to
  relevant function documentation.
- **Regression Testing** - Extensive QC and testing is done before each
  release.
