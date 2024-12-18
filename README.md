
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gsm.simaerep

<!-- badges: start -->

[![R-CMD-check](https://github.com/IMPALA-Consortium/gsm.simaerep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/IMPALA-Consortium/gsm.simaerep/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/IMPALA-Consortium/gsm.simaerep/graph/badge.svg)](https://app.codecov.io/gh/IMPALA-Consortium/gsm.simaerep)
<!-- badges: end -->

This is a [{simaerep}](https://github.com/openpharma/simaerep/)
extension for the [{gsm}](https://github.com/Gilead-BioStats/gsm)
package.

This is in development the MVP will contain the following:

`gsm`-style functions as detailed
[here](https://gilead-biostats.github.io/gsm/articles/DataModel.html):

- Input_CumCount() patient-level cumulative count from source data
- Transform_Blank() this step is not needed for `simaerep`, but defined
  for consistency with `gsm`
- Analyze_Simaerep() will execute `simaerep`
- Flag_Simaerep() will flag based on `simaerep` statistics

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

Will be added soon.

## Quality Control

Since {gsm} is designed for use in a
[GCP](https://en.wikipedia.org/wiki/Good_clinical_practice) framework,
we have conducted extensive quality control as part of our development
process. In particular, we do the following during early development:

- **Unit Tests** - Unit tests are written for all core functions.
- **Workflow Tests** - Additional unit tests confirm that core workflows
  behave as expected.
- **Data Model** - Vignettes providing detailed descriptions of the data
  model.
- **Code Examples** - The Cookbook Vignette provides a series of simple
  examples, and all functions include examples as part of Roxygen
  documentation.
- **Function Documentation** - Detailed documentation for each function
  is maintained with Roxygen.
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
