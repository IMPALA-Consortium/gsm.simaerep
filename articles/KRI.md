# gsm.simaerep Details

### Installation

``` r
install.packages("pak")
pak::pak("Gilead-BioStats/clindata")
pak::pak("Gilead-BioStats/gsm.core")
pak::pak("Gilead-BioStats/gsm.mapping")
pak::pak("Gilead-BioStats/gsm.kri")
pak::pak("Gilead-BioStats/gsm.reporting")
pak::pak("IMPALA-Consortium/gsm.simaerep")
```

### Load

``` r
suppressPackageStartupMessages(library(dplyr))
library(gsm.core)
library(gsm.mapping)
library(gsm.kri)
library(gsm.reporting)
library(gsm.simaerep)
#> 
#> Attaching package: 'gsm.simaerep'
#> The following object is masked from 'package:gsm.kri':
#> 
#>     MakeCharts
```

## {simaerep} Defaults

- 1000 replicates of bootstrap resampling iterations
- By default were are using the thresholds 99%, 95% and do not apply
  multiplicity correction
- Events NA/NULL in numerator of denominator date columns will be
  dropped
- By default {gsm.simaerep} uses all denominator events. When visits are
  used as denominator events filtering out unscheduled visits will
  result in greater snychronisation of the visit counts which will be
  more accurrate especially in small trials.

A higher number of replicates will produce more stable results
especially for KRI based on low event counts such as treatment
discontinuations of which only a small fraction of patients will have at
most one event. [kri0008.yaml Treatment
Discontinuation](https://github.com/IMPALA-Consortium/gsm.simaerep/blob/main/inst/workflow/2_metrics/kri0008.yaml)
provides an example on how to change the number of iterations.

Multiplicity Correction reduces the noise especially for trials with a
high number of sites and makes outliers stand out more. It is the
default setting for {simaerep} as results appear more plausible when
looking at a single study. However, the Benjamini-Hochberg procedure
eliminates small differences between the probability scores and assigns
shared ranks and identical scores to some of the sites. This makes
results less reproducible. On a portfolio level we have also observed a
small decrease in overall statistical performance when multiplicity
correction is used.

We recommend multiplicity correction if the purpose of the probability
scoring is to flag sites for visually inspection on a plot displaying
all sites of the study at once [kri0006.yaml Treatment
Discontinuation](https://github.com/IMPALA-Consortium/gsm.simaerep/blob/main/inst/workflow/2_metrics/kri0006.yaml)
demonstrates how to switch on the multipicity correction. We advise to
decrease the threshold to 95% and 75%. Increasing the number of
bootstrap iterations will also help to get more reproducible results.

When dropping of event with NA date values is not acceptable relevant
date imputation steps need to be added.

## {gsm.simaerep} Preprocessing Options

[`Input_CumCount()`](https://impala-consortium.github.io/gsm.simaerep/reference/Input_CumCount.md)
has some more complex built-in pre-processing options that can handle
special types of clinical events that surpass the simple imputation of
missing dates.

### Orphaned Clinical Events

Sometimes clinical events such as protocol deviations will be assigned
to a site but not to a subject. {simaerep} strictly requires events to
be assigned to a subject for including them in the simulations.

[`Input_CumCount()`](https://impala-consortium.github.io/gsm.simaerep/reference/Input_CumCount.md)
can treat such events in two ways either filter them (default) so that
they are removed from the analysis or assign them to a random patient
that is enrolled at the site at the time of the event.

However, we will only assign events that occurr between the first and
the last + 30 days denominator event on site.

``` r

# we create a Numerator dataframe with missing subjet id
dfNumerator <- clindata::ctms_protdev %>%
  rename(subjid = subjectenrollmentnumber) %>%
  left_join(
    clindata::rawplus_dm %>% 
    select(subjid, invid),
    by = "subjid"
  ) %>%
  filter(!is.na(deviationdate), !is.na(invid)) %>%
  # set 30% of subjectid per subject to NA
  arrange(runif(n())) %>%
  mutate(rnk = row_number() / n(), .by = subjid) %>%
  mutate(subjid = ifelse(rnk < 0.3, NA, subjid))

total_events <- nrow(dfNumerator)

total_events
#> [1] 1454

linked_events <- dfNumerator %>%
  filter(! is.na(subjid)) %>%
  nrow()

linked_events
#> [1] 1188

dfCumCount <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = dfNumerator,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt",
    strOrphanedMethod = "filter"
  ) %>%
  filter(Denominator == max(Denominator), .by = c(SubjectID))

filtered_events <- sum(dfCumCount$Numerator)

stopifnot(linked_events == filtered_events)

dfCumCountOrphans <- Input_CumCount(
    dfSubjects = clindata::rawplus_dm,
    dfNumerator = dfNumerator,
    dfDenominator = clindata::rawplus_visdt %>% mutate(visit_dt = lubridate::ymd(visit_dt)),
    strSubjectCol = "subjid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strNumeratorDateCol = "deviationdate",
    strDenominatorDateCol = "visit_dt",
    strOrphanedMethod = "assign"
  ) %>%
  filter(Denominator == max(Denominator), .by = c(SubjectID))

assigned_events <- sum(dfCumCountOrphans$Numerator)
  
assigned_events
#> [1] 1375

filtered_events / total_events
#> [1] 0.8170564

assigned_events / total_events
#> [1] 0.9456671
```

### Terminal Binary Events

Binary Events that can only occur once per patient and mark an early
drop out of the study can create a survivor bias when sampling
replacement patients.

It is recommended in this case to use planned visits as the denominator
instead of actual visits. However planned visits are not often
available, therefore
[`Input_CumCount()`](https://impala-consortium.github.io/gsm.simaerep/reference/Input_CumCount.md)
can attempt to estimate planned visits.

It will try to rank regular visit instances using the subject-level
dates and determine the median time that has passed between them.

For this it will only consider visits that do not match a specific SQL
like pattern and which are recorded for a minimum ratio of subjects.

Extrapolated visits that go beyond the last observed visit of patients
that did not discontinue will not be generated.

``` r

dfDenominator <- clindata::rawplus_visdt %>%
    mutate(visit_dt = lubridate::ymd(visit_dt))

dfNumerator <- clindata::rawplus_studcomp %>%
  mutate(mincreated_dts = lubridate::ymd_hms(mincreated_dts))

vLikePatternInstanceName <- c("%unsch%", "%disc%")

strInstanceNameCol <- "instancename"

dfInputExtra <- Input_CumCount(
  dfSubjects = clindata::rawplus_dm,
  dfNumerator = dfNumerator,
  dfDenominator = dfDenominator,
  strGroupCol = "invid",
  strSubjectCol = "subjid",
  strGroupLevel = "Site",
  strNumeratorDateCol = "mincreated_dts",
  strDenominatorDateCol = "visit_dt",
  vLikePatternInstanceName = vLikePatternInstanceName,
  strInstanceNameCol = strInstanceNameCol,
  nMinSubjectRatioInstance = 0.7
)

dfInputRegular <- Input_CumCount(
  dfSubjects = clindata::rawplus_dm,
  dfNumerator = dfNumerator,
  dfDenominator = dfDenominator,
  strGroupCol = "invid",
  strSubjectCol = "subjid",
  strGroupLevel = "Site",
  strNumeratorDateCol = "mincreated_dts",
  strDenominatorDateCol = "visit_dt"
)

dfInputRegular %>%
  filter(SubjectID == "0002") %>%
  knitr::kable()
```

| SubjectID | GroupID | GroupLevel | Numerator | Denominator |
|:----------|:--------|:-----------|----------:|------------:|
| 0002      | 0X104   | Site       |         0 |           1 |
| 0002      | 0X104   | Site       |         0 |           2 |
| 0002      | 0X104   | Site       |         0 |           3 |
| 0002      | 0X104   | Site       |         0 |           4 |
| 0002      | 0X104   | Site       |         1 |           5 |

``` r

dfInputExtra %>%
  filter(SubjectID == "0002") %>%
  knitr::kable()
```

| SubjectID | GroupID | GroupLevel | Numerator | Denominator |
|:----------|:--------|:-----------|----------:|------------:|
| 0002      | 0X104   | Site       |         0 |           1 |
| 0002      | 0X104   | Site       |         0 |           2 |
| 0002      | 0X104   | Site       |         0 |           3 |
| 0002      | 0X104   | Site       |         1 |           4 |
| 0002      | 0X104   | Site       |         1 |           5 |
| 0002      | 0X104   | Site       |         1 |           6 |
| 0002      | 0X104   | Site       |         1 |           7 |
| 0002      | 0X104   | Site       |         1 |           8 |
| 0002      | 0X104   | Site       |         1 |           9 |
| 0002      | 0X104   | Site       |         1 |          10 |
| 0002      | 0X104   | Site       |         1 |          11 |
| 0002      | 0X104   | Site       |         1 |          12 |
| 0002      | 0X104   | Site       |         1 |          13 |
| 0002      | 0X104   | Site       |         1 |          14 |
| 0002      | 0X104   | Site       |         1 |          15 |
| 0002      | 0X104   | Site       |         1 |          16 |
| 0002      | 0X104   | Site       |         1 |          17 |
| 0002      | 0X104   | Site       |         1 |          18 |
| 0002      | 0X104   | Site       |         1 |          19 |
| 0002      | 0X104   | Site       |         1 |          20 |
