# Extrapolate Denominator Events

internal function called by
[`Input_CumCount()`](https://impala-consortium.github.io/gsm.simaerep/reference/Input_CumCount.md)
Filters all instances not matching vLikePatternInstanceName that occur
for a minimum ratio of subjects. Attempts to estimate the order of
instances from subject-level instance dates and to establish the median
time difference between instances. Missing instances for subjects with
entries in numerator data frame will be added using the median days
between instances to extrapolate dates. No instances will be added if
extrapolated date exceeds the maximum instance date for subjects without
numerator entries.

## Usage

``` r
ExtrapolateDenominator(
  dfDenominator,
  dfNumerator,
  strSubjectCol,
  strDenominatorDateCol,
  strInstanceNameCol,
  vLikePatternInstanceName = c("%unsch%", "%disc%"),
  nMinSubjectRatioInstance = 0.7
)
```

## Arguments

- dfDenominator:

  `data.frame` with a column for SubjectID and `strDenominatorDateCol`

- dfNumerator:

  `data.frame` with a column for SubjectID and `strNumeratorDateCol`

- strSubjectCol:

  `character` Column name in `dfSubjects` to use for subject ID.

- strDenominatorDateCol:

  `character` Column name in `dfDenominator` to use for denominator
  calculation.

- strInstanceNameCol:

  `character` When provided will extrapolate planned Numerator events
  e.g. visits. Use for terminal binary events such as patient
  discontinuations to avoid survival bias. Default: NULL

- vLikePatternInstanceName:

  `character vector` vector of sql like patterns to filter instance
  names for extrapolation. Default: c("%unsch%", "%disc%")

- nMinSubjectRatioInstance:

  numeric, minimum subject ratio per instance name to consider for
  extrapolation. Default: 0.7
