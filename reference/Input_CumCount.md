# Cumulative Numerator Event Count per Denominator Event Count

**\[experimental\]**

Calculate a subject level cumulative numerator event count per
denominator event count.

This function takes in a list of data frames including dfSUBJ,
dfNumerator, and dfDenominator, and calculates a subject level
cumulative numerator count per cumulative denominator count. Numerator
events between two denominator events are assigned to the later
denominator event.

Numerator events before first denominator event will be assigned to the
first denominator event. Numerator events after last denominator event
will be assigned to last denominator event.

When numerator events fall on the same day as a denominator event they
will be assigned to that event event if the date time columns imply that
numerator event occurred after the denominator.

The data requirements for the function are as follows:

- dfSubjects: A data frame with columns for SubjectID and any other
  relevant subject information

- dfNumerator: A data frame with a column for SubjectID and
  `strNumeratorCol` if `strNumeratorMethod` is "Sum"

- dfDenominator: A data frame with a column for SubjectID and
  `strDenominatorCol` if `strDenominatorMethod` is "Sum"

All other columns are dropped from the output data frame. GroupIDs will
be added via dfSubjects if not present events without GroupID and
subjects without denominator or numerator event will be dropped.

Numerator events with no SubjectID but with GroupID can get assigned to
a random enrolled subject of the same GroupID

For terminal events such as discontinuations pass planned visits instead
of actual visits as dfDenominator to avoid survivor-bias.
strInstanceNameCol, vLikePatternInstanceName, and
nMinSubjectRatioInstance can be used to attempt to extrapolate planned
visits. See ExtrapolateDenominator() for details.

## Usage

``` r
Input_CumCount(
  dfSubjects,
  dfNumerator,
  dfDenominator,
  strGroupCol,
  strGroupLevel = NULL,
  strSubjectCol,
  strNumeratorCol = NULL,
  strDenominatorCol = NULL,
  strNumeratorDateCol,
  strDenominatorDateCol,
  strOrphanedMethod = c("filter", "assign"),
  strInstanceNameCol = NULL,
  vLikePatternInstanceName = c("%unsch%", "%disc%"),
  nMinSubjectRatioInstance = 0.7
)
```

## Arguments

- dfSubjects:

  `data.frame` with columns for SubjectID and any other relevant subject
  information

- dfNumerator:

  `data.frame` with a column for SubjectID and `strNumeratorDateCol`

- dfDenominator:

  `data.frame` with a column for SubjectID and `strDenominatorDateCol`

- strGroupCol:

  `character` Column name in `dfSubjects` to use for grouping.

- strGroupLevel:

  `character` value for the group level. Default: NULL defaults to
  `strGroupCol`

- strSubjectCol:

  `character` Column name in `dfSubjects` to use for subject ID.

- strNumeratorCol:

  `character` Column name in `dfNumerator` to use for numerator ID.
  Default: NULL

- strDenominatorCol:

  `character` Column name in `dfDenominator` to use for denominator ID.
  Default: NULL

- strNumeratorDateCol:

  `character` Column name in `dfNumerator` to use for numerator
  calculation.

- strDenominatorDateCol:

  `character` Column name in `dfDenominator` to use for denominator
  calculation.

- strOrphanedMethod:

  `character` one_of("filter", "assign") filter orphaned numerator
  events or assign to random patient enrolled at site the time of event.
  Default: "filter"

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

## Value

`data.frame` with the following specification:

|             |                              |           |
|-------------|------------------------------|-----------|
| Column Name | Description                  | Type      |
| SubjectID   | The subject ID               | Character |
| GroupID     | The group ID                 | Character |
| GroupLevel  | The group type               | Character |
| Numerator   | Cumulative Count Numerator   | Numeric   |
| Denominator | Cumulative Count Denominator | Numeric   |

## Examples

``` r
# dfSubjects tibble with one subject and one site
dfSubjects <- tibble::tibble(
  SubjectID = 1,
  GroupID = 1
)

dfNumerator <- tibble::tibble(
  SubjectID = rep(1, 10),
  num_dt = as.Date("2000-01-01") + c(months(0:4), rep(months(7), 2), months(9:11)),
) %>%
  dplyr::mutate(
    num_dt = num_dt + lubridate::hours(12)
  )

# dfNumerator tibble with one subject 10 AEs, two of which on same day
dfNumerator
#> # A tibble: 10 × 2
#>    SubjectID num_dt             
#>        <dbl> <dttm>             
#>  1         1 2000-01-01 12:00:00
#>  2         1 2000-02-01 12:00:00
#>  3         1 2000-03-01 12:00:00
#>  4         1 2000-04-01 12:00:00
#>  5         1 2000-05-01 12:00:00
#>  6         1 2000-08-01 12:00:00
#>  7         1 2000-08-01 12:00:00
#>  8         1 2000-10-01 12:00:00
#>  9         1 2000-11-01 12:00:00
#> 10         1 2000-12-01 12:00:00

dfDenominator <- tibble::tibble(
  SubjectID = rep(1, 4),
  denom_dt = c(as.Date(c("2000-01-03", "2000-04-12", "2000-08-01", "2000-11-12")))
) %>%
  dplyr::mutate(
    denom_dt = denom_dt + lubridate::hours(1)
  )

# dfDenominator tibble with one subject 4 visits, one on same day as two Numerator events
# Denominator time indicates that they occur before Numerator events
dfDenominator
#> # A tibble: 4 × 2
#>   SubjectID denom_dt           
#>       <dbl> <dttm>             
#> 1         1 2000-01-03 01:00:00
#> 2         1 2000-04-12 01:00:00
#> 3         1 2000-08-01 01:00:00
#> 4         1 2000-11-12 01:00:00


# numerator before first denominator rolls up to first denominator
# numerator after last denominator rolls back to last denominator
# two numerator events on 08-01 add to the 3rd denominator event on same day

Input_CumCount(
  dfSubjects = dfSubjects,
  dfNumerator = dfNumerator,
  dfDenominator = dfDenominator,
  strSubjectCol = "SubjectID",
  strGroupCol = "GroupID",
  strGroupLevel = "Site",
  strNumeratorDateCol = "num_dt",
  strDenominatorDateCol = "denom_dt"
)
#> # A tibble: 4 × 5
#>   SubjectID GroupID GroupLevel Numerator Denominator
#>       <dbl>   <dbl> <chr>          <dbl>       <dbl>
#> 1         1       1 Site               1           1
#> 2         1       1 Site               4           2
#> 3         1       1 Site               7           3
#> 4         1       1 Site              10           4



# {clindata} Example for cumulative AE per Visit Count
Input_CumCount(
  dfSubjects = clindata::rawplus_dm,
  dfNumerator = clindata::rawplus_ae,
  dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
  strSubjectCol = "subjid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strNumeratorDateCol = "aest_dt",
  strDenominatorDateCol = "visit_dt"
)
#> # A tibble: 28,074 × 5
#>    SubjectID GroupID GroupLevel Numerator Denominator
#>    <chr>     <chr>   <chr>          <dbl>       <dbl>
#>  1 0358      0X001   Site               0           1
#>  2 0358      0X001   Site               0           2
#>  3 0358      0X001   Site               0           3
#>  4 0358      0X001   Site               0           4
#>  5 0358      0X001   Site               0           5
#>  6 0358      0X001   Site               1           6
#>  7 0358      0X001   Site               1           7
#>  8 0358      0X001   Site               1           8
#>  9 0358      0X001   Site               1           9
#> 10 0358      0X001   Site               1          10
#> # ℹ 28,064 more rows
```
