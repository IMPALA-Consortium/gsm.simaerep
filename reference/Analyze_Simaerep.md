# Analyze Simaerep

**\[experimental\]**

Calculate over and under-reporting probabilities for numerator
denominator ratio using
[`simaerep::simaerep()`](https://openpharma.github.io/simaerep/reference/simaerep.html).

Uses inframe `simaerep` method and does not apply multiplicity
correction as per latest recommendations.

The sum of under- and over reporting probability per group is usually
zero, however
[`simaerep::simaerep()`](https://openpharma.github.io/simaerep/reference/simaerep.html)
adjusts edge cases in which group ratio and expected ratio are all zero.
We combine both metrics into a score between 0 and 1, where 0 is
under-reporting and 1 is over-reporting.

## Usage

``` r
Analyze_Simaerep(dfInput, r = 1000)
```

## Arguments

- dfInput:

  `data.frame` as returned by
  [`Input_CumCount()`](https://impala-consortium.github.io/gsm.simaerep/reference/Input_CumCount.md)

- r:

  Integer or tbl_object, number of repetitions for bootstrap simulation.
  Pass a tbl object referring to a table with one column and as many
  rows as desired repetitions. Default: 1000.

## Value

`data.frame` with the following specification:

|                   |                                              |           |
|-------------------|----------------------------------------------|-----------|
| Column Name       | Description                                  | Type      |
| GroupID           | The group ID                                 | Character |
| GroupLevel        | The group level                              | Character |
| Numerator         | Numerator events                             | Numeric   |
| Denominator       | Denominator events                           | Numeric   |
| Metric            | Ratio all subjects in GroupID                | Numeric   |
| Score             | Reporting Score                              | Numeric   |
| ScoreMult         | Reporting Score with Multiplicity Correction | Numeric   |
| ExpectedNumerator | Count of Expected Numerator Events           | Numeric   |

## See also

[`simaerep::simaerep()`](https://openpharma.github.io/simaerep/reference/simaerep.html),
[`Input_CumCount()`](https://impala-consortium.github.io/gsm.simaerep/reference/Input_CumCount.md)

## Examples

``` r
# {clindata} Example for cumulative AE per Visit Count
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

Analyze_Simaerep(dfInput)
#> # A tibble: 176 × 8
#>    GroupID GroupLevel Numerator Denominator Metric  Score ScoreMult
#>    <chr>   <chr>          <dbl>       <dbl>  <dbl>  <dbl>     <dbl>
#>  1 0X001   Site              14          63 0.222   0.73     0.117 
#>  2 0X002   Site               8          22 0.364   0.867    0.193 
#>  3 0X003   Site               4         276 0.0145 -1       -1     
#>  4 0X004   Site               8          61 0.131  -0.502   -0.0118
#>  5 0X005   Site              34         154 0.221   0.766    0.117 
#>  6 0X006   Site              34          82 0.415   0.985    0.78  
#>  7 0X007   Site               8          48 0.167   0.516    0     
#>  8 0X008   Site               8          21 0.381   0.881    0.252 
#>  9 0X010   Site              16          63 0.254   0.799    0.117 
#> 10 0X011   Site               3          75 0.04   -0.949   -0.701 
#> # ℹ 166 more rows
#> # ℹ 1 more variable: ExpectedNumerator <dbl>
```
