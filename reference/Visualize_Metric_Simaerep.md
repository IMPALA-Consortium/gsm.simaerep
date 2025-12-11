# Visualize_Metric Function

**\[stable\]**

The function creates all available charts for a metric using the data
provided

## Usage

``` r
Visualize_Metric_Simaerep(
  dfResults,
  dfInput,
  dfMetrics = NULL,
  dfGroups = NULL,
  strMetricID = NULL,
  strSnapshotDate = NULL,
  bDebug = FALSE,
  vColors = c(`0` = "#9ED782", `1` = "#FEAA01", `2` = "#FF5858", `-1` = "#FEAA01", `-2` =
    "#FF5858", `NA` = "#a9a9a9"),
  ...
)
```

## Arguments

- dfResults:

  \`data.frame\` A stacked summary of analysis pipeline output. Created
  by passing a list of results returned by \[Summarize()\] to
  \[BindResults()\]. Expected columns: \`GroupID\`, \`GroupLevel\`,
  \`Numerator\`, \`Denominator\`, \`Metric\`, \`Score\`, \`Flag\`,
  \`MetricID\`, \`StudyID\`, \`SnapshotDate\`.

- dfInput:

  data.frame created by
  [`Input_CumCount()`](https://impala-consortium.github.io/gsm.simaerep/reference/Input_CumCount.md)

- dfMetrics:

  \`data.frame\` Metric-specific metadata for use in charts and
  reporting. Created by passing an \`lWorkflow\` object to
  \[MakeMetric()\]. Expected columns: \`File\`, \`MetricID\`, \`Group\`,
  \`Abbreviation\`, \`Metric\`, \`Numerator\`, \`Denominator\`,
  \`Model\`, \`Score\`, and \`Threshold\`. For more details see the Data
  Model vignette: \`vignette("DataModel", package = "gsm.core")\`.

- dfGroups:

  \`data.frame\` Group-level metadata dictionary. Created by passing
  CTMS site and study data to \[MakeLongMeta()\]. Expected columns:
  \`GroupID\`, \`GroupLevel\`, \`Param\`, \`Value\`.

- strMetricID:

  \`character\` MetricID to subset the data.

- strSnapshotDate:

  \`character\` Snapshot date to subset the data.

- bDebug:

  \`logical\` Display console in html viewer for debugging. Default is
  \`FALSE\`.

- vColors:

  vector, named hex values for every Flag value in dfFlagged\$Flag,
  Default NULL

- ...:

  Additional chart configuration settings.

## Value

A list containing the following charts:

- simaerep: A simaerep plot using JavaScript.

- scatterPlot: A scatter plot using JavaScript.

- barChart: A bar chart using JavaScript with metric on the y-axis.

- timeSeries: A time series chart using JavaScript with score on the
  y-axis.

- metricTable: A table containing all

## Examples

``` r
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
#> ℹ Sorted dfFlagged using custom Flag order: 2.Sorted dfFlagged using custom Flag order: -2.Sorted dfFlagged using custom Flag order: 1.Sorted dfFlagged using custom Flag order: -1.Sorted dfFlagged using custom Flag order: 0.

Visualize_Metric_Simaerep(
  dfResults = dfFlagged,
  dfInput = dfInput
)
#> ℹ Only one snapshot found. Time series charts will not be generated.
#> $simaerepChart
#> 
#> $scatterPlot
#> 
#> $barChart
#> 
#> $metricTable
#> <div id="scfhrfukag" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
#>   <style>#scfhrfukag table {
#>   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
#>   -webkit-font-smoothing: antialiased;
#>   -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> #scfhrfukag thead, #scfhrfukag tbody, #scfhrfukag tfoot, #scfhrfukag tr, #scfhrfukag td, #scfhrfukag th {
#>   border-style: none;
#> }
#> 
#> #scfhrfukag p {
#>   margin: 0;
#>   padding: 0;
#> }
#> 
#> #scfhrfukag .gt_table {
#>   display: table;
#>   border-collapse: collapse;
#>   line-height: normal;
#>   margin-left: auto;
#>   margin-right: auto;
#>   color: #333333;
#>   font-size: 16px;
#>   font-weight: normal;
#>   font-style: normal;
#>   background-color: #FFFFFF;
#>   width: auto;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #A8A8A8;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #A8A8A8;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_caption {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#> }
#> 
#> #scfhrfukag .gt_title {
#>   color: #333333;
#>   font-size: 125%;
#>   font-weight: initial;
#>   padding-top: 2px;
#>   padding-bottom: 2px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-color: #FFFFFF;
#>   border-bottom-width: 0;
#> }
#> 
#> #scfhrfukag .gt_subtitle {
#>   color: #333333;
#>   font-size: 85%;
#>   font-weight: initial;
#>   padding-top: 1px;
#>   padding-bottom: 3px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-color: #FFFFFF;
#>   border-top-width: 0;
#> }
#> 
#> #scfhrfukag .gt_heading {
#>   background-color: #FFFFFF;
#>   text-align: center;
#>   border-bottom-color: #FFFFFF;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_bottom_border {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_col_headings {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_col_heading {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 2.5px;
#>   padding-bottom: 3.5px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   overflow-x: hidden;
#> }
#> 
#> #scfhrfukag .gt_column_spanner_outer {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: normal;
#>   text-transform: inherit;
#>   padding-top: 0;
#>   padding-bottom: 0;
#>   padding-left: 4px;
#>   padding-right: 4px;
#> }
#> 
#> #scfhrfukag .gt_column_spanner_outer:first-child {
#>   padding-left: 0;
#> }
#> 
#> #scfhrfukag .gt_column_spanner_outer:last-child {
#>   padding-right: 0;
#> }
#> 
#> #scfhrfukag .gt_column_spanner {
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: bottom;
#>   padding-top: 2.5px;
#>   padding-bottom: 2.5px;
#>   overflow-x: hidden;
#>   display: inline-block;
#>   width: 100%;
#> }
#> 
#> #scfhrfukag .gt_spanner_row {
#>   border-bottom-style: hidden;
#> }
#> 
#> #scfhrfukag .gt_group_heading {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   text-align: left;
#> }
#> 
#> #scfhrfukag .gt_empty_group_heading {
#>   padding: 0.5px;
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   vertical-align: middle;
#> }
#> 
#> #scfhrfukag .gt_from_md > :first-child {
#>   margin-top: 0;
#> }
#> 
#> #scfhrfukag .gt_from_md > :last-child {
#>   margin-bottom: 0;
#> }
#> 
#> #scfhrfukag .gt_row {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   margin: 10px;
#>   border-top-style: solid;
#>   border-top-width: 1px;
#>   border-top-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 1px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 1px;
#>   border-right-color: #D3D3D3;
#>   vertical-align: middle;
#>   overflow-x: hidden;
#> }
#> 
#> #scfhrfukag .gt_stub {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #scfhrfukag .gt_stub_row_group {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   font-size: 100%;
#>   font-weight: initial;
#>   text-transform: inherit;
#>   border-right-style: solid;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   vertical-align: top;
#> }
#> 
#> #scfhrfukag .gt_row_group_first td {
#>   border-top-width: 2px;
#> }
#> 
#> #scfhrfukag .gt_row_group_first th {
#>   border-top-width: 2px;
#> }
#> 
#> #scfhrfukag .gt_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #scfhrfukag .gt_first_summary_row {
#>   border-top-style: solid;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_first_summary_row.thick {
#>   border-top-width: 2px;
#> }
#> 
#> #scfhrfukag .gt_last_summary_row {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_grand_summary_row {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   text-transform: inherit;
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #scfhrfukag .gt_first_grand_summary_row {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-top-style: double;
#>   border-top-width: 6px;
#>   border-top-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_last_grand_summary_row_top {
#>   padding-top: 4px;
#>   padding-bottom: 4px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#>   border-bottom-style: double;
#>   border-bottom-width: 6px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_striped {
#>   background-color: rgba(128, 128, 128, 0.05);
#> }
#> 
#> #scfhrfukag .gt_table_body {
#>   border-top-style: solid;
#>   border-top-width: 2px;
#>   border-top-color: #D3D3D3;
#>   border-bottom-style: solid;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_footnotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_footnote {
#>   margin: 0px;
#>   font-size: 90%;
#>   padding-top: 2px;
#>   padding-bottom: 2px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #scfhrfukag .gt_sourcenotes {
#>   color: #333333;
#>   background-color: #FFFFFF;
#>   border-bottom-style: none;
#>   border-bottom-width: 2px;
#>   border-bottom-color: #D3D3D3;
#>   border-left-style: none;
#>   border-left-width: 2px;
#>   border-left-color: #D3D3D3;
#>   border-right-style: none;
#>   border-right-width: 2px;
#>   border-right-color: #D3D3D3;
#> }
#> 
#> #scfhrfukag .gt_sourcenote {
#>   font-size: 90%;
#>   padding-top: 2px;
#>   padding-bottom: 2px;
#>   padding-left: 5px;
#>   padding-right: 5px;
#> }
#> 
#> #scfhrfukag .gt_left {
#>   text-align: left;
#> }
#> 
#> #scfhrfukag .gt_center {
#>   text-align: center;
#> }
#> 
#> #scfhrfukag .gt_right {
#>   text-align: right;
#>   font-variant-numeric: tabular-nums;
#> }
#> 
#> #scfhrfukag .gt_font_normal {
#>   font-weight: normal;
#> }
#> 
#> #scfhrfukag .gt_font_bold {
#>   font-weight: bold;
#> }
#> 
#> #scfhrfukag .gt_font_italic {
#>   font-style: italic;
#> }
#> 
#> #scfhrfukag .gt_super {
#>   font-size: 65%;
#> }
#> 
#> #scfhrfukag .gt_footnote_marks {
#>   font-size: 75%;
#>   vertical-align: 0.4em;
#>   position: initial;
#> }
#> 
#> #scfhrfukag .gt_asterisk {
#>   font-size: 100%;
#>   vertical-align: 0;
#> }
#> 
#> #scfhrfukag .gt_indent_1 {
#>   text-indent: 5px;
#> }
#> 
#> #scfhrfukag .gt_indent_2 {
#>   text-indent: 10px;
#> }
#> 
#> #scfhrfukag .gt_indent_3 {
#>   text-indent: 15px;
#> }
#> 
#> #scfhrfukag .gt_indent_4 {
#>   text-indent: 20px;
#> }
#> 
#> #scfhrfukag .gt_indent_5 {
#>   text-indent: 25px;
#> }
#> 
#> #scfhrfukag .katex-display {
#>   display: inline-flex !important;
#>   margin-bottom: 0.75em !important;
#> }
#> 
#> #scfhrfukag div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
#>   height: 0px !important;
#> }
#> </style>
#>   <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
#>   <thead>
#>     <tr class="gt_col_headings">
#>       <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Group">Group</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Numerator">Numerator</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Denominator">Denominator</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Metric">Metric</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Score">Score</th>
#>       <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Flag">Flag</th>
#>     </tr>
#>   </thead>
#>   <tbody class="gt_table_body">
#>     <tr><td headers="Group" class="gt_row gt_left">0X024</td>
#> <td headers="Numerator" class="gt_row gt_right">74</td>
#> <td headers="Denominator" class="gt_row gt_right">140</td>
#> <td headers="Metric" class="gt_row gt_right">0.53</td>
#> <td headers="Score" class="gt_row gt_right">1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X027</td>
#> <td headers="Numerator" class="gt_row gt_right">93</td>
#> <td headers="Denominator" class="gt_row gt_right">210</td>
#> <td headers="Metric" class="gt_row gt_right">0.44</td>
#> <td headers="Score" class="gt_row gt_right">1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X159</td>
#> <td headers="Numerator" class="gt_row gt_right">397</td>
#> <td headers="Denominator" class="gt_row gt_right">695</td>
#> <td headers="Metric" class="gt_row gt_right">0.57</td>
#> <td headers="Score" class="gt_row gt_right">1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X175</td>
#> <td headers="Numerator" class="gt_row gt_right">129</td>
#> <td headers="Denominator" class="gt_row gt_right">366</td>
#> <td headers="Metric" class="gt_row gt_right">0.35</td>
#> <td headers="Score" class="gt_row gt_right">1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X003</td>
#> <td headers="Numerator" class="gt_row gt_right">4</td>
#> <td headers="Denominator" class="gt_row gt_right">276</td>
#> <td headers="Metric" class="gt_row gt_right">0.01</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X059</td>
#> <td headers="Numerator" class="gt_row gt_right">14</td>
#> <td headers="Denominator" class="gt_row gt_right">312</td>
#> <td headers="Metric" class="gt_row gt_right">0.04</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X080</td>
#> <td headers="Numerator" class="gt_row gt_right">18</td>
#> <td headers="Denominator" class="gt_row gt_right">390</td>
#> <td headers="Metric" class="gt_row gt_right">0.05</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X124</td>
#> <td headers="Numerator" class="gt_row gt_right">35</td>
#> <td headers="Denominator" class="gt_row gt_right">596</td>
#> <td headers="Metric" class="gt_row gt_right">0.06</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X126</td>
#> <td headers="Numerator" class="gt_row gt_right">8</td>
#> <td headers="Denominator" class="gt_row gt_right">390</td>
#> <td headers="Metric" class="gt_row gt_right">0.02</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X153</td>
#> <td headers="Numerator" class="gt_row gt_right">25</td>
#> <td headers="Denominator" class="gt_row gt_right">597</td>
#> <td headers="Metric" class="gt_row gt_right">0.04</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X161</td>
#> <td headers="Numerator" class="gt_row gt_right">143</td>
#> <td headers="Denominator" class="gt_row gt_right">1442</td>
#> <td headers="Metric" class="gt_row gt_right">0.10</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X043</td>
#> <td headers="Numerator" class="gt_row gt_right">0</td>
#> <td headers="Denominator" class="gt_row gt_right">101</td>
#> <td headers="Metric" class="gt_row gt_right">0.00</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X106</td>
#> <td headers="Numerator" class="gt_row gt_right">1</td>
#> <td headers="Denominator" class="gt_row gt_right">107</td>
#> <td headers="Metric" class="gt_row gt_right">0.01</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X163</td>
#> <td headers="Numerator" class="gt_row gt_right">42</td>
#> <td headers="Denominator" class="gt_row gt_right">601</td>
#> <td headers="Metric" class="gt_row gt_right">0.07</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X180</td>
#> <td headers="Numerator" class="gt_row gt_right">12</td>
#> <td headers="Denominator" class="gt_row gt_right">261</td>
#> <td headers="Metric" class="gt_row gt_right">0.05</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X026</td>
#> <td headers="Numerator" class="gt_row gt_right">18</td>
#> <td headers="Denominator" class="gt_row gt_right">314</td>
#> <td headers="Metric" class="gt_row gt_right">0.06</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X122</td>
#> <td headers="Numerator" class="gt_row gt_right">23</td>
#> <td headers="Denominator" class="gt_row gt_right">352</td>
#> <td headers="Metric" class="gt_row gt_right">0.07</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X125</td>
#> <td headers="Numerator" class="gt_row gt_right">11</td>
#> <td headers="Denominator" class="gt_row gt_right">230</td>
#> <td headers="Metric" class="gt_row gt_right">0.05</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X157</td>
#> <td headers="Numerator" class="gt_row gt_right">6</td>
#> <td headers="Denominator" class="gt_row gt_right">188</td>
#> <td headers="Metric" class="gt_row gt_right">0.03</td>
#> <td headers="Score" class="gt_row gt_right">-1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X169</td>
#> <td headers="Numerator" class="gt_row gt_right">42</td>
#> <td headers="Denominator" class="gt_row gt_right">75</td>
#> <td headers="Metric" class="gt_row gt_right">0.56</td>
#> <td headers="Score" class="gt_row gt_right">1.00</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X041</td>
#> <td headers="Numerator" class="gt_row gt_right">37</td>
#> <td headers="Denominator" class="gt_row gt_right">66</td>
#> <td headers="Metric" class="gt_row gt_right">0.56</td>
#> <td headers="Score" class="gt_row gt_right">0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X054</td>
#> <td headers="Numerator" class="gt_row gt_right">33</td>
#> <td headers="Denominator" class="gt_row gt_right">47</td>
#> <td headers="Metric" class="gt_row gt_right">0.70</td>
#> <td headers="Score" class="gt_row gt_right">0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X090</td>
#> <td headers="Numerator" class="gt_row gt_right">17</td>
#> <td headers="Denominator" class="gt_row gt_right">16</td>
#> <td headers="Metric" class="gt_row gt_right">1.06</td>
#> <td headers="Score" class="gt_row gt_right">0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X164</td>
#> <td headers="Numerator" class="gt_row gt_right">0</td>
#> <td headers="Denominator" class="gt_row gt_right">80</td>
#> <td headers="Metric" class="gt_row gt_right">0.00</td>
#> <td headers="Score" class="gt_row gt_right">-0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X160</td>
#> <td headers="Numerator" class="gt_row gt_right">0</td>
#> <td headers="Denominator" class="gt_row gt_right">65</td>
#> <td headers="Metric" class="gt_row gt_right">0.00</td>
#> <td headers="Score" class="gt_row gt_right">-0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X014</td>
#> <td headers="Numerator" class="gt_row gt_right">150</td>
#> <td headers="Denominator" class="gt_row gt_right">533</td>
#> <td headers="Metric" class="gt_row gt_right">0.28</td>
#> <td headers="Score" class="gt_row gt_right">0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 41.4c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 109.3 361.4 246.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3l-160-160zm160 352l-160-160c-12.5-12.5-32.8-12.5-45.3 0l-160 160c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0L224 301.3 361.4 438.6c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X016</td>
#> <td headers="Numerator" class="gt_row gt_right">7</td>
#> <td headers="Denominator" class="gt_row gt_right">161</td>
#> <td headers="Metric" class="gt_row gt_right">0.04</td>
#> <td headers="Score" class="gt_row gt_right">-0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FF5859;overflow:visible;position:relative;"><path d="M246.6 470.6c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 402.7 361.4 265.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3l-160 160zm160-352l-160 160c-12.5 12.5-32.8 12.5-45.3 0l-160-160c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0L224 210.7 361.4 73.4c12.5-12.5 32.8-12.5 45.3 0s12.5 32.8 0 45.3z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X178</td>
#> <td headers="Numerator" class="gt_row gt_right">12</td>
#> <td headers="Denominator" class="gt_row gt_right">221</td>
#> <td headers="Metric" class="gt_row gt_right">0.05</td>
#> <td headers="Score" class="gt_row gt_right">-0.99</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X063</td>
#> <td headers="Numerator" class="gt_row gt_right">56</td>
#> <td headers="Denominator" class="gt_row gt_right">155</td>
#> <td headers="Metric" class="gt_row gt_right">0.36</td>
#> <td headers="Score" class="gt_row gt_right">0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X129</td>
#> <td headers="Numerator" class="gt_row gt_right">18</td>
#> <td headers="Denominator" class="gt_row gt_right">240</td>
#> <td headers="Metric" class="gt_row gt_right">0.07</td>
#> <td headers="Score" class="gt_row gt_right">-0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X188</td>
#> <td headers="Numerator" class="gt_row gt_right">58</td>
#> <td headers="Denominator" class="gt_row gt_right">158</td>
#> <td headers="Metric" class="gt_row gt_right">0.37</td>
#> <td headers="Score" class="gt_row gt_right">0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X119</td>
#> <td headers="Numerator" class="gt_row gt_right">4</td>
#> <td headers="Denominator" class="gt_row gt_right">107</td>
#> <td headers="Metric" class="gt_row gt_right">0.04</td>
#> <td headers="Score" class="gt_row gt_right">-0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X006</td>
#> <td headers="Numerator" class="gt_row gt_right">34</td>
#> <td headers="Denominator" class="gt_row gt_right">82</td>
#> <td headers="Metric" class="gt_row gt_right">0.41</td>
#> <td headers="Score" class="gt_row gt_right">0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X035</td>
#> <td headers="Numerator" class="gt_row gt_right">130</td>
#> <td headers="Denominator" class="gt_row gt_right">470</td>
#> <td headers="Metric" class="gt_row gt_right">0.28</td>
#> <td headers="Score" class="gt_row gt_right">0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X154</td>
#> <td headers="Numerator" class="gt_row gt_right">138</td>
#> <td headers="Denominator" class="gt_row gt_right">544</td>
#> <td headers="Metric" class="gt_row gt_right">0.25</td>
#> <td headers="Score" class="gt_row gt_right">0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X100</td>
#> <td headers="Numerator" class="gt_row gt_right">11</td>
#> <td headers="Denominator" class="gt_row gt_right">169</td>
#> <td headers="Metric" class="gt_row gt_right">0.07</td>
#> <td headers="Score" class="gt_row gt_right">-0.98</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">X187X</td>
#> <td headers="Numerator" class="gt_row gt_right">9</td>
#> <td headers="Denominator" class="gt_row gt_right">165</td>
#> <td headers="Metric" class="gt_row gt_right">0.05</td>
#> <td headers="Score" class="gt_row gt_right">-0.97</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X020</td>
#> <td headers="Numerator" class="gt_row gt_right">6</td>
#> <td headers="Denominator" class="gt_row gt_right">114</td>
#> <td headers="Metric" class="gt_row gt_right">0.05</td>
#> <td headers="Score" class="gt_row gt_right">-0.97</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X116</td>
#> <td headers="Numerator" class="gt_row gt_right">8</td>
#> <td headers="Denominator" class="gt_row gt_right">143</td>
#> <td headers="Metric" class="gt_row gt_right">0.06</td>
#> <td headers="Score" class="gt_row gt_right">-0.96</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X132</td>
#> <td headers="Numerator" class="gt_row gt_right">38</td>
#> <td headers="Denominator" class="gt_row gt_right">105</td>
#> <td headers="Metric" class="gt_row gt_right">0.36</td>
#> <td headers="Score" class="gt_row gt_right">0.96</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X069</td>
#> <td headers="Numerator" class="gt_row gt_right">4</td>
#> <td headers="Denominator" class="gt_row gt_right">96</td>
#> <td headers="Metric" class="gt_row gt_right">0.04</td>
#> <td headers="Score" class="gt_row gt_right">-0.96</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X088</td>
#> <td headers="Numerator" class="gt_row gt_right">19</td>
#> <td headers="Denominator" class="gt_row gt_right">232</td>
#> <td headers="Metric" class="gt_row gt_right">0.08</td>
#> <td headers="Score" class="gt_row gt_right">-0.96</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X057</td>
#> <td headers="Numerator" class="gt_row gt_right">43</td>
#> <td headers="Denominator" class="gt_row gt_right">130</td>
#> <td headers="Metric" class="gt_row gt_right">0.33</td>
#> <td headers="Score" class="gt_row gt_right">0.95</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 137.4c12.5-12.5 32.8-12.5 45.3 0l160 160c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L224 205.3 86.6 342.6c-12.5 12.5-32.8 12.5-45.3 0s-12.5-32.8 0-45.3l160-160z"/></svg></td></tr>
#>     <tr><td headers="Group" class="gt_row gt_left">0X013</td>
#> <td headers="Numerator" class="gt_row gt_right">8</td>
#> <td headers="Denominator" class="gt_row gt_right">136</td>
#> <td headers="Metric" class="gt_row gt_right">0.06</td>
#> <td headers="Score" class="gt_row gt_right">-0.95</td>
#> <td headers="Flag" class="gt_row gt_center"><svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:#FEAA02;overflow:visible;position:relative;"><path d="M201.4 374.6c12.5 12.5 32.8 12.5 45.3 0l160-160c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L224 306.7 86.6 169.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l160 160z"/></svg></td></tr>
#>   </tbody>
#>   
#> </table>
#> </div>
#> 
```
