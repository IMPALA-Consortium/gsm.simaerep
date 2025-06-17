# test Flag_Simaerep result compatible with gsm.core::Summarize

    Code
      dfSummarize <- gsm.core::Summarize(dfFlagged, nMinDenominator = 1)
    Condition
      Warning:
      The `nMinDenominator` argument of `Summarize()` is deprecated as of gsm.core 1.0.0.
      i Please use the `nAccrualThreshold` and `strAccrualMetric` arguments in `Flag()` instead
    Message
      i 0 Group(s) have insufficient sample size due to KRI denominator less than 1. 
      These group(s) will not have KRI score and flag summarized.

