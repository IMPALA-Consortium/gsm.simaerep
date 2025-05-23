# test Flag_Simaerep result compatible with gsm.core::Summarize

    Code
      dfSummarize <- gsm.core::Summarize(dfFlagged, nMinDenominator = 1)
    Message
      i 0 Site(s) have insufficient sample size due to KRI denominator less than 1. 
      These site(s) will not have KRI score and flag summarized.

