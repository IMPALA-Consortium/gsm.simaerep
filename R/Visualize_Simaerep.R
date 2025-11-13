


#' Visualize Simaerep
#' @description create a ggplot2 visualisation for a simaerep KRI
#' @dfInput data.frame created by [Input_CumCount()]
#' @dfFlagged data.frame created by [Flag_Simaerep()]
#' @strStudyID character, study label, Default: "StudyID"
#' @strScoreCol character, name of score column in dfFlagged, Default: "Score"
#' @nSiteMax integer, maximum of flagged sites to plot, Default: 16
#' @vColors vector, named hex values for every Flag value in dfFlagged$Flag, Default NULL
#' @export
#' @examples 
#'  dfInput <- Input_CumCount(
#'    dfSubjects = clindata::rawplus_dm,
#'    dfNumerator = clindata::rawplus_ae,
#'    dfDenominator = clindata::rawplus_visdt %>% dplyr::mutate(visit_dt = lubridate::ymd(visit_dt)),
#'    strSubjectCol = "subjid",
#'    strGroupCol = "siteid",
#'    strGroupLevel = "Site",
#'    strNumeratorDateCol = "aest_dt",
#'    strDenominatorDateCol = "visit_dt"
#'  )
#' 
#'  dfAnalyzed <- Analyze_Simaerep(dfInput)
#'  dfFlagged <- Flag_Simaerep(dfAnalyzed, vThreshold = c(-0.99, -0.95, 0.95, 0.99))
#' 
#'  Visualize_Simaerep(dfInput, dfFlagged)
Visualize_Simaerep <- function(dfInput,
                               dfFlagged,
                               strStudyId = "StudyID",
                               strScoreCol = "Score",
                               nSiteMax = 16,
                               vColors = NULL
                               ) {

    lsPrepData <- prepare_visualization_data(
      dfInput = dfInput,
      dfFlagged = dfFlagged,
      strScoreCol = strScoreCol,
      nSiteMax = nSiteMax,
      vColors = vColors
    )

    args <- c(
      lsPrepData,
      list(
        strStudyId = strStudyId,
        strScoreCol = strScoreCol
      )
    )

    p <- do.call(plot_simaerep, args)

    return(p)
  
}

#' @keywords internal
prepare_visualization_data <- function(dfInput,
                                       dfFlagged,
                                       strScoreCol,
                                       nSiteMax,
                                       vColors
                                       ) {

    # Handle Colors -----------------------------------------------------
    if (is.null(vColors)) {
      vColors <- scales::brewer_pal(type = "seq", "Blues")(n_distinct(abs(dfFlagged$Flag)))
      names(vColors) <- sort(unique(abs(dfFlagged$Flag)))

    dfFlagged <- dfFlagged %>%
      mutate(
        Color = vColors[abs(.data$Flag) + 1]
      )
      
    } else {
      stopifnot(
        "Provide name vColor for every value in Flag" = all(unique(dfFlagged$Flag) %in% names(vColors))
      )
      dfFlagged <- dfFlagged %>%
        mutate(
          Color = vColors[as.character(.data$Flag)]
        )

    }



    # Get Mean Numerator Development ------------------------------------

    cols_rename <- c(
        "site_number" = "GroupID",
        "patnum" = "SubjectID",
        "n_event" = "Numerator",
        "visit" = "Denominator"
    )

    cols_rename_reverse <- setNames(names(cols_rename), cols_rename)

    df_mean_group <- dfInput %>%
        mutate(study_id = "A") %>%
        rename(all_of(cols_rename)) %>%
        simaerep::get_cum_mean_event_dev(
            group = "site_number",
            event_names = "event"
        ) %>%
        rename(any_of(cols_rename_reverse)) %>%
        select(- "study_id")

    df_mean_study <-dfInput %>%
        mutate(study_id = "A") %>%
        rename(all_of(cols_rename)) %>%
        simaerep::get_cum_mean_event_dev(
            group = "study_id",
            event_names = "event"
        ) %>%
        rename(any_of(cols_rename_reverse)) %>%
        select(- "study_id")


    # Determine Sites for Plotting ----------------------------------------

    df_flag_filt <- dfFlagged %>%
        filter(.data$Flag != 0) %>%
        arrange(desc(abs(.data[[strScoreCol]])), desc(abs(.data$ExpectedNumerator)))

    if (! is.null(nSiteMax)) {
      group_plot <- df_flag_filt %>%
        filter(row_number() <= .env$nSiteMax) %>%
        pull(.data$GroupID)
    } else {
      group_plot <- df_flag_filt$GroupID
    }

    df_visit <- dfInput %>%
        filter(.data$GroupID %in% group_plot)

    # Prepare Dataframes --------------------------------------------------

    df_mean_group <- df_mean_group %>%
      left_join(
        dfFlagged %>%
          select(all_of(c("GroupID", "Color"))),
        by = "GroupID"
      )

    df_mean_group_flagged <- df_mean_group %>%
        filter(.data$GroupID %in% df_flag_filt$GroupID)

    df_mean_group_not_flagged <- df_mean_group %>%
        filter(! .data$GroupID %in% df_flag_filt$GroupID)


    df_label_sites <- df_flag_filt %>%
        left_join(
          df_visit %>%
            summarise(
              nSubjects = n_distinct(.data$SubjectID),
              .by = "GroupID"
            ),
          by = "GroupID"
        )

    return(list(
      df_mean_study = df_mean_study,
      df_mean_group_flagged = df_mean_group_flagged,
      df_mean_group_not_flagged = df_mean_group_not_flagged,
      df_visit = df_visit,
      df_label_sites = df_label_sites
    ))
}

#' @keywords internal
plot_simaerep <- function(df_mean_study,
                        df_mean_group_flagged,
                        df_mean_group_not_flagged,
                        df_visit,
                        df_label_sites,
                        strStudyId,
                        strScoreCol) {


    # study plot -----------------------------------------------------------------


  p_study <- df_mean_group_not_flagged %>%
    ggplot(aes(Denominator, .data[["cum_mean_dev_event"]])) +
    geom_line(aes(
      group = .data$GroupID,
      color = .data$Color
    )) +
    geom_line(aes(
      group = .data$GroupID,
      color = .data$Color
    ),
    data = df_mean_group_flagged,
    linewidth = 1
    ) +
    geom_line(aes(group = 1),
              data = df_mean_study,
              color = "gold3",
              linewidth = 1,
              alpha = 0.5
    ) +
    scale_color_identity() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(y = paste0("Mean Cumulative Numerator Count per Site"))

  # site plot -------------------------------------------------------------------

  # we use ordered factors to ensure that sites are ordered by highest score and highest Expected Numerator

  # we filter sites that are not plotted but re flagged  
  df_label_sites <- df_label_sites %>%
    filter(.data$GroupID %in% unique(df_visit$GroupID))

  df_mean_group_flagged <- df_mean_group_flagged %>%
    filter(.data$GroupID %in% unique(df_visit$GroupID)) %>%
    mutate(GroupID = forcats::fct_relevel(.data$GroupID, df_label_sites$GroupID))

  max_denom <- max(df_mean_study$Denominator)
  max_num <- max(c(df_visit[["Numerator"]], df_mean_study[["cum_mean_dev_event"]]))

  df_visit <- df_visit %>%
    mutate(GroupID = forcats::fct_relevel(.data$GroupID, df_label_sites$GroupID))

  df_label_sites <- df_label_sites %>%
    mutate(
      label_subj = paste("N:", .data$nSubjects),
      label_score = paste(round(.data[[strScoreCol]], 3) * 100, "%"),
      label_delta = paste(round(.data$ExpectedNumerator, 0), "delta"),
      Flag = as.character(.data$Flag),
      GroupID = forcats::fct_relevel(.data$GroupID, df_label_sites$GroupID)
    ) %>%
    filter(.data$GroupID %in% unique(df_visit$GroupID))


  p_site <- df_visit %>%
    ggplot(aes(.data$Denominator, .data$Numerator)) +
    geom_line(aes(group = .data$SubjectID),
              color = "grey",
              alpha = 0.5
    ) +
    geom_line(aes(
      y = .data[["cum_mean_dev_event"]],
      group = .data$GroupID,
      color = .data$Color,
      alpha = 0.5
    ),
    data = df_mean_group_flagged,
    linewidth = 1
    ) +
    geom_line(aes(y = .data[["cum_mean_dev_event"]]),
              data = df_mean_study,
              color = "gold3",
              linewidth = 1,
              alpha = 0.5) +
    geom_text(aes(label = .data$label_subj),
              data = df_label_sites,
              x = 0.2 * max_denom,
              y = 0.9 * max_num,
              na.rm = TRUE) +
    geom_label(aes(label = .data$label_score,
                   color = .data$Color),
               data = df_label_sites,
               x = 0.8 * max_denom,
               y = 0.9 * max_num,
               na.rm = TRUE) +
    geom_label(aes(label = .data$label_delta,
                    color = .data$Color),
                data = df_label_sites,
                x = 0.8 * max_denom,
                y = 0.1 * max_num,
                na.rm = TRUE) +
    scale_color_identity() +
    facet_wrap(~ .data$GroupID) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(y = paste0("Mean Cumulative Numerator Count per Site"))

  # title -----------------------------------------------------

  make_title <- function(x) {
    cowplot::ggdraw() +
      cowplot::draw_label(
        x,
        fontface = "bold"
      )
  }

  t <- make_title(strStudyId)

  # compose plots ---------------------------------------------

  lwr <- cowplot::plot_grid(p_study, p_site, nrow = 1)

  gr <- cowplot::plot_grid(t, lwr, ncol = 1, rel_heights = c(0.05, 1))

  return(gr)

}
