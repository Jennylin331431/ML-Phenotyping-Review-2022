# Function to get the validation metrics formatted for plotting.
#--------------------------------------------------------------#
# df: Dataframe.
# comparator: String for what method to compare to, either "rule"
#   or "traditional".
get_metrics <- function(df, comparator = "rule"){

  if (comparator == "rule") {

    df <- df %>% filter(Compare_with_rule_based != "")
    df <- unnest_validate_string(df, comparator = "not deep")

  } else if (comparator == "traditional") {

    df <- df %>% filter(Best_comparator_traditional != "")
    df <- unnest_validate_string(df, comparator = "deep")
  }



  df %>%
    unite("Study1", c(Phenotype, Data_source, PMID), sep = "\n", remove = FALSE) %>%
    unite("Study2", c(Phenotype, Data_source), sep = "\n", remove = FALSE)

}


# Function to determine best performing model for plotting.
#---------------------------------------------------------#
# df: Dataframe.
# baseline_method: String for column name of baseline method.
# comparison_method: String ofr column name of comparison method.
best_performing_model <- function(df, baseline_method,
                                  comparison_method,
                                  metrics = "ALL"){

  df <- df %>% mutate_all(na_if, "")

  if(metrics == "ALL"){

    df$baseline_sens_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Sensitivity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Sensitivity")))) * 1

    df$baseline_ppv_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_PPV"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_PPV")))) * 1

    df$baseline_spec_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Specificity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Specificity")))) * 1

    df$baseline_npv_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_NPV"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_NPV")))) * 1

    df$baseline_auc_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_AUROC"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_AUROC")))) * 1

    df$baseline_all_better <- apply(df %>% select(baseline_sens_better,
                                                  baseline_ppv_better,
                                                  baseline_spec_better,
                                                  baseline_npv_better,
                                                  baseline_auc_better),
                                    1, min,
                                    na.rm = TRUE)
  } else if (metrics == "deep"){

    df$baseline_sens_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Sensitivity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Sensitivity")))) * 1

    df$baseline_ppv_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_PPV"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_PPV")))) * 1

    df$baseline_spec_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_Specificity"))) >
                                    df %>% select(!!sym(paste0(comparison_method, "_Specificity")))) * 1

    df$baseline_auc_better <-  I(df %>% select(!!sym(paste0(baseline_method, "_AUROC"))) >
                                   df %>% select(!!sym(paste0(comparison_method, "_AUROC")))) * 1

    df$baseline_all_better <- apply(df %>% select(baseline_sens_better,
                                                  baseline_ppv_better,
                                                  baseline_spec_better,
                                                  baseline_auc_better),
                                    1, min,
                                    na.rm = TRUE)

    # Remove papers only reporting F-score.
    #df <- df %>% filter(baseline_all_better != Inf)
  }


  return(df)
}


# Function to plot validation metrics.
#------------------------------------#
# df: Dataframe.
# comparator: Comparator method.
# large_fig: Whether to produce large figure.
plot_validate_metrics <- function(df,
                                  comparator = "rule",
                                  large_fig = FALSE) {

  label_size <- if_else(large_fig, 15, 10)

  if (comparator == "rule") {

    # Get traditional ML models that compare to rule-based.
    df_formatted <- get_metrics(df, rule)

    df_ML_better <- best_performing_model(df_formatted %>%
      filter(Best_performing_model == "Traditional ML"),
      "Best_performing", "Best_comparator_Rule")


    df_rule_better <- best_performing_model(df_formatted %>%
                                            filter(Best_performing_model == "Rule-based"),
                                           "Best_comparator_traditional",
                                           "Best_performing")
    df_rule_better$Phenotype <- c("Obesity and other comorbidities",
                                  "Marital Status")

    g1 <- plot_rule_compare(df_ML_better %>% filter(baseline_all_better == 1),
                            study = "Phenotype",
                            large_fig = large_fig)

    g2 <- plot_rule_compare(df_ML_better %>% filter(baseline_all_better != 1,
                                                      baseline_sens_better == 1,
                                                    (baseline_ppv_better == 1 | is.na(baseline_ppv_better))),
                            large_fig = large_fig)


    g3 <- plot_rule_compare(df_ML_better %>% filter(baseline_all_better != 1 &
                                                      baseline_sens_better == 0,
                                                    (baseline_ppv_better == 1 | is.na(baseline_ppv_better))) ,
                            large_fig = large_fig)

    g4 <- plot_rule_compare(df_rule_better, large_fig = large_fig)

    plot_grid(g1, g2, g3, g4,
              nrow = 5,
              align = "v",
              labels = c('(a)', '(b)', '(c)', '(d)'),
              label_size = label_size,
              rel_heights = c(3, 2, 2, 3))

  } else if (comparator == "deep") {

    # Get traditional ML models that compare to rule-based.
    df_formatted <- get_metrics(deep_supervised, "traditional")

    df_ML_better_1 <- best_performing_model(df_formatted %>%
                                            filter(Best_performing_model == "DL"),
                                          "Best_performing",
                                          "Best_comparator_traditional",
                                          metrics = "deep")

    df_ML_better_2 <- best_performing_model(df_formatted %>%
                                            filter(Best_performing_model == "Rule-based"),
                                          "Best_comparator_traditional",
                                          "Best_comparator_DL",
                                          metrics = "deep")

    df_ML_better <- rbind(df_ML_better_1, df_ML_better_2)
    df_ML_better[df_ML_better$PMID == 32548622, "Phenotype"] = "Social determinants of health"
    df_ML_better[df_ML_better$PMID == 35007754, "Phenotype"] = "Acute care phenotypes"

    df_rule_better <- best_performing_model(df_formatted %>%
                                              filter(Best_performing_model == "Traditional ML"),
                                            "Best_comparator_traditional",
                                            "Best_performing",
                                            metrics = "deep")

    df_rule_better$Phenotype <- c("Chest injury", "Bleeding", "Fall")

    g1 <- plot_deep_compare(df_ML_better %>% filter(baseline_all_better == 1),
                            study = "Phenotype",
                            large_fig = large_fig)

    g2 <- plot_deep_compare(df_ML_better %>% filter(baseline_all_better != 1,
                                                    baseline_sens_better == 1,
                                                    (baseline_ppv_better == 0 | is.na(baseline_ppv_better))),
                            study = "Phenotype",
                            large_fig = large_fig)


    g3 <- plot_deep_compare(df_ML_better %>% filter(baseline_all_better != 1 &
                                                      baseline_sens_better == 0,
                                                    (baseline_ppv_better == 1 | is.na(baseline_ppv_better))),
                            study = "Phenotype",
                            large_fig = large_fig)

    g4 <- plot_deep_compare(df_rule_better, study = "Phenotype", large_fig = large_fig)

    plot_grid(g1, g2, g3, g4,
              nrow = 4,
              align = "v",
              labels = c('(a)', '(b)', '(c)', '(d)'),
              label_size = label_size,
              greedy = TRUE,
              rel_heights = c(15, 7, 6, 6))








     #df <- get_deep_metrics(df)
    df <- read.csv("../data/dl_performance.csv")

    # Arrange by sensitivity.
    df_sens <- ml_deep_metrics(df, "Sensitivity") %>%
      select(Phenotype_ref, diff, DL_better) %>%
      unique() %>%
      arrange(DL_better, -diff)

    # Set levels as the decreased sensitivity.
    df$Phenotype_ref <- factor(df$Phenotype_ref, levels = df_sens$Phenotype_ref)

    df_sens_better <- df %>% filter(Category == "Sens_better")
    df_ppv_better <- df %>% filter(Category == "PPV_better")
    df_ML_better <- df %>% filter(Category == "ML_better")
    df_DL_better <- df %>% filter(Category == "DL_better")

    g1 <- plot_deep_compare(df_DL_better, large_fig = large_fig)
    g2 <- plot_deep_compare(df_ML_better, large_fig = large_fig)
    g3 <- plot_deep_compare(df_sens_better, large_fig = large_fig)
    g4 <- plot_deep_compare(df_ppv_better, large_fig = large_fig, legend = TRUE)

    plot_grid(g1, g2, g3, g4,
              nrow = 4,
              align = "v",
              labels = c('(a)', '(b)', '(c)', '(d)'),
              label_size = label_size,
              greedy = TRUE,
              rel_heights = c(15, 3, 7, 6))

  } else if (comparator == "weakly") {

    df <- get_weakly_metrics(df, "rule")

    # Arrange by sensitivity.
    df_sens <- ml_rule_metrics(df, "Sensitivity") %>%
      dplyr::select(Phenotype, rule, diff, ML_better) %>% unique() %>% arrange(-abs(diff))

    df$Phenotype <- factor(df$Phenotype, levels = df_sens$Phenotype)

    comparable <- c("Metastatic breast cancer",
                    "Rheumatoid arthritis\n (PheProb)",
                    "Obesity\n (APHRODITE)",
                    "Fall\n (NimbleMiner)",
                    "Glaucoma\n (APHRODITE)",
                    "Epilepsy\n (APHRODITE)",
                    "Type 2 diabetes mellitus\n (APHRODITE)",
                    "Cataracts\n (APHRODITE)",
                    "Venous thromboembolism\n (APHRODITE)",
                    "Heart failure\n (APHRODITE)",
                    "Peripheral arterial disease\n (APHRODITE)")

    df_ML_better <- df %>% filter(!(Phenotype %in% comparable))
    df_comparable <- df %>% filter(Phenotype %in% comparable)

    g1 <- plot_rule_compare(df_ML_better, comparison = "weakly", large_fig = large_fig)
    g3 <- plot_rule_compare(df_comparable, comparison = "weakly", large_fig = large_fig, legend = TRUE)

    plot_grid(g1, g3,
              nrow = 2,
              align = "v",
              labels = c('(a)', '(b)'),
              rel_heights = c(5, 5))

  } else {

    #weakly v.s. traditional
    df <- get_weakly_metrics(df, "ml")

    # Arrange by sensitivity.
    df_auc <- weakly_rule_metrics(df, "AUROC") %>%
      dplyr::select(Phenotype, diff) %>% unique() %>% arrange(-(diff))

    df$Phenotype <- factor(df$Phenotype, levels = df_auc$Phenotype)

    plot_metrics(df,
                 comparison = comparator,
                 study = "Phenotype",
                 metric = "AUROC",
                 large_fig,
                 xlim = 0.5) +
      theme(legend.position = "bottom")

  }

}

# Figure to plot ML vs rule across all metrics.
plot_rule_compare <- function(df,
                              comparison = "rule",
                              study = "Phenotype",
                              large_fig = FALSE,
                              legend = FALSE) {

  # Remove y-axis label and text for graph other than sensitivity.
  theme_type <- theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())

  legend_pos = if_else(legend, "bottom", "none")

  p1 <- plot_metrics(df, comparison, study, metric = "Sensitivity", large_fig)
  p2 <- plot_metrics(df, comparison, study, metric = "Specificity", large_fig) + theme_type
  p3 <- plot_metrics(df, comparison, study, metric = "PPV", large_fig) +
    theme(legend.position = legend_pos) +
    theme_type
  p4 <- plot_metrics(df, comparison, study, metric = "NPV", large_fig) + theme_type
  p5 <- plot_metrics(df, comparison, study, metric = "AUROC", large_fig) + theme_type

  egg::ggarrange(p1,p2,p3,p4,p5, nrow = 1, draw = FALSE)

}

# Figure to plot ML vs DL across all metrics.
plot_deep_compare <- function(df,
                              comparison = "deep",
                              study = "Phenotype_ref",
                              large_fig = FALSE,
                              legend = FALSE) {

  # Remove y-axis label and text for graph other than sensitivity.
  theme_type <- theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())

  legend_pos = if_else(legend, "bottom", "none")

  p1 <- plot_metrics(df, comparison, study, "Sensitivity", large_fig = large_fig)
  p4 <- plot_metrics(df, comparison, study, "PPV", large_fig = large_fig) + theme_type
  p5 <- plot_metrics(df, comparison, study, "AUROC", large_fig = large_fig) + theme_type
  p2 <- plot_metrics(df, comparison, study, "Specificity", large_fig = large_fig) +
    theme(legend.position = legend_pos) +
    theme_type

  egg::ggarrange(p1, p2, p4, p5, nrow = 1, draw = FALSE)

}

plot_metrics <- function(df,
                         comparison = "rule",
                         study = "Phenotype",
                         metric = "Sensitivity",
                         large_fig = FALSE,
                         xlim = 0.01) {

  # Remove all the metrics are NAs.
  #df1 <- apply(df[, c(which(colnames(df) ==
  #"Best_performing_Sensitivity")):ncol(df)], 2, as.numeric)

  if (comparison == "rule") {

    df <- ml_rule_metrics(df, metric)

  } else if (comparison == "weakly") {

    df <- weakly_metrics(df, metric)

  } else if (comparison == "deep") {

    # Check missingness and fill the NA.
    if (typeof(df) == "double") {

      df1 <- t(data.frame(df))
      missing_index <- rowMeans(is.na(df1))

      if (sum(missing_index == 1) > 0) {

        df <- df[-which(missing_index == 1), ]

      }

    }

    df <- ml_deep_metrics(df, metric)

  } else {

    df <- weakly_rule_metrics(df, metric)

  }

  title_size <- if_else(large_fig, 18, 8)
  text_size <- if_else(large_fig, 10, 5)
  legend_size <- if_else(large_fig, 15, 7)
  point_size <- if_else(large_fig, 3, 2)
  vjust_size <- if_else(large_fig, 0.5, 0.25)

  df %>%
    ggplot(aes(x = !!sym(study),
               y = as.numeric(!!sym(metric)),
               color = Method)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 60)) +
    scale_y_continuous(limits = c(xlim, 1),
                       labels = function(y) label_parsed(paste0(y*100))) +
    scale_color_jama() +
    coord_flip() +
    labs(x = "", y = "", title = metric) +
    theme_bw() +
    geom_point(size = point_size) +
    theme(legend.position = "none",
          plot.title = element_text(size = title_size, hjust = 0.5, face="bold"),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = text_size, color = "black"),
          axis.text.y = element_text(size = legend_size, vjust = vjust_size),
          legend.title = element_blank(),
          legend.text = element_text(size = legend_size))

}

# Utility function to plot supervised ML vs rule for one metric.
ml_rule_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)
  metric_rule <- paste0("Best_comparator_Rule_", metric)

  df %>%
    mutate(`Traditional supervised learning` = case_when(
      str_detect(Best_performing_model, "Traditional ML") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_ml)))) %>%
    mutate(`Rule-based` = case_when(
      str_detect(Best_performing_model, "Rule-based") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_rule)))) %>%
    mutate(diff = `Traditional supervised learning` - `Rule-based`) %>%
    mutate(ML_better = diff > 0) %>%
    mutate(rule = `Rule-based`) %>%
    pivot_longer(cols = c(`Traditional supervised learning`, `Rule-based`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Study1,
                  Study2,
                  PMID,
                  Phenotype,
                  rule,
                  diff,
                  ML_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}

# Utility function to plot supervised ML vs rule for one metric.
weakly_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)
  metric_rule <- paste0("Best_comparator_Rule_", metric)

  df %>%
    filter(Best_performing_model != "") %>%
    mutate(`Weakly-supervised learning` = case_when(
      str_detect(Best_performing_model, "Traditional ML") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_ml)))) %>%
    mutate(`Rule-based` = case_when(str_detect(
      Best_performing_model, "Rule-based") ~ as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_rule)))) %>%
    mutate(diff = `Weakly-supervised learning` - `Rule-based`) %>%
    mutate(ML_better = diff > 0) %>%
    mutate(rule = `Rule-based`) %>%
    pivot_longer(cols = c(`Weakly-supervised learning`, `Rule-based`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Study1,
                  Study2,
                  PMID,
                  Phenotype,
                  rule,
                  diff,
                  ML_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}

# Utility function to plot supervised ML vs rule for one metric.
weakly_rule_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)

  df %>%
    mutate(`Weakly-supervised learning` = as.numeric(!!sym(metric_best))) %>%
    mutate(`Traditional supervised learning` = as.numeric(!!sym(metric_ml))) %>%
    mutate(diff = `Weakly-supervised learning`-`Traditional supervised learning`) %>%
    mutate(ML_better = diff > 0) %>%
    pivot_longer(cols = c(`Weakly-supervised learning`, `Traditional supervised learning`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Phenotype,
                  diff,
                  ML_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}

# Utility function to plot ML vs DL for one metric.
ml_deep_metrics <- function(df, metric = "Sensitivity") {

  metric_best <- paste0("Best_performing_", metric)
  metric_ml <- paste0("Best_comparator_traditional_", metric)
  metric_deep <- paste0("Best_comparator_DL_", metric)

  df %>%
    filter(Best_performing_model != "") %>%
    mutate(`Deep supervised learning` = case_when(
      str_detect(Best_performing_model, "DL") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_deep)))) %>%
    mutate(`Traditional supervised learning` = case_when(
      str_detect(Best_performing_model, "Traditional ML") ~
        as.numeric(!!sym(metric_best)),
      TRUE ~ as.numeric(!!sym(metric_ml)))) %>%
    mutate(diff = `Deep supervised learning`-`Traditional supervised learning`) %>%
    mutate(DL_better = diff > 0) %>%
    pivot_longer(cols = c(`Traditional supervised learning`,
                          `Deep supervised learning`),
                 names_to = "Method",
                 values_to = metric) %>%
    dplyr::select(Study1,
                  Study2,
                  Phenotype,
                  diff,
                  DL_better,
                  !!sym(metric),
                  Method) %>%
    mutate_if(~ all(. %in% c(0, NA)), ~ replace(., is.na(.), 0))

}



# Function to calculate the median improvement of DL models.
cal_deep_median <- function(df, metrics = "AUROC") {

  perf_summary <- ml_deep_metrics(df, metrics) %>% filter(DL_better != "")

  deep <- perf_summary %>%
    filter(Method == "Deep supervised learning") %>%
    select(!!sym(metrics)) %>% pull()

  trad <- perf_summary %>%
    filter(Method == "Traditional supervised learning") %>%
    select(!!sym(metrics)) %>% pull()

  n_pheno <- length(unique(perf_summary$Study1))
  n_study <- length(unique(str_extract(perf_summary$Study1, "[0-9]+$")))

  # library(nonpar)
  # print(mediantest(x = deep, y = trad, exact=TRUE))

  print(paste(n_study, "articles studied", n_pheno, "phenotypes, and reported",
              metrics, "with median", round(median(deep - trad),3)))

}
