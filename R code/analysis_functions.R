# --------------------------------- R packages --------------------------------- #

library(splitstackshape)
library(janitor)

# --------------------------------- Plotting --------------------------------- #

# Function for stacked bar plot
#--------------------------------#
# df: dataframe
# group_var: Variable to group by (ie. x axis of the plot)
# group_var_stack: Variable to stack by
# order_by_count: Whether to order bars by counts
# string_wrap: Whether to perform string wrapping for long group_var names
# large_fig: Whether to make a large figure
stacked_bar <- function(df,
                        group_var,
                        group_var_stack,
                        order_by_count = TRUE,
                        string_wrap = NA,
                        large_fig = FALSE){

  # Split the label text if it is too long.
  if (!is.na(string_wrap)) {
    df[, group_var] <- str_wrap(pull(df[, group_var]),
                                width = string_wrap)
  }

  # Create two new variables: (i) n: count for a column and
  # (ii) plot_n: column height to be used in the stacked bar
  # plot.  The plot_n variable deals with the case where the count number
  # in the column is too large to display in a stacked bar chart.
  temp_df <- df %>%
    group_by(!!sym(group_var), !!sym(group_var_stack)) %>%
    summarise(Count = n()) %>%
    mutate(plot_n = Count + 3)

  # Order the columns by count, if desired.
  if (order_by_count) {

    p <- ggplot(data = temp_df,
                aes(x = reorder(!!sym(group_var), -plot_n, sum),
                    y = plot_n,
                    fill = !!sym(group_var_stack)))

  } else {

    p <- ggplot(data = temp_df,
                aes(x = !!sym(group_var),
                    y = plot_n,
                    fill = !!sym(group_var_stack)))
  }

  # Adjust font size.
  text_size <- if_else(large_fig, 8, 3)
  axis_text_size <- if_else(large_fig, 20, 9)
  axis_title_size <- if_else(large_fig, 15, 7)

  p + geom_bar(stat = "identity", position = "stack") +
    scale_fill_jama() +
    labs(x = "", y = "Number of articles") +
    guides(fill = guide_legend("")) +
    geom_text(aes(label = Count),
              size = text_size,
              color = "white",
              position = position_stack(vjust = 0.5)) +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(face = "bold", size = axis_text_size),
          axis.text = element_text(size = axis_text_size, colour = "black"),
          axis.title = element_text(size = axis_text_size),
          legend.title = element_text(size = axis_text_size),
          legend.text = element_text(size = axis_title_size))
}


# Function for horizontal stacked bar plot
#----------------------------------------#
# df: dataframe
# group_var: Variable to group by (ie. x axis of the plot)
# group_var_stack: Variable to stack by
# restrict_count: Whether to restrict group_var to be at least 2
# top_count: Maximum count of group_var
# y_lim: Restriction for the y limit
# label_text: Whether to label bars with the count
# title: Title name for the plot
# ylab: y_axis label
# string_wrap: Whether to perform string wrapping for long group_var names
# color_grid: Option grid of colors to use
# large_fig: Whether to make a large figure
# legend_caption: Caption for the legend
horizontal_bar <- function(df,
                           group_var,
                           group_var_stack = NA,
                           restrict_count = FALSE,
                           top_count = 999,
                           ylim = NA,
                           label_text = TRUE,
                           title = NULL,
                           ylab = NA,
                           string_wrap = NA,
                           color_grid = NA,
                           large_fig = FALSE,
                           legend_cap = "Unsupervised phenotype category"){

  # Split the label text if it is too long.
  if (!is.na(string_wrap)) {

    df[, group_var] <- str_wrap(pull(df[, group_var]), width = string_wrap)

  }

  # Group by stacked variable, if any.
  if (is.na(group_var_stack)) {

    temp_df <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Count = n()) %>%
      mutate(n = Count)

  } else {

    df[, legend_cap] <- df[, group_var_stack]

    temp_df <- df %>%
      group_by(!!sym(group_var)) %>%
      mutate(n = n()) %>%
      group_by(!!sym(group_var), !!sym(legend_cap)) %>%
      summarise(n = mean(n), Count = n())

  }

  # Restict `group_var` to be > 1, if desired.
  if (restrict_count) {

    temp_df <- temp_df %>% filter(n > 1)

  }

  # Keep the top n bars.
  temp_df <- temp_df %>% arrange(-n) %>% top_n(top_count, n)

  # Set up plots.
  if (is.na(group_var_stack)) {

    p <- ggplot(data = temp_df,
                aes(x = reorder(!!sym(group_var), -Count, sum),
                    y = Count))

    if (!is.na(color_grid)) {

      p <- p + geom_bar(aes(fill = !!sym(group_var)), stat = "identity")

    } else {

      p <- p + geom_bar(stat = "identity")

    }

  } else {

    p <- ggplot(data = temp_df,
                aes(x = reorder(!!sym(group_var), -Count, sum),
                    y = Count,
                    alpha = !!sym(legend_cap))) +
      geom_bar(stat="identity")
  }

  # Label the number on top of the bars, if desired.
  if (label_text) {

    p <- p + geom_text(aes(label = Count),
                       position = position_dodge(width = 0.9),
                       hjust = -0.5,
                       size = 3)
  }

  # Restrict the y limit, if desired.
  if (!is.na(ylim))  p <- p + ylim(0, ylim)

  # Make the plot.
  if (!is.na(group_var_stack)) {
    p <- p + geom_col_pattern(aes(pattern    = !!sym(legend_cap),
                                  pattern_angle   = !!sym(legend_cap),
                                  pattern_spacing = !!sym(legend_cap)),
                              fill            = 'white',
                              colour          = 'black',
                              pattern_density = 0.05,
                              pattern_fill    = 'black',
                              pattern_colour  = 'black') +
      theme_classic() +
      coord_flip() +
      labs(title = title, y = "", x = "") +
      theme(legend.position = "right")

  } else {

    p <- p + scale_fill_manual(values = color_grid) +
      theme_classic() +
      coord_flip() +
      labs(title = title, y = "", x = "") +
      theme(legend.position = "none")

  }

  text_size = if_else(large_fig, 15, 7)
  title_size = if_else(large_fig, 20, 12)

  p <- p +
    theme(plot.title = element_text(face = "bold", size = title_size),
          axis.text = element_text(size = text_size),
          legend.title = element_text(size = title_size),
          legend.text = element_text(size = text_size))

  if (!is.na(ylab)) p <- p + ylab(ylab)

  p +
    theme(axis.text.y = element_text(colour = "black"),
          axis.text.x = element_text(colour = "black"))

}


# Figure to plot top phenotypes.
#------------------------------#
# df: dataframe
# group_var: Variable to group by (ie. x axis of the plot)
# restrict_count: Whether to restrict group_var to be at least 2
# top_count: Maximum count of group_var
# title: Title name for the plot
# y_lim: Restriction for the y limit
# label_text: Whether to label bars with the count
# string_wrap: Whether to perform string wrapping for long group_var names
# color_grid: Option grid of colors to use
# large_fig: Whether to make a large figure
plot_top_pheno <- function(df,
                           group_var_stack = NA,
                           restrict_count = TRUE,
                           top_count = 5,
                           title,
                           ylim = 5,
                           label_text = TRUE,
                           string_wrap = 25,
                           color_grid = NA,
                           large_fig = FALSE) {

  df_wo_comp <- df %>% filter(Competition_data_name == '')
  phenotype_unnested <- unnest_string_var(df_wo_comp, "Phenotype")

  horizontal_bar(phenotype_unnested,
                 "Phenotype_unnested",
                 group_var_stack = group_var_stack,
                 restrict_count = restrict_count,
                 top_count = top_count,
                 ylim = ylim,
                 label_text = label_text,
                 title = title,
                 string_wrap = string_wrap,
                 color_grid = color_grid,
                 large_fig = large_fig)
}


# Function to plot top phenotypes for all methods.
#------------------------------------------------#
# traditional_supervised: dataframe with articles using traditional supervised
# deep_supervised: dataframe with articles using traditional supervised
# semi_supervised: dataframe with articles using traditional supervised
# weakly_supervised: dataframe with articles using weakly supervised
# un_supervised: dataframe with articles using unsupervised
plot_all_top_pheno <- function(traditional_supervised,
                               deep_supervised,
                               semi_supervised,
                               weakly_supervised,
                               un_supervised) {

  df_all <- rbind(traditional_supervised,
                  deep_supervised,
                  semi_supervised,
                  weakly_supervised,
                  un_supervised)

  df_wo_comp <- df_all %>% filter(Competition_data_name == '')

  phenotype_unnested <- unnest_string_var(df_wo_comp, "Phenotype")

  phenotype_common <- phenotype_unnested %>%
    group_by(Phenotype_unnested, ML_type, Traditional) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    arrange(-n) %>%
    group_by(ML_type, Traditional) %>%
    top_n(5)

  phenotype_unique <- phenotype_common %>%
    group_by(Phenotype_unnested) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    select(Phenotype_unnested) %>%
    pull()

  phenotype_common <- phenotype_common %>%
    select(Phenotype_unnested) %>%
    pull()

  # Set the color.
  color_grid <- rep("#000003", length(phenotype_common))
  color_panel <- pal_d3()(9)
  for (i in c(1:length(phenotype_unique))) {
    color_grid[which(phenotype_common == phenotype_unique[i])] <- color_panel[i]
  }

  names(color_grid) <- phenotype_common

  color_grid["Chronic obstructive\npulmonary disease"] <- "#000003"
  color_grid["Non-alcoholic fatty liver\ndisease"] <- "#000003"
  color_grid["Type 2 diabetes mellitus"] <- "#6fd5db"

  p1 <- plot_top_pheno(traditional_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Traditional supervised",
                       color_grid = color_grid)

  p2 <- plot_top_pheno(deep_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Deep supervised",
                       color_grid = color_grid)

  p3 <- plot_top_pheno(semi_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Semi-supervised",
                       color_grid = color_grid)

  p4 <- plot_top_pheno(weakly_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Weakly-supervised",
                       color_grid = color_grid)
  p5 <- plot_top_pheno(un_supervised,
                       label_text = FALSE,
                       top_count = 5,
                       title = "Unsupervised",
                       color_grid = color_grid)

  plot_grid(p1, p2, p3, p4, p5,
            ncol = 2,
            nrow = 3,
            align = "v",
            axis = "l",
            labels = c('(a)', '(b)', '(c)', '(d)', '(e)'),
            label_size = 12)
}

# ------------------------------------- Tables --------------------------------#

# Print a summary table with grouped information.
# df: dataframe.
# group_var: Variable to group by.
# group_var_2: Additional variable to group by, optional.
# group_var_3: Additional variable to group by, optional.
# title: Title of plot, optional.
# top_count: Largest number of rows to display for each groupby variable,
#   default is 5.
# restric_count: Whether to restrict to articles with a value of at least one
#   in the grouped variables
# col_width: Column width for latex tabels, default is 5.
# hold_pos: Whether to scale latex tables, default is FALSE.
print_tables <- function(df,
                         group_var,
                         group_var_2 = NA,
                         group_var_3 = NA,
                         title = NA,
                         top_count = 5,
                         restric_count = TRUE,
                         col_width = 5,
                         hold_pos = FALSE) {

  # Group data based on grouping variables.
  if (is.na(group_var_2)) {

    df <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Count = n()) %>%
      slice_max(order_by = Count, n = top_count) %>%
      arrange(-Count)

    if (restric_count) df <- df %>% filter(Count > 1)

  } else if (is.na(group_var_3)) {

    df <- df %>%
      group_by(!!sym(group_var), !!sym(group_var_2)) %>%
      summarise(Count = n()) %>%
      slice_max(order_by = Count, n = top_count)

    if (restric_count) df <- df %>% filter(Count > 1)

  } else {

    df1 <- df %>%
      group_by(!!sym(group_var)) %>%
      summarise(Count = n()) %>%
      slice_max(order_by = Count, n = top_count)

    if (restric_count) df1 <- df1 %>% filter(Count > 1)

    df <- df %>%
      group_by(!!sym(group_var), !!sym(group_var_2), !!sym(group_var_3)) %>%
      summarise(Count = n()) %>%
      pivot_wider(names_from = c("ML_type", "Traditional"),
                  values_from = "Count",
                  names_sep = " ",
                  values_fill = 0) %>%
      inner_join(df1) %>%
      arrange(-Count)

  }

  new_name <- strsplit(colnames(df)[1], "_")
  new_name <- new_name[[1]]

  colnames(df)[1] <- str_c(new_name[-length(new_name)], collapse = " ")

  if (!is.na(group_var_2) & is.na(group_var_3)) {

    new_name <- strsplit(colnames(df)[2], "_")
    new_name <- new_name[[1]]

    colnames(df)[2] <- str_c(new_name[-length(new_name)], collapse = " ")

  }

  # To split the label text if it is too long.
  if (!is.na(group_var_3)) {

    num_col <- ncol(df)

    # Latex table options, whether to scale down table size.

    if (hold_pos) {
      df %>%
        kbl(booktabs = T, caption = title) %>%
        kable_paper("striped") %>%
        kable_styling(position = "center", latex_options = "HOLD_position") %>%
        column_spec(c(1:num_col), width = paste0(col_width, "em"))

    } else {

      df %>%
        kbl(booktabs = T, caption = title) %>%
        kable_paper("striped") %>%
        kable_styling(position = "center", latex_options = "scale_down") %>%
        column_spec(c(1:num_col), width = paste0(col_width, "em"))

    }

  } else {

    df %>%
      kbl(booktabs = T, caption = title) %>%
      kable_paper("striped") %>%
      kable_styling(position = "center", latex_options = "HOLD_position")

  }

}




# Function for number of papers using multiple types of a single variable.
# df: dataframe.
# item: variable to count number of papers using multiples.
print_multiple_items <- function(df, item) {

  n_multiple <- df %>%
    group_by(PMID) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    nrow()

  print(paste('There are', n_multiple, 'papers using multiple', item))

}


# ---------------------------- Unnested functions ---------------------------- #

# Function to unnest a column with strings separated by a semi-colon.
# df: dataframe
# var: variable name to unnest
unnest_string_var <- function(df, var) {

  df %>% mutate(var_unnested = (strsplit(as.character(!!sym(var)), ";"))) %>%
    unnest(var_unnested) %>%
    mutate_if(is.character, trimws) %>%
    rename_with(stringr::str_replace,
                pattern = "var_unnested",
                replacement = paste0(var, "_unnested"),
                matches("var_unnested"))

}

# Function to unnest two column with strings separated by a semi-colon.
unnest_validate_string <- function(df, comapartor = "deep") {

  # Specifically for validation columns, they are recorded in paired columns by
  # semi-colons.

  # Input:  col 1: phenotypes hypertension;obesity  col 2: specificity 0.99;0.22
  # Output: hypertension_specificity: 0.99          obesity_specificity: 0.22

  res <- df %>% select(PMID, Data_source)

  if (comapartor == "deep") {

    for (metric in c("Sensitivity", "Specificity", "PPV", "AUROC")) {

      metric_best <- paste0("Best_performing_", metric)
      metric_ml <- paste0("Best_comparator_traditional_", metric)
      metric_deep <- paste0("Best_comparator_DL_", metric)

      best <- unnest_two_string(df, vars = c("Phenotype", metric_best))
      ml <- unnest_two_string(df, vars = c("Phenotype", metric_ml))
      deep <- unnest_two_string(df, vars = c("Phenotype", metric_deep))

      mfile <- merge(best, ml, all = TRUE)
      mfile <- merge(mfile, deep, all = TRUE)
      res <- merge(res, mfile, all = TRUE)

    }

  } else {

    for (metric in c("Sensitivity", "Specificity", "PPV", "NPV", "AUROC")) {

      metric_best <- paste0("Best_performing_", metric)
      metric_ml <- paste0("Best_comparator_traditional_", metric)
      metric_rule <- paste0("Best_comparator_Rule_", metric)

      best <- unnest_two_string(df, vars = c("Phenotype", metric_best))
      ml <- unnest_two_string(df, vars = c("Phenotype", metric_ml))
      rule <- unnest_two_string(df, vars = c("Phenotype", metric_rule))

      mfile <- merge(best, ml, all = TRUE)
      mfile <- merge(mfile, rule, all = TRUE)
      res <- merge(res, mfile, all = TRUE)

    }

  }

  res <- res[!is.na(res$Best_performing_model),]
  #res <- res[!is.na(res$Data_source), ]
  return(res)

}

