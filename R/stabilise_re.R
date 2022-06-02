#' stabilise_re
#'
#' @name stabilise_re
#'
#' @description Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats using clustered data.
#'
#' @param data A dataframe containing an outcome variable to be permuted.
#' @param outcome The outcome as a string (i.e. "y").
#' @param level_2_id The variable name determining level 2 status as a string (i.e., "level_2_column_name").
#' @param n_top_filter The number of variables to filter for final model (Default = 50).
#' @param boot_reps The number of bootstrap samples. Default is "auto" which selects number based on dataframe size.
#' @param permutations The number of times to be permuted per repeat. Default is "auto" which selects number based on dataframe size.
#' @param perm_boot_reps The number of times to repeat each set of permutations. Default is 20.
#' @param normalise Normalise numeric variables (TRUE/FALSE)
#' @param dummy Create dummy variables for factors/characters (TRUE/FALSE)
#' @param impute Impute missing data (TRUE/FALSE)
#'
#'
#' @return A list containing a table of variable stabilities and a numeric permutation threshold.
#'
#' @import rsample
#' @import dplyr
#' @importFrom lme4 lmer
#' @importFrom expss gt neq lt count_row_if
#' @importFrom Hmisc rcorr
#' @importFrom matrixStats rowQuantiles
#' @importFrom purrr map
#'
#' @export
#'

utils::globalVariables(c("models", "in_model", "mean_coefficient", "ci_lower", "ci_upper", "stable"))

stabilise_re <- function(data, outcome, level_2_id, n_top_filter = 50,
                         boot_reps = "auto", permutations = "auto", perm_boot_reps = 20,
                         normalise = TRUE, dummy = TRUE, impute = TRUE) {
  boot_reps <- rep_selector_boot(data = data, boot_reps = boot_reps)
  permutations <- rep_selector_perm(data = data, permutations = permutations)

  message("Stabilising across ", boot_reps, " bootstrap resamples. Permuting ", permutations, " times, with ", perm_boot_reps, " bootstrap samples for each permutation.")

  # Prep non level_2 data
  level_data <- data %>%
    select(all_of(level_2_id))

  data_for_prep <- data %>%
    select(-level_2_id)

  data_prepped <- prep_data(data = data_for_prep, outcome = outcome, normalise = normalise, dummy = dummy, impute = impute)

  data <- level_data %>%
    bind_cols(data_prepped)

  message("Filtering data...")

  # Filter
  x_names <- data %>%
    select(-outcome, -level_2_id)

  df_re_model <- as.data.frame(colnames(x_names))
  colnames(df_re_model)[1] <- "variable"

  rand_names <- paste0("+ (1|", level_2_id, ")")
  df <- data

  df_cor1_func <- as.data.frame(matrix(0, ncol = 2, nrow = 1))
  colnames(df_cor1_func)[1] <- "r"
  colnames(df_cor1_func)[2] <- "p"
  for (i in 1:(ncol(x_names))) {
    x_cor <- df %>%
      select(outcome) %>%
      bind_cols(x_names[, i]) %>%
      as.data.frame()

    rcor_10 <- rcorr(as.matrix(x_cor), type = "pearson")
    r <- rcor_10$r[1, 2]
    p <- rcor_10$P[1, 2]
    df_calc <- cbind(r, p)
    df_calc
    df_cor1_func <- rbind(df_cor1_func, df_calc)
  }

  df_cor1_func <- as.data.frame(df_cor1_func[-c(1), ]) # removes blank first row and the outcome to outcome correlaton
  df_cor1_func$ID <- colnames(x_names)
  df_cor1_func$p <- as.numeric(df_cor1_func$p)
  vars_select_order <- df_cor1_func[order(df_cor1_func$p), ]

  vars_select <- vars_select_order[1:n_top_filter, ]

  v_sel <- vars_select$ID

  x_selected <- x_names[, v_sel]

  data_selected <- df %>%
    select(outcome, level_2_id) %>%
    bind_cols(., x_selected)

  message("Done")
  message("Stabilising lmer...")

  # Calculate stability
  for (i in 1:boot_reps) {
    RE_boot <- data_selected[sample(1:nrow(data_selected), nrow(data_selected), replace = TRUE), ]


    mod_code <- paste(colnames(RE_boot[, 3:ncol(RE_boot)]), sep = "", collapse = "+")
    mod_sim_RE_boot <- suppressMessages(lmer(paste0(outcome, " ~ ", mod_code, rand_names), data = RE_boot))
    mod_sim_RE_out <- summary(mod_sim_RE_boot)

    COEFS <- as.data.frame(mod_sim_RE_out$coefficients)
    COEFS$variable <- rownames(COEFS)
    selected_no_intercept <- COEFS %>% filter(variable != "(Intercept)")

    selected <- selected_no_intercept %>% filter(selected_no_intercept$`t value` > 2 | selected_no_intercept$`t value` < -2)

    boot_final_mod_data <- RE_boot[, selected$variable]
    boot_final_mod_data_2 <- RE_boot %>%
      select(outcome, level_2_id) %>%
      bind_cols(boot_final_mod_data)

    final_mod_code <- paste(colnames(boot_final_mod_data_2[, 3:ncol(boot_final_mod_data_2)]), sep = "", collapse = "+")

    final_boot_mod <- suppressMessages(lmer(paste0(outcome, " ~ ", final_mod_code, rand_names), data = boot_final_mod_data_2))

    final_boot_mod_out <- summary(final_boot_mod)

    COEFS_final <- as.data.frame(final_boot_mod_out$coefficients)
    COEFS_final$variable <- rownames(COEFS_final)

    select_coef <- COEFS_final[, c(1, 4)]
    df_re_model <- left_join(df_re_model, select_coef, by = "variable")
  }

  CMS_NEW <- df_re_model[, -1]

  CMS_NEW <- (mapply(CMS_NEW, FUN = as.numeric))
  CMS_NEW <- matrix(data = CMS_NEW, ncol = ncol(CMS_NEW), nrow = nrow(CMS_NEW))
  CMS_NEW_quant <- as.data.frame(rowQuantiles(CMS_NEW, rows = NULL, cols = NULL, na.rm = TRUE, probs = c(0.025, 0.5, 0.975))) # 95% interval for all variables
  rownames(CMS_NEW_quant) <- df_re_model$variable

  CMS_NEW_quant$sqrd2.5 <- sqrt(CMS_NEW_quant$`2.5%`^2)
  CMS_NEW_quant$sqrd5 <- sqrt(CMS_NEW_quant$`50%`^2)
  CMS_NEW_quant$sqrd97.5 <- sqrt(CMS_NEW_quant$`97.5%`^2)

  CMS_NEW[is.na(CMS_NEW)] <- 0
  nmber_bootstraps <- ncol(CMS_NEW)
  nmber_not_zero <- as.data.frame(count_row_if(neq(0), CMS_NEW[, 1:ncol(CMS_NEW)]))
  percent_counts_in_model_join_4 <- as.data.frame((100 * count_row_if(neq(0), CMS_NEW[, 1:ncol(CMS_NEW)])) / nmber_bootstraps)
  P_value_calc1_in_model_join_4 <- as.data.frame(((100 * count_row_if(gt(0), CMS_NEW[, 1:ncol(CMS_NEW)])) / nmber_not_zero) / 100)
  P_value_calc2_in_model_join_4 <- as.data.frame(((100 * count_row_if(lt(0), CMS_NEW[, 1:ncol(CMS_NEW)])) / nmber_not_zero) / 100)

  p_calcs <- as.data.frame((cbind(P_value_calc1_in_model_join_4, P_value_calc2_in_model_join_4)))

  colnames(p_calcs)[1:2] <- c("p1", "p2")
  p_calcs$Pvalue <- apply(p_calcs[1:2], 1, FUN = min)
  percent_counts_in_model_join_4 <- as.data.frame(cbind(CMS_NEW_quant, percent_counts_in_model_join_4, p_calcs$Pvalue))
  colnames(percent_counts_in_model_join_4)[7] <- "percent_in_model"
  colnames(percent_counts_in_model_join_4)[8] <- "Boot P"

  percent_counts_in_model_join_4_order <- percent_counts_in_model_join_4[order(-percent_counts_in_model_join_4$percent_in_model), ]

  stability_boot_RE_model <- percent_counts_in_model_join_4_order

  coef_means <- as.data.frame(stability_boot_RE_model$`50%`)
  coef_means$variable <- rownames(stability_boot_RE_model)

  stab_percent <- as.data.frame(stability_boot_RE_model$percent_in_model)
  stab_percent$variable <- rownames(stability_boot_RE_model)

  coef_means$ci_lower <- (stability_boot_RE_model$`2.5%`) # lower interval for all variables
  coef_means$ci_upper <- (stability_boot_RE_model$`97.5%`)

  colnames(coef_means)[1] <- "mean_coefficient"

  table_stabil_means <- stab_percent
  colnames(table_stabil_means) <- c("stability", "variable")
  table_stabil_means$stability <- as.numeric(table_stabil_means$stability)
  table_stabil_means <- table_stabil_means[order(-table_stabil_means$stability), ]

  message("Done")
  message("Permuting lmer...")

  # Permutation

  for (i in 1:permutations) {
    data_to_use <- df

    outcome_variable <- data_to_use %>%
      select(all_of(outcome))

    outcome_variable <- outcome_variable[sample(1:nrow(outcome_variable), nrow(outcome_variable), replace = TRUE), ]

    x_variables <- data %>%
      select(-all_of(outcome))

    data_to_use <- outcome_variable %>%
      bind_cols(x_variables)

    table_stabil_means_PERM_multi <- as.data.frame(colnames(x_names))

    colnames(table_stabil_means_PERM_multi)[1] <- "variable"

    stab_df_coefs_PERM <- as.data.frame(colnames(x_names))
    colnames(stab_df_coefs_PERM)[1] <- "variable"
    stab_df_stabil_PERM <- as.data.frame(colnames(x_names))
    colnames(stab_df_stabil_PERM)[1] <- "variable"

    df_re_model <- data.frame(colnames(x_names))
    colnames(df_re_model)[1] <- "variable"

    ### FILTER CODE
    df_cor1_func <- as.data.frame(matrix(0, ncol = 2, nrow = 1))
    colnames(df_cor1_func)[1] <- "r"
    colnames(df_cor1_func)[2] <- "p"
    for (j in 1:(ncol(x_names))) {
      x_cor <- data_to_use %>%
        select(outcome) %>%
        bind_cols(x_names[, j])
      rcor_10 <- rcorr(as.matrix(x_cor), type = "pearson")
      r <- rcor_10$r[1, 2]
      p <- rcor_10$P[1, 2]
      df_calc <- cbind(r, p)
      df_cor1_func <- rbind(df_cor1_func, df_calc)
    }

    df_cor1_func <- as.data.frame(df_cor1_func[-c(1), ])
    df_cor1_func$ID <- colnames(x_names)
    df_cor1_func$p <- as.numeric(df_cor1_func$p)
    vars_select_order <- df_cor1_func[order(df_cor1_func$p), ]
    vars_select <- vars_select_order[1:n_top_filter, ]

    v_sel <- vars_select$ID

    x_selected <- x_names[, v_sel]

    data_selected <- data_to_use %>%
      select(outcome, level_2_id) %>%
      bind_cols(x_selected)

    for (k in 1:perm_boot_reps) {
      RE_boot <- data_selected[sample(1:nrow(data_selected), nrow(data_selected), replace = TRUE), ]

      mod_code <- paste(colnames(RE_boot[, 3:ncol(RE_boot)]), sep = "", collapse = "+")

      mod_sim_RE_boot <- suppressMessages(lmer(paste0(outcome, " ~ ", mod_code, rand_names), data = RE_boot))

      mod_sim_RE_out <- summary(mod_sim_RE_boot)

      COEFS <- as.data.frame(mod_sim_RE_out$coefficients)

      selected <- COEFS %>% filter(COEFS$`t value` > 2 | COEFS$`t value` < -2)

      selected$variable <- rownames(selected)
      select_coef <- selected[, c(1, 4)]
      df_re_model <- left_join(df_re_model, select_coef, by = "variable")
    }

    CMS_NEW <- df_re_model[, -1]
    CMS_NEW[is.na(CMS_NEW)] <- 0
    CMS_NEW <- (mapply(CMS_NEW, FUN = as.numeric))
    CMS_NEW <- matrix(data = CMS_NEW, ncol = ncol(CMS_NEW), nrow = nrow(CMS_NEW))

    rownames(CMS_NEW_quant) <- df_re_model$variable

    CMS_NEW_quant$sqrd2.5 <- sqrt(CMS_NEW_quant$`2.5%`^2)
    CMS_NEW_quant$sqrd5 <- sqrt(CMS_NEW_quant$`50%`^2)
    CMS_NEW_quant$sqrd97.5 <- sqrt(CMS_NEW_quant$`97.5%`^2)

    nmber_bootstraps <- ncol(CMS_NEW)
    nmber_not_zero <- as.data.frame(count_row_if(neq(0), CMS_NEW[, 1:ncol(CMS_NEW)]))
    percent_counts_in_model_join_4 <- as.data.frame((100 * count_row_if(neq(0), CMS_NEW[, 1:ncol(CMS_NEW)])) / nmber_bootstraps)
    P_value_calc1_in_model_join_4 <- as.data.frame(((100 * count_row_if(gt(0), CMS_NEW[, 1:ncol(CMS_NEW)])) / nmber_not_zero) / 100)
    P_value_calc2_in_model_join_4 <- as.data.frame(((100 * count_row_if(lt(0), CMS_NEW[, 1:ncol(CMS_NEW)])) / nmber_not_zero) / 100)
    p_calcs <- as.data.frame((cbind(P_value_calc1_in_model_join_4, P_value_calc2_in_model_join_4)))

    colnames(p_calcs)[1:2] <- c("p1", "p2")
    p_calcs$Pvalue <- apply(p_calcs[1:2], 1, FUN = min)

    percent_counts_in_model_join_4 <- as.data.frame(cbind(CMS_NEW_quant, percent_counts_in_model_join_4, p_calcs$Pvalue))
    colnames(percent_counts_in_model_join_4)[7] <- "percent_in_model"
    colnames(percent_counts_in_model_join_4)[8] <- "Boot P"

    percent_counts_in_model_join_4_order <- percent_counts_in_model_join_4[order(-percent_counts_in_model_join_4$percent_in_model), ]

    stability_boot_RE_model <- percent_counts_in_model_join_4_order

    stab_coef <- as.data.frame(stability_boot_RE_model$`50%`)
    stab_coef$variable <- rownames(stability_boot_RE_model)
    stab_df_coefs_PERM <- left_join(stab_df_coefs_PERM, stab_coef, by = "variable")

    stab_percent <- as.data.frame(stability_boot_RE_model$percent_in_model)
    stab_percent$variable <- rownames(stability_boot_RE_model)
    stab_df_stabil_PERM <- left_join(stab_df_stabil_PERM, stab_percent, by = "variable")

    stab_df_stabil_PERM[is.na(stab_df_stabil_PERM)] <- 0
    stab_df_stabil_PERM$means <- rowMeans(stab_df_stabil_PERM[2:ncol(stab_df_stabil_PERM)])

    table_stabil_means_PERM <- as.data.frame(cbind(stab_df_stabil_PERM$variable, as.numeric(stab_df_stabil_PERM$means)))
    colnames(table_stabil_means_PERM) <- c("variable", "stability")
    table_stabil_means_PERM$stability <- as.numeric(table_stabil_means_PERM$stability)
    table_stabil_means_PERM <- table_stabil_means_PERM[order(-table_stabil_means_PERM$stability), ]

    table_stabil_means_PERM_multi <- left_join(table_stabil_means_PERM_multi, table_stabil_means_PERM, by = "variable")
  }

  max_stab_df <- data.frame()
  for (col in 2:ncol(table_stabil_means_PERM_multi)) {
    max_stab <- max(table_stabil_means_PERM_multi[, col])
    max_stab_df <- rbind(max_stab_df, max_stab)
  }

  perm_thresh <- mean(max_stab_df) - 0.01
  table_stabil_means$in_model <- ifelse(table_stabil_means$stability > perm_thresh, 1, 0)
  coefs_in_model <- as.data.frame(full_join(table_stabil_means, coef_means, by = "variable"))
  in_model_selected <- table_stabil_means %>% filter(in_model == 1)
  selected_variables <- in_model_selected$variable
  selected_to__model <- df %>% select(selected_variables)

  selected_to__model2 <- df %>%
    select(all_of(outcome), level_2_id) %>%
    bind_cols(selected_to__model)

  mod_code_sel <- paste(colnames(selected_to__model), sep = "", collapse = "+")

  selected_mod <- lmer(paste0(outcome, " ~ ", mod_code_sel, rand_names), data = selected_to__model2)

  selected_mod_out <- summary(selected_mod)

  selected_variances_out <- as.data.frame(selected_mod_out$varcor)
  selected_SD_lev_1 <- round(selected_variances_out$sdcor[2], 2)
  selected_SD_lev_2 <- round(selected_variances_out$sdcor[1], 2)

  stability <- as_tibble(coefs_in_model[c(2, 1, 3:6)]) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*")) %>%
    select(-in_model) %>%
    select(variable, mean_coefficient, ci_lower, ci_upper, stability, stable)

  list_out <- list("lmer" = list("stability" = stability, "perm_thresh" = perm_thresh))
  message("Done")
  return(list_out)
}
