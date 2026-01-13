#' stabilise_re_glmer
#'
#' @name stabilise_re_glmer
#'
#' @description Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats using clustered data.
#'
#' @param data A dataframe containing an outcome variable to be permuted.
#' @param outcome The outcome as a string (i.e. "y").
#' @param intercept_level_ids A vector names defining which variables are random effect, i.e., c("level_2_column_name", "level_3_column_name").
#' @param n_top_filter The number of variables to filter for final model (Default = 50).
#' @param boot_reps The number of bootstrap samples. Default is "auto" which selects number based on dataframe size. For glmer models, these are subsamples of the dataset, set to 80%.
#' @param permutations The number of times to be permuted per repeat. Default is "auto" which selects number based on dataframe size.
#' @param perm_boot_reps The number of times to repeat each set of permutations. Default is 20.
#' @param normalise Normalise numeric variables (TRUE/FALSE)
#' @param dummy Create dummy variables for factors/characters (TRUE/FALSE)
#' @param impute Impute missing data (TRUE/FALSE)
#' @param base_id level of the random effect to bootstrap by, e.g individual. This is likely the lower level of random effect specified
#' @param parallel TRUE or FALSE, whether to set up parallel processing
#' @param num_cores Number of cores to use if parallel processing required
#'
#'
#' @return A list containing a table of variable stabilities and a numeric permutation threshold.
#'
#' @import rsample
#' @import dplyr
#' @importFrom lme4 glmer
#' @importFrom expss gt neq lt count_row_if
#' @importFrom Hmisc rcorr
#' @importFrom matrixStats rowQuantiles
#' @importFrom purrr map
#' @importFrom future plan
#' @importFrom future multisession
#' @importFrom furrr future_map
#'
#' @export
#'

utils::globalVariables(c("models", "in_model", "mean_coefficient", "ci_lower", "ci_upper", "sta
                         ble"))

stabilise_re_glmer <- function(data, outcome, intercept_level_ids, base_id = NULL, n_top_filter = 50,
                               boot_reps = "auto", permutations = "auto", perm_boot_reps = 20,
                               normalise = FALSE, dummy = FALSE, impute = FALSE,
                               parallel = TRUE, cores = 4
) {

  if(parallel== TRUE){
    future::plan(multisession, workers = cores)
    message("Parallel set up with ", cores, " cores")
  }

  ## If no base ID set, then default to first level of intercept levels
  if(is.null(base_id)==TRUE){
    base_id <- intercept_level_ids[1]
  } else {
    base_id = base_id
  }

  data <- as_tibble(data)

  boot_reps <- stabiliser:::rep_selector_boot(data = data, boot_reps = boot_reps)
  permutations <- stabiliser:::rep_selector_perm(data = data, permutations = permutations)

  message("Stabilising across ", boot_reps, "sub-samples. Permuting ", permutations, " times, with ", perm_boot_reps, "sub-samples for each permutation.")


  # Prep non level_2 data
  level_data <- data %>%
    dplyr::select(all_of(intercept_level_ids))

  data_for_prep <- data %>%
    dplyr::select(-all_of(intercept_level_ids))

  ## Must have no missing variables, TO DO make it work with normalistion etc
  data_prepped <- stabiliser:::prep_data(data = data_for_prep, outcome = outcome, normalise = normalise, dummy = dummy, impute = impute)

  data_selected <- level_data %>%
    bind_cols(data_prepped)


  rand_names <- paste0("")

  for (level_name in intercept_level_ids) {
    rand_names <- paste0(rand_names, "+ (1|", level_name, ")")
  }

  ## Set up DF to catch coefficients - including factor levels
  x_names <- data %>%
    dplyr::select(-outcome, -all_of(intercept_level_ids))

  ## get factor cols
  factor_cols <- map_lgl(x_names, is.factor)

  x_factors <- x_names[, factor_cols]

  ## Optional processing for if the dataframe contains factors
  if(length(x_factors >0)){
    levels_list <- list()
    for(fac in 1:length(x_factors)){
      print(colnames(x_factors)[fac])
      print(levels(x_factors[[fac]]))

      levels_list[[fac]] <- paste(colnames(x_factors)[fac], levels(x_factors[[fac]]), sep = "")
    }

    levels_df <- levels_list %>%
      map_dfr(as_tibble)
    colnames(levels_df)[1] <- "variable"
  }

  df_re_model <- as.data.frame(colnames(x_names))
  colnames(df_re_model)[1] <- "variable"

  ## remove factors to re-add factors with levels
  df_re_model <- df_re_model %>%
    filter(!variable %in% colnames(x_factors))

  if(exists("levels_df")){
    df_re_model <- rbind(df_re_model, levels_df)
  }

  message("Dataset up and running...")

  ## Set up a base data-frame to catch coefficients at various point
  base_names <- df_re_model[,1]
  base_df <- data.frame(variable = base_names)

  message("Done")
  message("Stabilising glmer...")

  boot_res <- vector("list", length = boot_reps )

  # Calculate stability

  boot_function <- function(i, data_selected, base_id){

    ## Bootstrap by Base ID - to keep observations together
    message("Subsampling by ", base_id)
    base_id_char <- as.character(base_id)

    # Split data by ID
    groups <- data_selected %>%
      group_split(.data[[base_id_char]], .keep = TRUE)

    # Extract group keys
    group_ids <- map_chr(groups, ~ as.character(.x[[base_id_char]])[1])

    # Stability selection based on subsmaplingSample group IDs with replacement
    boot_ids <- sample(group_ids, size = floor(length(group_ids)*0.8), replace = FALSE)

    # Map sampled IDs back to their group data and bind
    RE_boot <- boot_ids %>%
      map(~ groups[[match(.x, group_ids)]]) %>%
      bind_rows() %>%
      mutate(.replicate = 1L)

    x_names_filtered <- RE_boot %>%
      dplyr::select(
        -all_of(outcome),
        -all_of(intercept_level_ids)
      )

    mod_code <- paste(colnames(x_names_filtered), sep = "", collapse = " + ")
    print(mod_code)
    mod_sim_RE_boot <- suppressMessages(glmer(paste0(outcome, " ~ ", mod_code, rand_names), data = RE_boot, family = binomial(link= "logit"), control=glmerControl(optimizer="bobyqa")))
    mod_sim_RE_out <- summary(mod_sim_RE_boot)

    print(mod_sim_RE_out)

    COEFS <- as.data.frame(mod_sim_RE_out$coefficients)
    COEFS$variable <- rownames(COEFS)
    selected_no_intercept <- COEFS %>%
      filter(variable != "(Intercept)")

    selected <- selected_no_intercept %>% filter(selected_no_intercept$`z value` > 2 | selected_no_intercept$`z value` < -2)

    ## Original names
    orig_names <- colnames(RE_boot)

    ## This allows use of not dummy factors, so that names can be matched
    ## Check names so if in original names, use selected matches - or factors dont work properly
    selected_matches <- c()
    for(entry in selected$variable){
      for(name in orig_names){
        if(grepl(name, entry)){
          selected_matches[entry] <- name
        }
      }
    }

    selected_matches <- unique(selected_matches)


    boot_final_mod_data <- RE_boot[, selected_matches]
    boot_final_mod_data_2 <- RE_boot %>%
      dplyr::select(outcome, all_of(intercept_level_ids)) %>%
      bind_cols(boot_final_mod_data)

    ## This needs to vary depending on how many random effects are in the model
    length_rand <- length(intercept_level_ids) + 1
    length_cols <- length(intercept_level_ids) + 1 + 1  ## get correct column positions, it depends on how many r
    if(ncol(boot_final_mod_data_2) > length_rand){
      final_mod_code <- paste(colnames(boot_final_mod_data_2[, length_cols:ncol(boot_final_mod_data_2)]), sep = "", collapse = "+")

      final_boot_mod <- suppressMessages(glmer(paste0(outcome, " ~ ",  final_mod_code, rand_names), data = boot_final_mod_data_2, family = binomial(link= "logit"), control=glmerControl(optimizer="bobyqa")))
    }

    if(ncol(boot_final_mod_data_2) <= length_rand){  # TODO: Can't rely on implied column numbers here - this is because it depends how many random effects there are
      rand_names_re_only <- substring(rand_names, 2)

      final_boot_mod <- suppressMessages(glmer(paste0(outcome, " ~ ", rand_names_re_only), data = boot_final_mod_data_2, family = binomial(link= "logit"), control=glmerControl(optimizer="bobyqa"))) ## Added bracket here CHECK
    }

    final_boot_mod_out <- summary(final_boot_mod)

    COEFS_final <- as.data.frame(final_boot_mod_out$coefficients)
    COEFS_final$variable <- rownames(COEFS_final)

    select_coef <- COEFS_final %>% dplyr::select(variable, Estimate) ## Changed to names here rather than column positions

    out <- left_join(base_df, select_coef, by = "variable")

    return(out)
  }

  if(parallel == TRUE){
    list_out <- future_map(
      1:boot_reps,
      boot_function,
      data_selected = data_selected,
      base_id = base_id,
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    )
  } else {

    list_out <- map(1:boot_reps, boot_function, data = data_selected)

  }

  # Convert all list elements to tibbles
  dfs <- map(list_out, as_tibble)
  # Identify the join key (first column name)
  join_key <- names(dfs[[1]])[1]
  # Reduce with left_join by the key
  df_re_model <- reduce(dfs, left_join, by = join_key)

  ## Stability calculations
  CMS_NEW <- df_re_model[, -1]
  CMS_NEW <- (mapply(CMS_NEW, FUN = as.numeric))
  CMS_NEW <- matrix(data = CMS_NEW, ncol = ncol(CMS_NEW), nrow = nrow(CMS_NEW))
  CMS_NEW_quant <- as.data.frame(rowQuantiles(CMS_NEW, rows = NULL, cols = NULL, na.rm = TRUE, probs = c(0.025, 0.5, 0.975))) # 95% interval for all variables
  rownames(CMS_NEW_quant) <- df_re_model$variable

  CMS_NEW_quant$sqrd2.5 <- sqrt(CMS_NEW_quant$`2.5%`^2)
  CMS_NEW_quant$sqrd5 <- sqrt(CMS_NEW_quant$`50%`^2)
  CMS_NEW_quant$sqrd97.5 <- sqrt(CMS_NEW_quant$`97.5%`^2)

  CMS_NEW[is.na(CMS_NEW)] <- 0

  ## Base R replacement for expss::count_row_if()
  # Number of bootstrap columns
  nmber_bootstraps <- ncol(CMS_NEW)

  # Row-wise count of non-zero entries
  nmber_not_zero <- rowSums(CMS_NEW != 0, na.rm = TRUE)

  # Percent of non-zero per row (out of all bootstraps/columns)
  percent_counts_in_model_join_4 <- (100 * nmber_not_zero) / nmber_bootstraps
  percent_counts_in_model_join_4 <- as.data.frame(percent_counts_in_model_join_4)

  # Row-wise count of positive (> 0) and negative (< 0) entries
  num_pos <- rowSums(CMS_NEW > 0, na.rm = TRUE)
  num_neg <- rowSums(CMS_NEW < 0, na.rm = TRUE)

  # Proportion among non-zero only; guard against division by zero
  P_value_calc1_in_model_join_4 <- as.data.frame(
    ifelse(nmber_not_zero > 0, num_pos / nmber_not_zero, NA_real_)
  )

  P_value_calc2_in_model_join_4 <- as.data.frame(
    ifelse(nmber_not_zero > 0, num_neg / nmber_not_zero, NA_real_)
  )

  p_calcs <- as.data.frame((cbind(P_value_calc1_in_model_join_4, P_value_calc2_in_model_join_4)))


  colnames(p_calcs)[1:2] <- c("p1", "p2")
  p_calcs$Pvalue <- apply(p_calcs[1:2], 1, FUN = min)

  print("P old version:")
  print(p_calcs)


  percent_counts_in_model_join_4 <- as.data.frame(cbind(CMS_NEW_quant, percent_counts_in_model_join_4, p_calcs$Pvalue))
  colnames(percent_counts_in_model_join_4)[7] <- "percent_in_model"
  colnames(percent_counts_in_model_join_4)[8] <- "Boot P"

  print("P next step:")
  print(percent_counts_in_model_join_4)

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

  message("Permuting glmer...")

  # Permutation
  perm_function <- function(i, j, data, outcome, perm_boot_reps, base_id){


    data_to_use <- as_tibble(data)
    df <- as_tibble(data)

    ## Permutatations need to happen within group for glmer?
    outcome_variable <- data_to_use %>%
      dplyr::select(dplyr::all_of(outcome))

    outcome_variable <- outcome_variable[sample(1:nrow(outcome_variable), nrow(outcome_variable), replace = TRUE), ]

    x_variables <- data %>%
      dplyr::select(-dplyr::all_of(outcome))

    data_to_use <- outcome_variable %>%
      bind_cols(x_variables)

    table_stabil_means_PERM_multi <- as.data.frame(colnames(x_names))

    colnames(table_stabil_means_PERM_multi)[1] <- "variable"

    stab_df_coefs_PERM <- as.data.frame(base_names)
    colnames(stab_df_coefs_PERM)[1] <- "variable"
    stab_df_stabil_PERM <- as.data.frame(base_names)
    colnames(stab_df_stabil_PERM)[1] <- "variable"

    df_re_model <- data.frame(base_names)
    colnames(df_re_model)[1] <- "variable"

    data_selected <- data_to_use  %>%
      group_by(across(all_of(intercept_level_ids))) %>%
      mutate(
        across(all_of(outcome),
               ~ sample(.x, size = length(.x), replace = FALSE))
      ) %>%
      ungroup()

    for (j in 1:perm_boot_reps) {

      ## Bootstrap by Base ID - to keep observations together
      base_id_char <- as.character(base_id)

      # Split data by ID
      groups <- data_selected %>%
        group_split(.data[[base_id_char]], .keep = TRUE)

      # Extract group keys
      group_ids <- map_chr(groups, ~ as.character(.x[[base_id_char]])[1])

      # Sub-sample group IDs without replacement
      boot_ids <- sample(group_ids, size = floor(length(group_ids)*0.8), replace = TRUE)

      # Map sampled IDs back to their group data and bind
      RE_boot <- boot_ids %>%
        map(~ groups[[match(.x, group_ids)]]) %>%
        bind_rows() %>%
        mutate(.replicate = 1L)

      mod_code <- paste(colnames(RE_boot[, 3:ncol(RE_boot)]), sep = "", collapse = "+")

      mod_sim_RE_boot <- suppressMessages(glmer(paste0(outcome, " ~ ", mod_code, rand_names), data = RE_boot, family = binomial(link= "logit"), control=glmerControl(optimizer="bobyqa")))

      mod_sim_RE_out <- summary(mod_sim_RE_boot)

      COEFS <- as.data.frame(mod_sim_RE_out$coefficients)

      selected <- COEFS %>% filter(COEFS$`z value` > 2 | COEFS$`z value` < -2)

      selected$variable <- rownames(selected)
      select_coef <- selected[, c("variable", "Estimate")]
      df_re_model <- left_join(df_re_model, select_coef, by = "variable")
    }

    P_CMS_NEW <- df_re_model[, -1]
    P_CMS_NEW[is.na(P_CMS_NEW)] <- 0
    P_CMS_NEW <- (mapply(P_CMS_NEW, FUN = as.numeric))
    P_CMS_NEW <- matrix(data = P_CMS_NEW, ncol = ncol(P_CMS_NEW), nrow = nrow(P_CMS_NEW))

    P_CMS_NEW_quant <- as.data.frame(rowQuantiles(P_CMS_NEW, rows = NULL, cols = NULL, na.rm = TRUE, probs = c(0.025, 0.5, 0.975)))
    rownames(P_CMS_NEW_quant) <- df_re_model$variable

    P_CMS_NEW_quant$sqrd2.5 <- sqrt(P_CMS_NEW_quant$`2.5%`^2)
    P_CMS_NEW_quant$sqrd5 <- sqrt(P_CMS_NEW_quant$`50%`^2)
    P_CMS_NEW_quant$sqrd97.5 <- sqrt(P_CMS_NEW_quant$`97.5%`^2)

    #nmber_bootstraps <- ncol(P_CMS_NEW)
    #nmber_not_zero <- as.data.frame(count_row_if(neq(0), P_CMS_NEW[, 1:ncol(P_CMS_NEW)]))
    #percent_counts_in_model_join_4 <- as.data.frame((100 * count_row_if(neq(0), P_CMS_NEW[, 1:ncol(P_CMS_NEW)])) / nmber_bootstraps)

    nmber_bootstraps <- ncol(P_CMS_NEW)

    # Row-wise count of non-zero entries
    nmber_not_zero <- rowSums(P_CMS_NEW != 0, na.rm = TRUE)

    # Percent of non-zero per row (out of all bootstraps/columns)
    percent_counts_in_model_join_4 <- (100 * nmber_not_zero) / nmber_bootstraps
    percent_counts_in_model_join_4 <- as.data.frame(percent_counts_in_model_join_4)

    # Row-wise count of positive (> 0) and negative (< 0) entries
    num_pos <- rowSums(P_CMS_NEW > 0, na.rm = TRUE)
    num_neg <- rowSums(P_CMS_NEW < 0, na.rm = TRUE)

    # Proportion among non-zero only; guard against division by zero
    P_value_calc1_in_model_join_4 <- as.data.frame(
      ifelse(nmber_not_zero > 0, num_pos / nmber_not_zero, NA_real_)
    )

    P_value_calc2_in_model_join_4 <- as.data.frame(
      ifelse(nmber_not_zero > 0, num_neg / nmber_not_zero, NA_real_)
    )

    p_calcs <- as.data.frame((cbind(P_value_calc1_in_model_join_4, P_value_calc2_in_model_join_4)))

    colnames(p_calcs)[1:2] <- c("p1", "p2")
    p_calcs$Pvalue <- apply(p_calcs[1:2], 1, FUN = min)

    percent_counts_in_model_join_4 <- as.data.frame(cbind(P_CMS_NEW_quant, percent_counts_in_model_join_4, p_calcs$Pvalue))
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

    return(table_stabil_means_PERM)


  }

  if(parallel == TRUE){
    perm_list_out <- future_map(
      1:permutations,
      perm_function,
      data = data,
      outcome = outcome,
      perm_boot_reps = perm_boot_reps,
      base_id = base_id,
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    )
  } else {

    perm_list_out <- map(1:permutations, perm_function, data = data)

  }


  # Convert all list elements to tibbles
  perm_stab_out <- map(perm_list_out, as_tibble)
  # Identify the join key (first column name)
  perm_join_key <- names(perm_stab_out[[1]])[1]
  # Reduce with left_join by the key
  table_stabil_means_PERM_multi <- reduce(perm_stab_out, left_join, by = perm_join_key)

  max_stab_df <- data.frame()
  for (col in 2:ncol(table_stabil_means_PERM_multi)) {
    max_stab <- max(table_stabil_means_PERM_multi[, col], na.rm = TRUE)
    max_stab_df <- rbind(max_stab_df, max_stab)
  }

  perm_thresh <- mean(max_stab_df, na.rm = TRUE)
  print(paste("perm threshold is:", perm_thresh))
  table_stabil_means$in_model <- ifelse(table_stabil_means$stability > perm_thresh, 1, 0)
  coefs_in_model <- as.data.frame(full_join(table_stabil_means, coef_means, by = "variable"))
  in_model_selected <- table_stabil_means %>% filter(in_model == 1)
  selected_variables <- in_model_selected$variable

  ## Match selected variables to their form in the dataset
  orig_names <- colnames(data)

  ## This allows use of not dummy factors, so that names can be matched
  ## Check names so if in original names, use selected matches - or factors dont work properly
  selected_matches <- c()
  for(entry in in_model_selected$variable){
    for(name in orig_names){
      if(grepl(name, entry)){
        selected_matches[entry] <- name
      }
    }
  }

  selected_matches <- unique(selected_matches)

  df <- as_tibble(data) ## create outside of function as not returned in paralell
  selected_to__model <- df %>% select(selected_matches)

  selected_to__model2 <- df %>%
    select(all_of(outcome), intercept_level_ids) %>%
    bind_cols(selected_to__model)

  mod_code_sel <- paste(colnames(selected_to__model), sep = "", collapse = "+")

  selected_mod <- glmer(paste0(outcome, " ~ ", mod_code_sel, rand_names), data = selected_to__model2, family = binomial(link= "logit"), control=glmerControl(optimizer="bobyqa"))

  selected_mod_out <- summary(selected_mod)

  selected_variances_out <- as.data.frame(selected_mod_out$varcor)
  selected_SD_lev_1 <- round(selected_variances_out$sdcor[2], 2)
  selected_SD_lev_2 <- round(selected_variances_out$sdcor[1], 2)

  stability <- as_tibble(coefs_in_model[c(2, 1, 3:6)]) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*")) %>%
    select(-in_model) %>%
    select(variable, mean_coefficient, ci_lower, ci_upper, stability, stable)

  variable_names <- data %>%
    select(-outcome) %>%
    colnames()

  list_out <- list("glmer" = list("stability" = stability, "perm_thresh" = perm_thresh, "variable_names" = variable_names))
  message("Done")
  return(list_out)
}



