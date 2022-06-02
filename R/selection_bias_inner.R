#' selection_bias_inner
#'
#' @name selection_bias_inner
#'
#' @description An function to illustrate the risk of selection bias in
#' conventional modelling approaches by simulating a dataset with no information and
#' conducting conventional modelling with prefiltration.
#'
#' @param nrows The number of rows to simulate.
#' @param ncols The number of columns to simulate.
#' @param p_thresh The p-value threshold to use in univariate pre-filtration.
#'
#' @return A list including a dataframe of results, a dataframe of the median number of variables selected and a plot illustrating false positive selection.
#'
#' @keywords internal
#' @import dplyr
#' @import purrr
#' @importFrom  Hmisc rcorr
#' @importFrom lme4 lmer
#' @importFrom lmerTest step
#'
utils::globalVariables(c("Eliminated"))

selection_bias_inner <- function(nrows, ncols, p_thresh) {
  db_nvars_selected_re <- data.frame()
  num_noise_vars <- ncols
  n_lev_2 <- 5 # NUMBER LEVEL 1 UNITS PER LEVEL 2 UNIT
  SD_lev_2 <- runif(1, min = 0.5, max = 6)
  SD_lev_1 <- runif(1, min = 0.5, max = 6)
  nlev2 <- nrows / n_lev_2 # NUMBER OF LVEL 2 SUBJECTS

  ## ADD CLUSTERING AT LEVEL 2
  rand_eff <- rnorm(nlev2, 0, SD_lev_2)
  rand_eff_rows <- rep(rand_eff, each = n_lev_2) # gives a RE to each level 2

  y_out <- rnorm(nrows, 0, 1) + rand_eff_rows

  ## ADD noise UNCOOR VARS
  noise_vars <- data.frame(replicate(num_noise_vars, rnorm(nrows, 0, 1)))
  data_all <- cbind(y_out, noise_vars)

  colnames(data_all)[2:ncol(data_all)] <- c(paste0("XX", 2:(ncol(data_all)))) # RENAME THE UNASSOCIATED X COLUMNS TO KEEP SIMPLE

  ## ADD RE LABELS
  df_rand <- data.frame(Doubles = double())
  for (i in 1:nlev2) {
    randoms <- as.vector(rep(i, n_lev_2))
    df_rand <- as.data.frame(rbind(c(df_rand, randoms)))
  }

  data_all$level2 <- unlist(t(df_rand[-1]))
  data_to_use <- data_all
  x_names <- data_to_use %>% dplyr::select(-y_out, -level2) ## ALTER IF DIFFERENT LEVELS INVOLVED
  rand_names <- "+ (1|level2)" ## ADD IF MORE LEVELS

  ### FILTER CODE
  df_cor1_func <- as.data.frame(matrix(0, ncol = 2, nrow = 1))
  colnames(df_cor1_func)[1] <- "r"
  colnames(df_cor1_func)[2] <- "p"

  for (i in 1:(ncol(x_names))) {
    x_cor <- as.data.frame(cbind(data_to_use$y_out, x_names[, i]))
    colnames(x_cor)[1:2] <- c("y_out", "xi")
    rcor_10 <- rcorr(as.matrix(x_cor), type = "pearson")
    r <- rcor_10$r[1, 2]
    p <- rcor_10$P[1, 2]
    df_calc <- cbind(r, p)
    df_calc
    df_cor1_func <- rbind(df_cor1_func, df_calc)
  }

  df_cor1_func <- as.data.frame(df_cor1_func[-c(1), ]) # removes blank first row and the y_out to y_out correlaton
  df_cor1_func$ID <- colnames(x_names)
  df_cor1_func$p <- as.numeric(df_cor1_func$p)

  vars_select <- df_cor1_func %>% filter(p < p_thresh)

  v_sel <- vars_select$ID
  nmb_filt <- NROW(v_sel)
  x_selected <- data_to_use[, v_sel]

  if (nmb_filt < 1) {
    nmbr_sel <- 0
    nmbr_sel_thresh <- cbind(nmbr_sel, ncols, nrows, p_thresh, SD_lev_1, SD_lev_2)
    db_nvars_selected_re <- rbind(db_nvars_selected_re, nmbr_sel_thresh)
  } else {
    data_selected <- as.data.frame(cbind(data_to_use$y_out, data_to_use$level2, x_selected))

    colnames(data_selected)[1] <- c("y_out")
    colnames(data_selected)[2] <- c("level2")


    # Fit the full model
    mod_var_names <- paste(colnames(data_selected[, 3:ncol(data_selected)]), sep = "", collapse = "+")
    mod_var_names <- noquote(mod_var_names)
    mod_code <- noquote(mod_var_names)
    mod_re <- lmerTest::lmer(paste0("y_out~", mod_code, " + (1|level2)"), data = data_selected)
    mod_re@call$data <- data_selected # Required within function, lmerTest::step doesn't find data_selected otherwise
    back_out <- lmerTest::step(mod_re, alpha.fixed = 0.05, reduce.random = FALSE)

    selected_in <- as.data.frame(back_out$fixed) %>% filter(Eliminated == 0)

    if (nrow(selected_in) < 1) {
      nmbr_sel <- 0
    } else {
      nmbr_sel <- nrow(selected_in)
    }

    nmbr_sel_thresh <- as.data.frame((cbind(nmbr_sel, ncols, nrows, p_thresh, SD_lev_1, SD_lev_2)))
    db_nvars_selected_re <- rbind(db_nvars_selected_re, nmbr_sel_thresh)
    results_table_out <- as.data.frame(db_nvars_selected_re)
    results_table_out
  }
}
