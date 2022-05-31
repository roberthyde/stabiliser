#' simulate_data_re
#'
#' @name simulate_data_re
#'
#' @description Simulate a dataset with random effects. This can optionally include variables with a given associated with the outcome.
#'
#' @param nrows The number of rows to simulate.
#' @param ncols The number of columns to simulate.
#' @param n_true The number of variables truly associated with the outcome.
#' @param amplitude The strength of association between true variables and the outcome.
#'
#' @return A simulated dataset
#'
#' @import dplyr
#' @import lme4
#' @importFrom stats rnorm toeplitz
#'
#' @export

simulate_data <- function(nrows, ncols, n_true = 2, amplitude = 2,
                          n_level_2 = 2, sd_level_1 = 0.5, sd_level_2 = 0) {
  # nlev2 must be positive integer
  # n_true must be positive integer

  nlev2 <- nrows / n_level_2

  uncor_sd_true <- data.frame(replicate(n_true, rnorm(nrows, 0, 1)))

  ##  FIXED EFFECTS AT LEVEL 2
  fixed_effect_level_2 <- data.frame(replicate(n_level_2, rep(rnorm(nlev2, 0, 1), each = n_level_2))) # gives a RE to each level 2

  ## JOIN FIXED EFFECTS TOGETHER ##
  data_sim_y <- as.data.frame(cbind(uncor_sd_true, fixed_effect_level_2))
  colnames(data_sim_y) <- c(1:ncol(data_sim_y))
  data_sim_y <- data_sim_y  %>%
    rename_with(~ paste0("true_", .))

  ## ADD RANDOM EFFECTS
  rand_eff <- rnorm(nlev2, 0, sd_level_2)

  rand_eff_rows <- rep(rand_eff, each = n_level_2) # gives a RE to each level 2

  #### NOW CALCULATE THE Y VARIABLES FROM A MODEL DEPENDENT ONLY ON THIS SET OF 10 VARIABLES AND WITH SOME RANDOM ERROR ADDED
  data_sim_y$error <- rnorm(nrows, 0, sd_level_1) ## A VECTOR OF ERROR TERMS RANDOMLY SELECTED FROM A RANDOM DISTRIBUTION MEAN = 0 SD = 5
  data_sim_y$RE <- (rand_eff_rows)

  ### THIS CODE CALCULATES A Y VALUE FOR EACH ROW FROM OUR 10 X VARIABLES WITH AN INTERCEPT = 1 AND TH EADDITION OF THE RANDON ERROR TERM
  ## SO THIS EFFECTIVELY PREDICTS THE Y FROM A MODEL USING OUR 10 VARIABLES AND A BETA=2 FOR EACH VARIABLE

  data_sim_y$y_out <- 0 + (data_sim_y$true_1 * amplitude) + (data_sim_y$true_2 * amplitude) + data_sim_y$RE + data_sim_y$error

  data_sim_y <- data_sim_y %>% dplyr::select(-error, -RE)

  df_rand <- data.frame(Doubles = double())
  for (i in 1:nlev2) {
    randoms <- as.vector(rep(i, n_level_2))
    df_rand <- as.data.frame(rbind(c(df_rand, randoms)))
  }

  data_sim_y$level2 <- unlist(t(df_rand[-1]))

  ## Add noise variables
  noise_vars <- data.frame(replicate(num_noise_vars, rnorm(nrows, 0, 1)))

  data_all <- cbind(data_sim_y, noise_vars)

  suppressWarnings(mod_sim_RE_test <- lmer(y_out ~ true_1 + true_2 + (1 | level2), data = data_all[, 1:10]))

  true_out <- summary(mod_sim_RE_test)

  coefs <- as.data.frame(true_out$coefficients)
  min_x_coef_t <- min(abs(coefs$`t value`[2:nrow(coefs)]))

  sds <- as.data.frame(true_out$varcor)
  level_1_sd <- round(sds$sdcor[2], 2)
  level_2_sd <- round(sds$sdcor[1], 2)

  data_to_use <- data_all

  x_names <- data_to_use %>% dplyr::select(-y_out, -level2) ## ALTER IF DIFFERENT LEVELS INVOLVED

  return(data_to_use)
}

simulate_data(
  nrows = 10, ncols = 10, n_true = 2, amplitude = 2,
  n_level_2 = 2, sd_level_1 = 0.5, sd_level_2 = 0
)
