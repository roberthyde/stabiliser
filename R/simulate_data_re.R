#' simulate_data_re
#'
#' @name simulate_data_re
#'
#' @description Simulate a 500x500 dataset with 8 true fixed effects, 492 junk variables and a clustered outcome suitable for a 2 level random effects analysis. The strength of association between true variables and the outcome is governed by the error added at level 1 (defined by parameter sd_level_1) and level 2 (sd_level_2).
#'
#' @param sd_level_1 Standard deviation of level 1 variables
#' @param sd_level_2 Standard deviation of level 2 variables
#'
#' @return A simulated dataset
#'
#' @import dplyr
#' @importFrom stats rnorm
#'
#' @export
#'

utils::globalVariables(c("error", "RE", "level2"))

simulate_data_re <- function(sd_level_1 = 2, sd_level_2 = 2) {

  if (sd_level_1 < 0.5 | sd_level_1 > 6 | sd_level_2 < 0.5 | sd_level_2 > 6){
    stop("For data simulation with random effects, both sd_level_1 and sd_level_2 must be set between 0.5-6.")
  }

  # TODO: User editable function
  nrows = 500
  ncols = 500
  n_true = 8
  amplitude = 2
  n_level_2 = 5

  nlev2 <- nrows / n_level_2
  num_noise_vars = nrows - n_true

  # Fixed effects at level 1 and 2
  outcome <-  data.frame(replicate(n_true / 2, rnorm(nrows, 0, 1)),
                      replicate(n_true / 2, rep(rnorm(nlev2, 0, 1), each = n_level_2)))

  colnames(outcome) <- c(1:ncol(outcome))
  outcome <- outcome  %>%
    rename_with(~ paste0("true_", .))

  ## ADD RANDOM EFFECTS
  rand_eff <- rnorm(nlev2, 0, sd_level_2)

  rand_eff_rows <- rep(rand_eff, each = n_level_2) # gives a RE to each level 2

  #### NOW CALCULATE THE Y VARIABLES FROM A MODEL DEPENDENT ONLY ON THIS SET OF 10 VARIABLES AND WITH SOME RANDOM ERROR ADDED
  outcome$error <- rnorm(nrows, 0, sd_level_1) ## A VECTOR OF ERROR TERMS RANDOMLY SELECTED FROM A RANDOM DISTRIBUTION MEAN = 0 SD = 5
  outcome$RE <- (rand_eff_rows)

  ### THIS CODE CALCULATES A Y VALUE FOR EACH ROW FROM OUR 10 X VARIABLES WITH AN INTERCEPT = 1 AND TH EADDITION OF THE RANDON ERROR TERM
  ## SO THIS EFFECTIVELY PREDICTS THE Y FROM A MODEL USING OUR 10 VARIABLES AND A BETA=2 FOR EACH VARIABLE
  # TODO: n_true == N
  outcome$outcome <- 0 +
    (outcome$true_1 * amplitude) +
    (outcome$true_2 * amplitude) +
    (outcome$true_3 * amplitude) +
    (outcome$true_4 * amplitude) +
    (outcome$true_5 * amplitude) +
    (outcome$true_6 * amplitude) +
    (outcome$true_7 * amplitude) +
    (outcome$true_8 * amplitude) +
    outcome$RE +
    outcome$error

  outcome <- outcome %>%
    as_tibble() %>%
    select(-error, -RE)

  df_rand <- data.frame(Doubles = double())
  for (i in 1:nlev2) {
    randoms <- as.vector(rep(i, n_level_2))
    df_rand <- as.data.frame(rbind(c(df_rand, randoms)))
  }

  outcome$level2 <- unlist(t(df_rand[-1]))

  outcome <- outcome %>%
    select(outcome, level2, everything())

  ## Add noise variables
  noise_vars <- replicate(num_noise_vars, rnorm(nrows, 0, 1)) %>%
    as.data.frame() %>%
    as_tibble() %>%
    rename_with(~ paste0("junk_", .x))

  out <- outcome %>%
    bind_cols(noise_vars)

  return(out)
}
