

simulate_lr_re_data <- function(
    n_subjects = 100,
    obs_per_subject = 10,
    n_signal = 2,            # number of causal predictors
    n_noise  = 3,            # number of junk predictors
    beta0    = -1,           # intercept
    beta_signal = NULL,      # optional: effects for causal predictors
    sigma_u  = 1            # SD for random intercepts
){

  # Basic checks
  stopifnot(n_subjects >= 1, obs_per_subject >= 1,
            n_signal >= 0, n_noise >= 0)

  N <- n_subjects * obs_per_subject

  # Random intercepts per subject
  subject_id <- rep(seq_len(n_subjects), each = obs_per_subject)
  u <- rnorm(n_subjects, mean = 0, sd = sigma_u)
  rand_intercepts <- rep(u, each = obs_per_subject)

  # Generate causal predictors x1..x{n_signal}
  X_signal <- NULL
  if (n_signal > 0) {
    X_signal <- replicate(n_signal, rnorm(N), simplify = TRUE)
    colnames(X_signal) <- paste0("x", seq_len(n_signal))
    # If user doesn't provide betas, make simple defaults
    if (is.null(beta_signal)) {
      beta_signal <- rep(0.6, n_signal)
    }
  } else {
    beta_signal <- numeric(0)
  }

  # Generate junk predictors junk1..junk{n_noise}
  X_noise <- NULL
  if (n_noise > 0) {
    X_noise <- replicate(n_noise, rnorm(N), simplify = TRUE)
    colnames(X_noise) <- paste0("junk", seq_len(n_noise))
  }

  # Linear predictor: intercept + random intercept + causal effects
  eta <- beta0 + rand_intercepts
  if (!is.null(X_signal)) {
    eta <- eta + as.vector(X_signal %*% beta_signal)
  }

  # Outcome
  p <- 1 / (1 + exp(-eta))
  y <- rbinom(N, size = 1, prob = p)

  # Bind into a data frame
  df <- data.frame(
    subject_id = factor(subject_id),
    y = y
  )

  df <- cbind(df, as.data.frame(X_noise))
  df <- cbind(df, as.data.frame(X_signal))
  return(df)
}
