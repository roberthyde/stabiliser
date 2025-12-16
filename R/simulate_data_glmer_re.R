
set.seed(123)

# Parameters
n_subjects <- 100
obs_per_subject <- 10
beta0 <- -1
beta1 <- 0.8   # true effect for x1
beta2 <- -0.5  # true effect for x2
sigma_u <- 1   # random intercept SD

# Subject IDs
subject_id <- rep(1:n_subjects, each = obs_per_subject)

# Random intercepts
u <- rnorm(n_subjects, mean = 0, sd = sigma_u)
rand_intercepts <- rep(u, each = obs_per_subject)

# True predictors
x1 <- rnorm(n_subjects * obs_per_subject)
x2 <- rnorm(n_subjects * obs_per_subject)

# Junk predictors (no effect)
junk1 <- rnorm(n_subjects * obs_per_subject)
junk2 <- rnorm(n_subjects * obs_per_subject)
junk3 <- rnorm(n_subjects * obs_per_subject)

# Linear predictor (only x1 and x2 matter)
eta <- beta0 + beta1 * x1 + beta2 * x2 + rand_intercepts

# Probability and outcome
p <- 1 / (1 + exp(-eta))
y <- rbinom(n_subjects * obs_per_subject, size = 1, prob = p)

# Combine into data frame
synthetic_data <- data.frame(
  subject_id = factor(subject_id),
  x1 = x1,
  x2 = x2,
  junk1 = junk1,
  junk2 = junk2,
  junk3 = junk3,
  y = y
)


