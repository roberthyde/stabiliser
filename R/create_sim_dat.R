# set.seed(141)

# 100 observations, 995 junk variables
# n <- 100; p <- 995

# junk_vars <- as_tibble(matrix(rnorm(n=n*p, mean = 0, sd=1), ncol = p))

# 5 causal variables correlated with outcome y (and each other)
# causal_vars <- rnorm_multi(n = 100,
#            mu = 0,
#            sd = 1,
#            r = 0.3,
#            varnames = c("y", "x1", "x2", "x3", "x4", "x5"),
#            empirical = FALSE) %>%
#  as_tibble()

# Bind causal and junk variables
# sim_dat <- causal_vars %>%
#  bind_cols(junk_vars)

# Test causal variables associated with y
# test_mod <- sim_dat %>%
#  select(y, contains("x")) %>%
#  lm(y ~ ., .)#

# broom::glance(test_mod)

# Add data to package
# use_data(sim_dat)
