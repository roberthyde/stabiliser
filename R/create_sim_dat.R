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

# Creating stabiliser_example
# stabiliser_example <- read_csv("D0B_4_TRUE_corr_10_sep_each_at_0.3_R26.csv") %>%
#  rename(y = y_out) %>%
#  select(1:104) %>%
#  select(-contains("X_out"), X_out5, X_out8, X_out4, X_out1) %>%
#  head(50) %>%
#  rename(causal1 = X_out5,
#         causal2 = X_out8,
#         causal3 = X_out4,
#         causal4 = X_out1)

# old_names <- stabiliser_example %>%
#  select(contains("XX")) %>%
#  colnames()

# new_names <- map_chr(.x = 1:95, .f = ~paste0("junk", .x))

# stabiliser_example <- stabiliser_example %>%
#  rename_at(.vars = old_names, .funs = ~ new_names) %>%
#  select(y, contains("causal"), everything())
# usethis::use_data(stabiliser_example)
