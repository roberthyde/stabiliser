devtools::

devtools::install_github("roberthyde/stabiliser")

library(stabiliser)

data("sim_dat")

devtools::load_all()
library(tidyverse)
library(bigstep)
test <- stabilise(sim_dat, outcome="y", boot_reps=100, permutations=5, perm_boot_reps=20, model=model_mbic)

test$stability %>%
  filter(stability >= test$perm_thresh)
packageVersion("stabiliser")

library(devtools)
check()
document("../stabiliser")
use_testthat()
use_test("model_mbic")
use_package("tidyr")
