permute <- function(data, permute, times, repeats){
  purrr::rerun(repeats, rsample::permutations(data = data, permute = permute, times = times)) %>%
    bind_rows(.id="repeat")
}
