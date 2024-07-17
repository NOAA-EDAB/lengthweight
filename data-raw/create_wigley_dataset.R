#' Create Wigley et al species list
#'
#'


wigley <- readr::read_csv(here::here("data-raw/data/LW_species.csv"))

usethis::use_data(wigley)
