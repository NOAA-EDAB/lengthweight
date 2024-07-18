#'
#'
#'
#'
#'
#'

# source files
read <- F
source(here::here("data-raw/make_rmd.r"))
source(here::here("data-raw/build_all_rmds.r"))

run_this <- function(channel) {

  # get data and save as rds
  get_all_lengthweight_data(channel)

  # build all rmds and writes species data
  params <- build_all_rmds()

  # build book
  bookdown::render_book()

}
