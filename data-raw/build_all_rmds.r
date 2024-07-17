#' Create the species rmds
#'

source(here::here("data-raw/make_rmd.r"))
lwData <- readRDS(here::here("data-raw/data/lwAllData.rds"))

build_all_rmds <- function(lwData) {
  # pull out species
  speciessvspp <- lengthweight::wigley$SVSPP

  # loop through spcies
  for(isvspp in speciessvspp) {
    lwSpecies <- lwData  |>
      dplyr::filter(SVSPP == isvspp)


    speciesName <- lwSpecies |>
      dplyr::select(COMNAME) |>
      dplyr::distinct() |>
      dplyr::pull()

    filename <- gsub("\\s+","",speciesName)
    saveRDS(lwSpecies,here::here(paste0("data-raw/data/lw",filename,".rds")))


    #fits <- lengthweight::fit_length_weight(lwSpecies,speciesName=speciesName,sex=NULL,season=c('FALL','SPRING'))

    message(speciesName)

    make_rmd(speciesName)
  }

}



