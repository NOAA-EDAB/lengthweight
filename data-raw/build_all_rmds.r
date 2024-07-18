#' Create the species rmds
#'

source(here::here("data-raw/make_rmd.r"))

build_all_rmds <- function() {

  lwData <- readRDS(here::here("data-raw/data/lwAllData.rds"))

  # pull out species
  speciessvspp <- lengthweight::wigley$SVSPP

  # loop through species
  df <- NULL
  for(isvspp in speciessvspp) {
    lwSpecies <- lwData  |>
      dplyr::filter(SVSPP == isvspp)


    speciesName <- lwSpecies |>
      dplyr::select(COMNAME,SCINAME) |>
      dplyr::distinct()

    filename <- gsub("\\s+","",speciesName$COMNAME)
    saveRDS(lwSpecies,here::here(paste0("data-raw/data/lw",filename,".rds")))

    fits <- lengthweight::fit_length_weight(lwSpecies,speciesName=speciesName$COMNAME,sex=NULL,season=NULL)
    newitem <- c(isvspp,speciesName,fits$commonSlope$coefficients, summary(fits$commonSlope)$sigma)
    df <- rbind(df,newitem)

    message(speciesName$COMNAME)

    make_rmd(speciesName$COMNAME)
  }

  params <- as.data.frame(df)
  rownames(params) <- NULL
  names(params) <- c("SVSPP","Common Name", "Scientific Name","ln(alpha)","beta","sigma")
  saveRDS(params,here::here("data-raw/data/params.rds"))
  return(params)

}



