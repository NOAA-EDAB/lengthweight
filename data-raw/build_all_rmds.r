#' Create the species rmds
#'

source(here::here("data-raw/make_rmd.r"))

build_all_rmds <- function() {

  lwData <- readRDS(here::here("data-raw/data/lwAllData.rds"))

  # pull out species
  speciessvspp <- lengthweight::wigley$SVSPP

  # loop through species
  df <- NULL
  df2 <- NULL

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

    # check for presence of all season coefficients anf then pad out vector with NAs to represent missing coefficients
    coefficients <- fits$seasonSlope$coefficients[1]
    if (sum(grepl("SEASON",names(fits$seasonSlope$coefficients))) != 4) {
      # missing seasonal coefficients. Find which are missing (due to lack of data)
      if ((length(fits$seasonSlope$coefficients) == 1) && is.na(fits$seasonSlope$coefficients)) {
        # only one season of data. common slope is the only fit
        season <- fits$nObs |>
          dplyr::pull(SEASON)
        coefficients <- c(rep(NA,4))
        names(coefficients) <- c("FALL","SPRING","SUMMER","WINTER")
        coefficients[season] <- fits$commonSlope$coefficients["log(LENGTH)"]
        coefficients <- c(fits$commonSlope$coefficients["(Intercept)"],coefficients)
        # add sigma
        sigma <- summary(fits$commonSlope)$sigma
      } else {

        for (aseason in c("FALL","SPRING","SUMMER","WINTER")) {
          if (any(grepl(aseason, names(fits$seasonSlope$coefficients)))) {
            coefficients <- c(coefficients,fits$seasonSlope$coefficients[grepl(aseason,names(fits$seasonSlope$coefficients))])
          } else {
            coefficients <- c(coefficients,NA)
          }
        }
        # add sigma
        sigma <- summary(fits$seasonSlope)$sigma
      }

    } else {
      coefficients <- fits$seasonSlope$coefficients
      sigma <- summary(fits$seasonSlope)$sigma
    }

    newitemSeason <- c(isvspp,speciesName,coefficients, sigma)
    df2 <- rbind(df2,newitemSeason)

    message(speciesName$COMNAME)

    make_rmd(speciesName$COMNAME)
  }

  params <- as.data.frame(df)
  rownames(params) <- NULL
  names(params) <- c("SVSPP","Common Name", "Scientific Name","ln(alpha)","beta","sigma")
  saveRDS(params,here::here("data-raw/data/params.rds"))
  # Seasonal parameters
  paramsSeason <- as.data.frame(df2)
  rownames(paramsSeason) <- NULL
  names(paramsSeason) <- c("SVSPP","Common Name", "Scientific Name","ln(alpha)","beta-Fall","beta-Spring","beta-Summer","beta-Winter","sigma")
  saveRDS(paramsSeason,here::here("data-raw/data/paramsSeason.rds"))


  return(params)

}



