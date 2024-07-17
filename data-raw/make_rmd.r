#' Create markdown file using parsed github issue
#'
#' Template structure for the rmd is hard coded in this function.
#' The template used data from github parsed github submission issue
#'
#' @param lengthWewightData dataframe. pulled length weight data for species
#'
#' @return creates an rmd in chapters folder. The name of the rmd is the indicator name

make_rmd <- function(speciesName){

  filename <- gsub("\\s+","",speciesName)

  # create rmd with name of indicator
  con <- file(here::here("vignettes",paste0(filename,".rmd")),open="w")

  # start to create the Rmd
  ### yml

  cat(paste0("---"),append=T,fill=T,file=con)
  cat(paste0("title: \"",speciesName,"\""),append=T,fill=T,file=con)
  cat(paste0("output: rmarkdown::html_vignette"),append=T,fill=T,file=con)
  cat(paste0("vignette: >"),append=T,fill=T,file=con)
  cat(paste0("  %\\VignetteIndexEntry{",speciesName,"}"),append=T,fill=T,file=con)
  cat(paste0("  %\\VignetteEngine{knitr::rmarkdown}"),append=T,fill=T,file=con)
  cat(paste0("  %\\VignetteEncoding{UTF-8}"),append=T,fill=T,file=con)
  cat(paste0("---"),append=T,fill=T,file=con)



  #
  # cat(paste0("# ",speciesName," {#",filename,"}"),append=T,fill=T,file=con)
   cat("",append=T,fill=T,file=con) # add space

  # header chunk script
  cat("```{r echo=FALSE}",append=T,fill=T,file=con)
  cat("knitr::opts_chunk$set(echo = F,message=F, warning=F)",append=T,fill=T,file=con)
  cat("library(lengthweight)",append=T,fill=T,file=con)
  cat("library(DT)",append=T,fill=T,file=con)
  #cat(paste0("fname <- 'data-raw/data/lw",filename,".rds'"),,append=T,fill=T,file=con)
  cat(paste0("lengthWeightData <- readRDS(here::here('data-raw/data/lw",filename,".rds'))"),append=T,fill=T,file=con)
  saveRDS(lwSpecies,here::here(paste0("data-raw/data/lw",filename,".rds")))
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  # fit chunk script
  cat("```{r fit,echo=FALSE}",append=T,fill=T,file=con)
  cat(paste0("fits <- fit_length_weight(lengthWeightData,speciesName='",speciesName,"',sex=NULL,season=c('FALL','SPRING'))"),append=T,fill=T,file=con) # add space
  cat("print(fits$plot)",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  # Obs header
  cat("## Number of observations",append=T,fill=T,file=con)

  cat("```{r nobs,echo=FALSE}",append=T,fill=T,file=con)
  cat("DT::datatable(fits$nObs)",append=T,fill=T,file=con) # add space
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  # Header for common slope
  cat("## Common Slope",append=T,fill=T,file=con)

  cat("```{r common,echo=FALSE}",append=T,fill=T,file=con)
  cat("equatiomatic::extract_eq(fits$commonSlope)",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)

  cat("```{r commonparams,echo=FALSE}",append=T,fill=T,file=con)
  cat("estimates <- as.data.frame(fits$commonSlope$coefficients)",append=T,fill=T,file=con) # add space
  cat("names(estimates) <- 'Estimates'",append=T,fill=T,file=con) # add space
  cat("residualVariance <- summary(fits$commonSlope)$sigma**2",append=T,fill=T,file=con) # add space
  cat("estimates <- rbind(estimates,residualVariance)",append=T,fill=T,file=con) # add space
  cat("ind <- nrow(estimates)",append=T,fill=T,file=con) # add space
  cat("rownames(estimates)[ind] <- 'residual variance'",append=T,fill=T,file=con) # add space
  cat("DT::datatable(estimates)",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)


  cat("",append=T,fill=T,file=con)

  # Header for seasonal slope
  cat("## Seasonal Slope",append=T,fill=T,file=con)

  cat("```{r season,echo=FALSE}",append=T,fill=T,file=con)
  cat("equatiomatic::extract_eq(fits$seasonSlope)",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  cat("```{r seasonalparams,echo=FALSE}",append=T,fill=T,file=con)
  cat("estimates <- as.data.frame(fits$seasonSlope$coefficients)",append=T,fill=T,file=con) # add space
  cat("names(estimates) <- 'Estimates'",append=T,fill=T,file=con) # add space
  cat("residualVariance <- summary(fits$seasonSlope)$sigma**2",append=T,fill=T,file=con) # add space
  cat("estimates <- rbind(estimates,residualVariance)",append=T,fill=T,file=con) # add space
  cat("ind <- nrow(estimates)",append=T,fill=T,file=con) # add space
  cat("rownames(estimates)[ind] <- 'residual variance'",append=T,fill=T,file=con) # add space

  cat("DT::datatable(estimates)",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)


  cat("",append=T,fill=T,file=con)


  # table of parameter and pvalue

  cat("```{r pval,echo=FALSE}",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)



  close(con)
}
