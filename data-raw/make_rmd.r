#' Create markdown file using parsed github issue
#'
#' Template structure for the rmd is hard coded in this function.
#' The template used data from github parsed github submission issue
#'
#' @param speciesName Character. Common name of the species
#'
#' @return creates an rmd in chapters folder. The name of the rmd is the indicator name

make_rmd <- function(speciesName){

  filename <- gsub("\\s+","",speciesName)

  # check to see if chapters folder exists
  if(!dir.exists(here::here("chapters"))){
    dir.create(here::here("chapters"))
  }

  # create rmd with name of indicator
  con <- file(here::here("chapters",paste0(filename,".rmd")),open="w")

  # start to create the Rmd
  ### yml

  # cat(paste0("---"),append=T,fill=T,file=con)
  # cat(paste0("title: \"",speciesName,"\""),append=T,fill=T,file=con)
  # cat(paste0("output: rmarkdown::html_vignette"),append=T,fill=T,file=con)
  # cat(paste0("vignette: >"),append=T,fill=T,file=con)
  # cat(paste0("  %\\VignetteIndexEntry{",speciesName,"}"),append=T,fill=T,file=con)
  # cat(paste0("  %\\VignetteEngine{knitr::rmarkdown}"),append=T,fill=T,file=con)
  # cat(paste0("  %\\VignetteEncoding{UTF-8}"),append=T,fill=T,file=con)
  # cat(paste0("---"),append=T,fill=T,file=con)

  cat(paste0("# ",speciesName, "{#",filename,"}"),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space



  # header chunk script
  cat("```{r ",paste0("readdata",filename),",echo=FALSE}",append=T,fill=T,file=con)
  #cat("knitr::opts_chunk$set(echo = F,message=F, warning=F)",append=T,fill=T,file=con)
  #cat("library(lengthweight)",append=T,fill=T,file=con)
  #cat("library(DT)",append=T,fill=T,file=con)
  #cat(paste0("fname <- 'data-raw/data/lw",filename,".rds'"),,append=T,fill=T,file=con)
  cat(paste0("lengthWeightData <- readRDS(here::here('data-raw/data/lw",filename,".rds'))"),append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  # fit chunk script
#  cat("```{r ",paste0("fit",filename),", echo=FALSE, fig.cap='Red solid line = Common fit, Blue solid line = Seasonal fit'}",append=T,fill=T,file=con)
  cat("```{r ",paste0("fit",filename),", echo=FALSE}",append=T,fill=T,file=con)
  cat(paste0("fits <- fit_length_weight(lengthWeightData,speciesName='",speciesName,"',sex=NULL,season=NULL)"),append=T,fill=T,file=con) # add space
  cat("print(fits$plot)",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  # Obs header
  cat("## Number of observations",append=T,fill=T,file=con)

  cat("```{r ",paste0("nobs",filename),",echo=FALSE}",append=T,fill=T,file=con)
  cat("DT::datatable(fits$nObs)",append=T,fill=T,file=con) # add space
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  # Header for common slope
  cat("## Common Slope",append=T,fill=T,file=con)

  cat("```{r ",paste0("common",filename),",echo=FALSE}",append=T,fill=T,file=con)
  cat("equatiomatic::extract_eq(fits$commonSlope)",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)

  cat("```{r ",paste0("commonparams",filename),",echo=FALSE}",append=T,fill=T,file=con)
  cat("estimates <- as.data.frame(fits$commonSlope$coefficients)",append=T,fill=T,file=con) # add space
  cat("names(estimates) <- 'Estimates'",append=T,fill=T,file=con) # add space
  cat("sd <- summary(fits$commonSlope)$sigma",append=T,fill=T,file=con) # add space
  cat("estimates <- rbind(estimates,sd)",append=T,fill=T,file=con) # add space
  cat("ind <- nrow(estimates)",append=T,fill=T,file=con) # add space
  cat("rownames(estimates)[ind] <- 'sigma'",append=T,fill=T,file=con) # add space
  cat("DT::datatable(estimates)",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)


  cat("",append=T,fill=T,file=con)

  # Header for seasonal slope
  cat("## Seasonal Slope",append=T,fill=T,file=con)

  cat("```{r ",paste0("season",filename),",echo=FALSE}",append=T,fill=T,file=con)
  cat("#Trying find a way to automate the display of nested equations",append=T,fill=T,file=con) # add space
  #cat("equatiomatic::extract_eq(fits$seasonSlope)",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)

  cat("```{r ",paste0("seasonparams",filename),",echo=FALSE}",append=T,fill=T,file=con)
  cat("estimates <- as.data.frame(fits$seasonSlope$coefficients)",append=T,fill=T,file=con) # add space
  cat("if(!all(is.na(estimates))) {",append=T,fill=T,file=con)
  cat(" names(estimates) <- 'Estimates'",append=T,fill=T,file=con) # add space
  cat(" sd <- summary(fits$seasonSlope)$sigma",append=T,fill=T,file=con) # add space
  cat(" estimates <- rbind(estimates,sd)",append=T,fill=T,file=con) # add space
  cat(" ind <- nrow(estimates)",append=T,fill=T,file=con) # add space
  cat(" rownames(estimates)[ind] <- 'sigma'",append=T,fill=T,file=con) # add space
  cat("DT::datatable(estimates)",append=T,fill=T,file=con) # add space
  cat("}",append=T,fill=T,file=con) # add space
  cat("",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)


  cat("",append=T,fill=T,file=con)


  # table of parameter and pvalue
  # Header for seasonal slope
#   cat("## Hypothesis tests",append=T,fill=T,file=con)
#   cat("",append=T,fill=T,file=con)
#   cat("We can test the Null hypothesis, $H_{0}:\\beta_{j}=\\beta_{1}$\n",append=T,fill=T,file=con)
#   cat("This tests the significance of a seasonal relationship (in contrast to a common relationship, regardless of season)
# ",append=T,fill=T,file=con)
#   cat(paste0("The p-value = ",round(fits$pval,digits = 3)),append=T,fill=T,file=con)

  cat("",append=T,fill=T,file=con)



  close(con)
}
