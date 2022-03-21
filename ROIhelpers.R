## ROI Calculator for cleaning time in hospitals
## This script sets up everything needed for the calculator function


## Packages

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(ggforce)
library(shiny)
library(readr)
library(bslib)
library(stringr)

## Function for the Shiny app to use


ROI_calculator <- function(V1 = 12313, PRnorm = 11, PRintv = 4.6, Time = 15, Hospital_rent = 2502, Housekeeper_wage = 12.89, 
                           Cleaning_standard = 0.38, Cleaning_enhanced = 0.81, Device_cost = 4500, N_enhanced_peryear = 52){
  ## Inputs
  ## Incremental probability reduction of an infection
  PRinc <- (PRnorm - PRintv)/100
  PRinc_print <- PRnorm - PRintv
  
  ## Incremental cost for hospital rent
  F1inc <- Hospital_rent/(1440/Time)
  
  ## Incremental housekeeping cost
  F2inc <- Housekeeper_wage/(60/Time)
  
  ## Incremental cleaning costs
  F3inc <- Cleaning_standard + Cleaning_enhanced
  
  ## Cost of T360 per usage
  F4inc <- Device_cost/N_enhanced_peryear
  
  ## Calculation
  Ob <- trunc((V1*PRinc) - (F1inc + F2inc + F3inc + F4inc))*N_enhanced_peryear
  
  
  ## Outputs
  Ob_output <- "Estimate Cost Savings Per Year of Adding Enhanced Cleaning and Disinfection."# $", Ob)
  PRinc_output <- "Probability risk reduction of in-room transmission." # , PRinc_print, "%")
  
  result <- as.list(c(Ob_output, PRinc_output,prettyNum(Ob ,scientific = FALSE, big.mark = ","),PRinc_print))
  
  return(result)
  
  
}

ROI_calculator()


customInfoBox <- function(title, value = NULL, subtitle = NULL,
                    icon = shiny::icon("bar-chart"), color = "aqua", width = 4, href = NULL,
                    fill = FALSE) {
  
#  validateColor(color)
  shinydashboard:::tagAssert(icon, type = "i")
  
  colorClass <- paste0("bg-", color)
  
  boxContent <- div(
    class = "info-box",
    class = if (fill) colorClass,
    span(
      class = "info-box-icon",
      class = if (!fill) colorClass,
      icon
    ),
    div(class = "info-box-content",
        span(class = "info-box-text", title),
        if (!is.null(value)) span(class = "info-box-number", value),
        if (!is.null(subtitle)) p(subtitle)
    )
  )
  
  if (!is.null(href))
    boxContent <- a(href = href, boxContent)
  
  div(class = if (!is.null(width)) paste0("col-sm-", width),
      boxContent
  )
}