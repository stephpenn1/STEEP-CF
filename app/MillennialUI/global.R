
library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyverse)
library(deSolve)
library(plotly)

source("derivs_V2_MM.R")
source("run_functions.R")

# Read in parameters - Described in Table A1 of Abramoff et al. (2021)
parameters.file <- read.table("soilpara_in_fit.txt") %>%
  add_row(V1 = "param_pc", V2 = 0.86) # quick fix, add back in param_pc since were using the example file
parameters <- as.list(parameters.file$V2)
names(parameters) <- parameters.file$V1

pools <- c("AGG" = "Aggregate~C~(gC~m^-2)",
           "CO2_emissions" = "CO2~emissions~(gC~m^-2~day^-1)",
           "MIC" = "Microbial~biomass~(gC~m^-2)",
           "LMWC" = "Low~molecular~weight~C~(gC~m^-2)",
           "POM" = "Particulate~organic~matter~(gC~m^-2)",
           "MAOM" = "Mineral~associated~organic~matter~(gC~m^-2)")

force_names <- c("forc_npp" = "Net~Primary~Production~(gC~m^2)",
                 "forc_st" = "Soil~Temperature~'(°C)'",
                 "forc_sw" = "Soil~Moisture~(m^3~m^-3)")
