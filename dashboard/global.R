library(shiny)

library(here)
library(tidyverse)
library(sf)

library(mapdeck)
library(geomtextpath)
library(ggiraph)

# Load modules
source('modules/smryplot_co2_emissions.R')

sf_states <- readr::read_rds(here('data/states-shp-file.rds'))
# state_pop <- readr::read_rds(here('data/state-population.rds'))
co2_emissions <- readr::read_rds(here('data/co2_emissions.rds'))
