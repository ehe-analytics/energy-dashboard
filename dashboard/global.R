library(shiny)

library(here)
library(tidyverse)
library(sf)

library(mapdeck)
library(geomtextpath)
library(ggiraph)

sf_states <- readr::read_rds(here('data/states-shp-file.rds'))
state_pop <- readr::read_rds(here('data/state-population.rds'))
all_data <- readr::read_rds(here('data/co2_emissions.rds'))


