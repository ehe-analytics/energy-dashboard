library(shiny)

library(here)
library(tidyverse)
library(sf)

library(mapdeck)
library(geomtextpath)
library(ggiraph)

# Load modules
source('modules/smryplot_co2_emissions.R')
source('modules/smryplot_production.R')

sf_states <- read_rds(here('data/states-shp-file.rds'))

production <- read_rds(here('data/energy_production.rds')) %>% filter(units == 'Billion Btu')
co2_emissions <- read_rds(here('data/co2_emissions.rds'))
