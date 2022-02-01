library(tidyverse)

# Downloaded data from EIA
# sids_df <- read_csv('data/series_ids.csv')
eia_data <- read_csv('data/downloaded_eia_data.csv')

# Shape files for states
sf_states <- tigris::states(cb = T) %>%
  select(geoid = GEOID, state_abb = STUSPS) %>% 
  rmapshaper::ms_simplify()
# sf::st_write(sf_states, 'data/shp-files/states-shp-file.shp', delete_layer = F)
write_rds(sf_states, 'data/states-shp-file.rds')

# State population
state_pop <- tidycensus::get_estimates(
  'state', 'population', year = 2019, state = unique(sf_states$state_abb)
  ) %>%
  filter(variable == 'POP') %>% 
  select(state = NAME, geoid = GEOID, population = value) %>% 
  left_join(sf::st_drop_geometry(sf_states), by = 'geoid')
write_csv(state_pop, 'data/state-population.csv')


# CO2 emissions by fuel type (all fuels, coal, nat gas, petroleum) 
# and sector (commercial, electric, industrial, residential, transportation, total from all sectors)
state_pop <- read_csv('data/state-population.csv')
co2_emissions <- eia_data %>% 
  filter(str_detect(series_id, 'EMISS.CO2')) %>% 
  separate(name, c('series', 'category', 'state'), sep = ', ') %>% 
  mutate(data_name = 'CO2 emissions', 
         category = str_to_sentence(category), 
         series = str_replace(series, ' carbon dioxide emissions', ''), 
         state_abb = str_replace(iso3166, 'USA-', '')) %>% 
  left_join(state_pop %>% select(-state), by = 'state_abb') %>% 
  mutate(population = ifelse(is.na(population), sum(state_pop$population), population), 
         per_capita = value*1e6/population, 
         per_capita_units = 'metric ton CO2') %>% 
  select(-iso3166, -geography, -copyright) 
write_rds(co2_emissions, 'data/co2_emissions.rds')


# Energy production
state_pop <- read_csv('data/state-population.csv')
eids <- c("BDFDB", "BDPRP", "BFFDB", "BFPRP", "CLPRB", "EMFDB", "ENPRP", "NCPRB", "NGMPB", "NUETB", "PAPRB", "REPRB", "TEPRB", "WWPRB")

production <- eia_data %>% 
  filter(str_detect(series_id, paste0(eids, collapse = '|'))) %>% 
  separate(name, c('series', 'category', 'state'), sep = ', ') %>% 
  mutate(data_name = 'Energy production', 
         state = ifelse(is.na(state), category, state)) %>% 
  select(-category) %>% 
  left_join(state_pop, by = 'state') %>% 
  mutate(
    population = ifelse(is.na(population), sum(state_pop$population), population), 
    per_capita = case_when(units == 'Billion Btu' ~ value/population,
                           units == 'Thousand barrels' ~ value/population), 
    per_capita_units = case_when(units == 'Billion Btu' ~ 'Billion Btu',
                                 units == 'Thousand barrels' ~ 'Thousand barrels'), 
    description = series, 
    series = str_to_sentence(
      str_squish(
        str_replace_all(description, 
                        'Biomass inputs \\(feedstock\\) to the production of|energy production|production|marketed|\\(including lease condensate\\)', 
                        '')))
    ) %>% 
  select(-iso3166, -geography, -copyright)
write_rds(production, 'data/energy_production.rds')
