library(tidyverse)

# Downloaded data from EIA
sids_df <- read_csv('data/series_ids.csv')
eia_data <- read_csv('data/downloaded_eia_data.csv')

# Shape files for states
sf_states <- tigris::states(cb = T) %>% 
  select(geoid = GEOID, state_abb = STUSPS)
write_rds(sf_states, 'data/states-shp-file.rds')

# State population
state_pop <- tidycensus::get_estimates(
  'state', 'population', year = 2019, state = unique(sf_states$state_abb)
  ) %>% 
  filter(variable == 'POP') %>% 
  select(geoid = GEOID, population = value)
write_rds(state_pop, 'data/state-population.rds')

# CO2 emissions by fuel type
co2_emissions <- eia_data %>% 
  filter(series_id %in% (sids_df %>% filter(category_id %in% c(2251664:2251669)) %>% pull(series_id))) %>% 
  separate(name, c('series', 'category', 'state'), sep = ', ') %>% 
  mutate(data = 'CO2 emissions', 
         category = str_to_sentence(category), 
         series = str_replace(series, ' carbon dioxide emissions', ''), 
         state_abb = str_replace(iso3166, 'USA-', '')) %>% 
  select(-iso3166, -geography, -copyright) 
write_rds(co2_emissions, 'data/co2_emissions.rds')


