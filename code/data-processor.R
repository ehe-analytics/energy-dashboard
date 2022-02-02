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
# state_pop <- tidycensus::get_estimates(
#   'state', 'population', year = 2019, state = unique(sf_states$state_abb)
#   ) %>%
#   filter(variable == 'POP') %>% 
#   select(state = NAME, geoid = GEOID, population = value) %>% 
#   left_join(sf::st_drop_geometry(sf_states), by = 'geoid')
# write_csv(state_pop, 'data/state-population.csv')

# State population
sf_states <- read_rds('data/states-shp-file.rds')
state_pop <- eia_data %>% 
  filter(str_detect(series_id, 'SEDS.TPOPP')) %>% 
  separate(name, c('series', 'state'), sep = ', ') %>% 
  mutate(population = value*1000, 
         state_abb = str_replace(iso3166, 'USA-', '')) %>% 
  select(state_abb, state, period, population) %>% 
  left_join(sf::st_drop_geometry(sf_states), by = 'state_abb')
write_csv(state_pop, 'data/state-population.csv')

# State GDP
state_pop <- read_csv('data/state-population.csv')
gdp <- eia_data %>% 
  filter(str_detect(series_id, 'SEDS.GDP')) %>% 
  separate(name, c('series', 'state'), sep = ', ') %>% 
  mutate(state_abb = str_replace(iso3166, 'USA-', ''),
         series = str_squish(str_replace(series, 'gross domestic product', ''))) %>% 
  left_join(state_pop %>% select(-state), by = c('state_abb', 'period')) %>% 
  mutate(per_capita = value*1e3/population, 
         per_capita_units = 'Thousand dollars') %>% 
  select(-iso3166, -geography, -copyright, -start, -end, -updated) 
write_rds(gdp, 'data/state-gdp.rds')


# CO2 emissions by fuel type (all fuels, coal, nat gas, petroleum) 
# and sector (commercial, electric, industrial, residential, transportation, total from all sectors)
co2_emissions <- eia_data %>% 
  filter(str_detect(series_id, 'EMISS.CO2')) %>% 
  separate(name, c('series', 'category', 'state'), sep = ', ') %>% 
  mutate(data_name = 'CO2 emissions', 
         category = str_to_sentence(category), 
         series = str_replace(series, ' carbon dioxide emissions', ''), 
         state_abb = str_replace(iso3166, 'USA-', ''), 
         state_abb = ifelse(state_abb == 'US', 'USA', state_abb)) %>% 
  left_join(state_pop %>% select(-state), by = c('state_abb', 'period')) %>% 
  mutate(per_capita = value*1e6/population, 
         per_capita_units = 'metric ton') %>% 
  mutate_at(vars(units, unitsshort), function (x) str_squish(str_replace_all(x, ' CO2', ''))) %>% 
  select(-iso3166, -geography, -copyright, -start, -end, -updated) 
write_rds(co2_emissions, 'data/co2_emissions.rds')


# Energy production
state_pop <- read_csv('data/state-population.csv')
eids <- c("BDFDB", "BDPRP", "BFFDB", "BFPRP", "CLPRB", "EMFDB", "ENPRP", "NCPRB", "NGMPB", "NUETB", "PAPRB", "REPRB", "TEPRB", "WWPRB")

production <- eia_data %>% 
  filter(str_detect(series_id, paste0(eids, collapse = '|'))) %>% 
  separate(name, c('series', 'temp', 'state'), sep = ', ', fill = 'right') %>% 
  mutate(data_name = 'Energy production', 
         category = 'All fuels',
         state = ifelse(is.na(state), temp, state), 
         state_abb = str_replace(iso3166, 'USA-', ''), 
         state_abb = ifelse(is.na(state_abb), 'USA', state_abb)) %>% 
  select(-temp) %>% 
  left_join(state_pop %>% select(-state), by = c('state_abb', 'period')) %>% 
  mutate(
    per_capita = case_when(units == 'Billion Btu' ~ value*1e3/population,
                           units == 'Thousand barrels' ~ value*1e3/population), 
    per_capita_units = case_when(units == 'Billion Btu' ~ 'Million Btu',
                                 units == 'Thousand barrels' ~ 'Barrels'), 
    description = series, 
    series = str_to_sentence(
      str_squish(
        str_replace_all(description, 
                        'Biomass inputs \\(feedstock\\) to the production of|energy production|production|marketed|\\(including lease condensate\\)', 
                        '')))
    ) %>% 
  select(-iso3166, -geography, -copyright, -start, -end, -updated)
write_rds(production, 'data/energy_production.rds')


