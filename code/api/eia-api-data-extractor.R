library(rvest)
library(tidyverse)
source('code/api/functions.R')


# Previously downloaded data
old_data <- read_csv('data/downloaded_eia_data.csv', col_types = cols(.default = 'c'))

# CO2 emissions for all fuel types 2251664 to 2251669
# CO2 emissions by coal (not including child categories): 2251607 to 2251617 (increment of 2)
# CO2 emissions by nat gas (not including child categories): 2251619 to 2251629 (increment of 2)
# CO2 emissions by petroleum (not including child categories): 2251631, 2251637, 2251641, 2251650, 2251654, 2251662
cats_to_dl <- c(seq(2251607, 2251617, by = 2), 
                seq(2251619, 2251629, by = 2), 
                c(2251631, 2251637, 2251641, 2251650, 2251654, 2251662), 
                2251664:2251669)
new_sids <- cats_to_dl %>% map(dl_sids) %>% bind_rows()

sids_to_update <- new_updates(old_data %>% dplyr::select(series_id, updated), new_sids)
sids_to_dl <- c(sids_to_update, setdiff(new_sids$series_id, old_data$series_id))
# sids_to_keep <- setdiff(old_data$series_id, sids_to_update)

(logfile <- paste0('logs/eia-api-dl-', Sys.Date(), '.txt'))
log <- file(logfile, open = 'wt')
sink(log, type = 'message')
new_eia_data <- list()
# A for loop is preferable to map in this rare case because even if there is an error 
# the data downloaded will be saved up to the point of error. 
# map on the other hand discards all previous iterations in the event of an error
for (i in 1:length(sids_to_dl)) { 
  new_eia_data[[i]] <- get_eia_data(sids_to_dl[i])
  # After every 50 downloads, sleep for 30 seconds to prevent overloading of 
  # EIA API and potentially getting bounced off
  if (i%%50 == 0) {
    Sys.sleep(30)
    # closeAllConnections()
    # gc()
  }
}
sink()
closeAllConnections()


new_eia_data <- bind_rows(new_eia_data) %>% select(-description)

# remove rows from the old database for which new data were downloaded
# and append the new data
eia_data <- old_data %>%
  filter(!series_id %in% sids_to_dl) %>% 
  bind_rows(new_eia_data)

write_csv(eia_data, 'data/downloaded_eia_data.csv')
