library(rvest)
library(tidyverse)
source('code/api/functions.R')

main <- function() { 
  args <- commandArgs(trailingOnly = TRUE)
  
  # data frame that contains category ids for which to download data for. 
  cats_to_dl <- read_csv(args[1]) %>% pull(cid)
  
  new_sids <- cats_to_dl %>% map(dl_sids) %>% bind_rows()
  
  # Previously downloaded data
  old_data <- read_csv(args[2], col_types = cols(.default = 'c'))
  
  sids_to_update <- new_updates(old_data %>% dplyr::select(series_id, updated), new_sids)
  message('The following series ids have new data available and will be updated: ', sids_to_update)
  
  sids_new <- setdiff(new_sids$series_id, old_data$series_id)
  message('The following series ids are new to the database and will be downloaded: ', sids_new)
  
  sids_to_dl <- c(sids_to_update, sids_new)
  # sids_to_keep <- setdiff(old_data$series_id, sids_to_update)
  
  logfile <- paste0('logs/eia-api-dl-', Sys.Date(), '.txt')
  message('Log for this session is stored in ', logfile)
  
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
    }
  }
  sink()
  closeAllConnections()
  gc()
  
  new_eia_data <- bind_rows(new_eia_data) %>% select(-description)
  
  # remove rows from the old database for which new data were downloaded
  # and append the new data
  eia_data <- old_data %>%
    filter(!series_id %in% sids_to_dl) %>% 
    bind_rows(new_eia_data)
  
  write_csv(eia_data, args[2])
  message('Raw database has been updated and saved to ', args[2])
}

main()