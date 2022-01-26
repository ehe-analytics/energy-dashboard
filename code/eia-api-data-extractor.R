## TODO: for data already in the system, update (API provides this) 
## Don't download data every time. 

library(tidyverse)


api_key <- paste0("api_key=", read.delim2('data/eia-api-key.txt', header=F))

# update_data <- function(sid) { 
#   api_link <- "http://api.eia.gov/updates/"
#   
# }

dl_data <- function(sid) { 
  api_link <- "http://api.eia.gov/series/"
  link <- paste0(api_link, "?", api_key, "&", "series_id=", sid)
 
  tryCatch({
    json <- rvest::read_html(link) %>%
      rvest::html_text() %>%
      jsonlite::fromJSON()
    },
    error = function(cond) {
      message(paste('Data for', sid, 'could not be downloaded due to an error.'))
      message(cond)
      return()
    }, warning = function(cond) {
      message(cond)
      return()
    }, finally = {
      message(paste('Data for', sid, 'successfully downloaded.'))
  })
  json$series
}

get_series_data <- function(sid) {
  json <- dl_data(sid)
  series_data <- setNames(data.frame(json$data), c('period', 'value'))
  df <- series_data %>% add_column(json %>% select(-data))
  return(df)
}

to_download <- c(2251664:2251669)

sids_df <- read_csv('data/series_ids.csv', col_types = cols(.default = 'c')) %>% 
  mutate(download = category_id %in% to_download)

sids_to_dl <- sids_df %>% 
  filter(download) %>% 
  pull(series_id) 

prev_eia_data <- read_csv('data/downloaded_eia_data.csv')

# A for loop is preferable to map because even if there is an error 
# it'll save data until the point of error. 
# map on the other hand discards all previous iterations in the event of an error
new_eia_data <- list()
for (i in 1:length(sids_to_dl)) { 
  new_eia_data[[i]] <- get_series_data(sids_to_dl[i])
  # After every 25 downloads, sleep for 30 seconds to prevent overloading of 
  # EIA API and potentially getting bounced off
  if (i%%25 == 0) Sys.sleep(30)
}

write_csv(bind_rows(new_eia_data) %>% select(-description), 'data/downloaded_eia_data.csv')
