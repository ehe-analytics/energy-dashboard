
# Query generator
generate_query <- function(link, ...) { 
  query <- list(...) %>% paste(collapse = '&')
  paste0(api_link, '?', query)
}

generate_query <- function(api_link, ...) { 
  # Generic query generator for EIA API that accepts multiple options 
  # as long as they meet EIA requirements
  api_key <- paste0("api_key=", read.delim2('data/eia-api-key.txt', header=F))
  base <- paste0(api_link, '?', api_key)
  query <- c(base, list(...)) %>% paste(collapse = '&')
  
  return(query)
}

dl_data <- function(query) { 
  read_html(query) %>% 
    html_text() %>% 
    jsonlite::fromJSON()
}

dl_cats <- function(cid) { 
  api_link <- "http://api.eia.gov/category/"
  cidq <- paste0('category_id=', cid)
  query <- generate_query(api_link, cidq)
  
  if (cid == 0) {
    message('Incorrect category id. Category id cannot be 0.')
    return()
  }
  
  out <- dl_data(query)
  return(out)
}

dl_sids <- function(cid, deep = F) { 
  # Returns all **series ids** for the category being queried 
  deepq <- if (deep) 'deep=true' else 'deep=false'
  cidq <- paste0('category_id=', cid)
  rowq <- paste0('rows=10000')
  
  # EIA API link for latest updates for each series_id under a category
  api_link <- "http://api.eia.gov/updates/"
  query <- generate_query(api_link, cidq, deepq, rowq)
  
  if (cid == 0) {
    message('Incorrect category id. Category id cannot be 0.')
    return()
  }
  
  out <- dl_data(query)$updates
  if(is.null(nrow(out))) {
    message(paste('Category id', cid, 'not found in EIA database.'))
    return()
  }
  out %>% mutate(updated = lubridate::as_datetime(updated))
}

new_updates <- function(old_sids, new_sids) {
  old_sids %>%
    mutate(updated = lubridate::as_datetime(updated)) %>% 
    left_join(new_sids, by = 'series_id') %>%
    mutate(new_update = updated.y > updated.x) %>% 
    filter(new_update) %>%
    pull(series_id)
}

dl_series_data <- function(sid) {
  api_link <- "http://api.eia.gov/series/"
  query <- generate_query(api_link, paste0('series_id=', sid))
  
  tryCatch({
    json <- dl_data(query)
  },
  error = function(cond) {
    message(paste('Data for', sid, 'could not be downloaded due to the following error:'))
    message(cond)
    return()
  }, 
  warning = function(cond) {
    message('The following warning was returned by the server:')
    message(cond)
    return()
  }, finally = {
    message(paste('Data for', sid, 'successfully downloaded.'))
  })
  
  json$series
  
}

get_eia_data <- function(sid) { 
  out <- dl_series_data(sid)
  eia_data <- setNames(data.frame(out$data), c('period', 'value')) %>%
    tibble::add_column(out %>% select(-data))
  return(eia_data)
}
