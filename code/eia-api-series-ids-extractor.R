## TODO: some categories have both children categories and series. 
## For some of these, category data are as important (or even more important) 
## than the child series data. 

library(rvest)
library(magrittr)
library(dplyr)

api_link <- "http://api.eia.gov/category/"
api_key <- "api_key=Hrw4tdefn3akWomd049w15st6KDUn3WLw6VAcCuY"

# Query generator
generate_query <- function(link, ...) { 
  query <- list(...) %>% paste(collapse = '&')
  paste0(api_link, '?', query)
}

# Result from the API query in JSON format
html_result <- function(query) {
  result <- read_html(query) %>% 
    html_node('p') %>% 
    html_text() %>% 
    jsonlite::fromJSON() 
  
  # if the category is not found in the API, it just returns 0, which is 
  # the highest level. This causes the program to start back at the top,
  # leading to the possibility of essentially getting stuck in an endless loop. 
  if (result$request$category_id == 0) {
    message('Query submitted was incorrect. Check the query.\nMay be due to an incorrect category id.', )
    return()
  } else 
    return(result)
}

# Load previously downloaded series ids
load_prev_series_df <- function(loc) { 
  tryCatch(
    {
      eia_series_df <- read_csv(loc,  col_types = cols(.default = "c"))
    }, 
    error = function(cond) { 
      message('Database does not exist')
      message(cond)
      return(NA)
    },
    warning = function(cond) { 
      message(cond)
      return(NA)
    }, 
    finally = {
      message('Database successfully imported.')
    }
  ) 
  return(eia_series_df)
}

# Fancy recursion function to get the series ids embedded within each category.
# Keep passing subcategory ids to this function until there are no more.
# This is the terminal condition. In this case, grab the series ids. 
create_eia_df <- function(cat_ids) {
  # catchall
  if (length(cat_ids) == 0 || is.null(cat_ids)) return()

  # if only one category id, get html result and extract the data
  if (length(cat_ids) == 1) {
    subquery <- paste0('category_id=', cat_ids)
    result <- generate_query(api_link, api_key, subquery) %>%
      html_result()
    Sys.sleep(3)

    # these are all the subcategories of cat ids
    kittens <- result$category$childcategories
    
    
    # if there are no subcategory ids, then grab the series ids and create a df
    # (the terminal condition); otherwise, pass all the subcategory ids to 
    # back to this function
    if (length(kittens) == 0) {
      # Create data frame
      dat <- data.frame(category_id = result$category$category_id,
                        category_name = result$category$name,
                        result$category$childseries)
      # A tracker to follow progress
      print(paste(cat_ids, result$category$name))
      return(dat)
      # if subcategory ids exist, pass these ids back into this function
    } else create_eia_df(kittens$category_id)
    
    
  } else {
    idx <- length(cat_ids)
    # Get the data from the first element and pass the rest of the vector
    # back to this function. 
    return(bind_rows(create_eia_df(cat_ids[idx]), create_eia_df(cat_ids[-idx])))
  }
}

check_n_import <- function(cid, cname, dids) { 
  if (cid %in% dids) { 
    message(paste('Series IDs for Data ID', cid, 'already exist in the database. Will NOT be imported.'))
    return()
  } else {
    message(paste('Series IDs for Data ID', cid, 'being imported now. May take some time...'))
    dat <- create_eia_df(cid) %>%
      mutate(data_id = cid, data_name = cname)
    return(dat)
  }
}

import_sids <- function (cids, sids_df, update_all = F) {
  if (update_all) { 
    message(paste0(
      'This will update Series IDs for all Data IDs provided.\n', 
      'This action is NOT recommended since it will update Series IDs for all Data IDs. \n', 
      'This is usually not necessary unless you believe that multiple data sources (Series IDs) have been added in EIA.\n'))
    user_input <- readline(prompt = "Do you want to proceed anyway? (y or n): ")
  } else { 
    user_input <- 'n'  
  }
  
  if (user_input %in% c('Y', 'y')) { 
    message('Downloading Series IDs for all Data IDs. This will take a while...')  
    list(cids, names(cids)) %>%
      purrr::pmap_df(~ data.frame(data_id = .x, data_name = .y, create_eia_df(.x)))  
  } else if (user_input %in% c('N', 'n')) {
    old_dids <- unique(sids_df$data_id)
    list(cids, names(cids)) %>%
      purrr::pmap_df(~ check_n_import(.x, .y, old_dids))  
  } else {
    message('Invalid input. No action taken. ')
  }
}



### Get all series ids
cats <- c('Total Energy' = '711224', 'CO2 Emissions' = "2251604")

prev_series_df <- load_prev_series_df('data/series_ids.csv')
new_series_df <- import_sids(cats, prev_series_df, update_all = F)
series_df <- bind_rows(prev_series_df, new_series_df)

write_csv(series_df, 'data/series_ids.csv')
### End 