##' Download Targets
##' @return data.frame in long format with days as rows, and time, site_id, variable, and observed as columns
download_targets <- function(){
  readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)
}

##' Download Site metadata
##' @return metadata dataframe
download_site_meta <- function(){
  site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") 
  site_data %>% filter(as.integer(aquatics) == 1)
}


##' append historical meteorological data into target file
##' @param target targets dataframe
##' @return updated targets dataframe with added weather data
merge_met_past <- function(target){
  
  ## connect to data
  df_past <- neon4cast::noaa_stage3()
  
  ## filter for site and variable
  sites <- unique(target$site_id)
  
  ## temporary hack to remove a site that's mid-behaving
  sites = sites[!(sites=="POSE")] 
  target = target |> filter(site_id %in% sites)  
  
  ## grab air temperature from the historical forecast
  noaa_past <- df_past |> 
    dplyr::filter(site_id %in% sites,
                  variable == "air_temperature") |> 
    dplyr::collect()
  
  ## aggregate to daily
  noaa_past_mean = noaa_past |> 
    mutate(datetime = as.Date(datetime)) |>
    group_by(datetime, site_id) |> 
    summarise(air_temperature = mean(prediction),.groups = "drop")
  
  ## Aggregate (to day) and convert units of drivers
  target <- target %>% 
    group_by(datetime, site_id,variable) %>%
    summarize(obs2 = mean(observation, na.rm = TRUE), .groups = "drop") %>%
    mutate(obs3 = ifelse(is.nan(obs2),NA,obs2)) %>%
    select(datetime, site_id, variable, obs3) %>%
    rename(observation = obs3) %>%
    filter(variable %in% c("temperature", "oxygen")) %>% 
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  ## Merge in past NOAA data into the targets file, matching by date.
  target <- left_join(target, noaa_past_mean, by = c("datetime","site_id"))
  
}

##' Download NOAA GEFS weather forecast
##' @param forecast_date start date of forecast
##' @return dataframe
download_met_forecast <- function(forecast_date){
  noaa_date <- forecast_date - lubridate::days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet
  
  ## connect to data
  df_future <- neon4cast::noaa_stage2(start_date = as.character(noaa_date))
  
  ## filter available forecasts by date and variable
  met_future <- df_future |> 
    dplyr::filter(datetime >= lubridate::as_datetime(forecast_date), 
                  variable == "air_temperature") |> 
    dplyr::collect()
  
  ## aggregate to daily
  met_future <- met_future %>% 
    mutate(datetime = lubridate::as_date(datetime)) %>% 
    group_by(datetime, site_id, parameter) |> 
    summarize(air_temperature = mean(prediction), .groups = "drop") |> 
    #    mutate(air_temperature = air_temperature - 273.15) |> 
    select(datetime, site_id, air_temperature, parameter)
  
  return(met_future)
}