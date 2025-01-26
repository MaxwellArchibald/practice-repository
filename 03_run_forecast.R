##' run aquatic forecast into the future
##' @param model site-specific list of forecast models
##' @param met_forecast weather forecast dataframe
##' @param site_data dataframe of site metadata
##' @return dataframe in EFI standard format
run_forecast <- function(model,met_forecast,site_data){
  
  forecast <- NULL
  sites <- names(model)
  
  for(i in 1:length(sites)){
    
    # Get site information for elevation
    site_info <- site_data %>% filter(field_site_id == sites[i]) 
    
    met_future_site <- met_future |> 
      filter(site_id == sites[i])
    
    if(!is.null(model[[i]])){
      
      #use model to forecast water temperature for each ensemble member
      forecasted_temperature <- predict(model[[i]],met_future_site)
      
      #use forecasted temperature to predict oyxgen by assuming that oxygen is saturated.  
      forecasted_oxygen <- rMR::Eq.Ox.conc(forecasted_temperature, 
                                           elevation.m = site_info$field_mean_elevation_m, 
                                           bar.press = NULL, 
                                           bar.units = NULL,
                                           out.DO.meas = "mg/L",
                                           salinity = 0, 
                                           salinity.units = "pp.thou")
      ## organize outputs
      temperature <- tibble(datetime = met_future_site$datetime,
                            site_id = sites[i],
                            parameter = met_future_site$parameter,
                            prediction = forecasted_temperature,
                            variable = "temperature")
      
      oxygen <- tibble(datetime = met_future_site$datetime,
                       site_id = sites[i],
                       parameter = met_future_site$parameter,
                       prediction = forecasted_oxygen,
                       variable = "oxygen")
      
      
      #Build site level dataframe.
      forecast <- dplyr::bind_rows(forecast, temperature, oxygen)
      
    }
    
  }
  
  ## reorganize into EFI standard
  forecast <- forecast |> 
    mutate(reference_datetime = forecast_date) |>
    select(datetime, reference_datetime, site_id, variable, parameter, prediction)
  
  return(forecast)
}