##' Save forecast and metadata to file, submit forecast to EFI
##' @param forecast dataframe
##' @param team_info list, see example
##' @param submit boolean, should forecast be submitted to EFI challenge
submit_forecast <- function(forecast,team_info,submit=FALSE){
  
  #Forecast output file name in standards requires for Challenge.  
  # csv.gz means that it will be compressed
  forecast_file <- paste0("aquatics","-",min(forecast$reference_datetime),"-",team_info$team_name,".csv.gz")
  
  ## final format tweaks for submission
  forecast = forecast |> mutate(model_id = team_info$team_name, family="ensemble") |>
    relocate(model_id,reference_datetime) |>
    relocate(parameter,.before = variable) |>
    relocate(family,.before = parameter)
  
  #Write csv to disk
  write_csv(forecast, forecast_file)
  
  #Confirm that output file meets standard for Challenge
  neon4cast::forecast_output_validator(forecast_file)
  
  # Generate metadata
  model_metadata = list(
    forecast = list(
      model_description = list(
        forecast_model_id =  system("git rev-parse HEAD", intern=TRUE), ## current git SHA
        name = "Air temperature to water temperature linear regression plus assume saturated oxygen", 
        type = "empirical",  
        repository = "https://github.com/MaxwellArchibald/practice-repository"   ## put your REPO here
      ),
      initial_conditions = list(
        status = "absent"
      ),
      drivers = list(
        status = "propagates",
        complexity = 1, #Just air temperature
        propagation = list( 
          type = "ensemble", 
          size = 31) 
      ),
      parameters = list(
        status = "data_driven",
        complexity = 2 # slope and intercept (per site)
      ),
      random_effects = list(
        status = "absent"
      ),
      process_error = list(
        status = "absent"
      ),
      obs_error = list(
        status = "absent"
      )
    )
  )
  
  ## this function needs to be restored
  #metadata_file <- neon4cast::generate_metadata(forecast_file, team_info$team_list, model_metadata)
  
  if(submit){
    neon4cast::submit(forecast_file = forecast_file, ask = FALSE) #metadata = metadata_file,
  }
  
}
