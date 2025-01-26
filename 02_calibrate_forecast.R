##' Calibrate aquatic forecast model
calibrate_forecast <- function(target){
  fit <- list() 
  sites <- unique(target$site_id)
  for(i in 1:length(sites)){
    site_target <- target |> 
      filter(site_id == sites[i])
    
    if(length(which(!is.na(site_target$air_temperature) & !is.na(site_target$temperature))) > 0){
      #Fit linear model based on past data: water temperature = m * air temperature + b
      fit[[i]] <- lm(temperature~air_temperature,data = site_target)
    }
  }
  names(fit) <- sites
  return(fit)
}