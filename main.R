### Aquatic Forecast Workflow ###
# devtools::install_github("eco4cast/neon4cast")
library(tidyverse)
library(neon4cast)
library(lubridate)
#install.packages("rMR")
library(rMR)

forecast_date <- Sys.Date()
noaa_date <- Sys.Date() - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet

#Step 0: Define team name and team members 
team_info <- list(team_name = "air2waterSat_MCD",
                  team_list = list(list(individualName = list(givenName = "Mike", 
                                                              surName = "Dietze"),
                                        organizationName = "Boston University",
                                        electronicMailAddress = "dietze@bu.edu"))
)

## Load required functions
if(file.exists("01_download_data.R"))      source("01_download_data.R")
if(file.exists("02_calibrate_forecast.R")) source("02_calibrate_forecast.R")
if(file.exists("03_run_forecast.R"))       source("03_run_forecast.R")
if(file.exists("04_submit_forecast.R"))    source("04_submit_forecast.R")

### Step 1: Download Required Data
target     <- download_targets()       ## Y variables
site_data  <- download_site_meta()
target     <- merge_met_past(target)   ## append met data (X) into target file
met_future <- download_met_forecast(forecast_date) ## Weather forecast (future X)

## visual check of data
ggplot(target, aes(x = temperature, y = air_temperature)) +
  geom_point() +
  labs(x = "NEON water temperature (C)", y = "NOAA air temperature (C)") +
  facet_wrap(~site_id)

met_future %>% 
  ggplot(aes(x = datetime, y = air_temperature, group = parameter)) +
  geom_line() +
  facet_grid(~site_id, scale ="free")

### Step 2: Calibrate forecast model
model <- calibrate_forecast(target)

### Step 3: Make a forecast into the future
forecast <- run_forecast(model,met_future,site_data)

#Visualize forecast.  Is it reasonable?
forecast %>% 
  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  geom_line() +
  facet_grid(variable~site_id, scale ="free")

### Step 4: Save and submit forecast and metadata
submit_forecast(forecast,team_info,submit=FALSE)