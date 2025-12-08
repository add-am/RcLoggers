
library(openxlsx2)
library(purrr)
library(glue)
library(ncdf4)
library(xml2)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)


logger_extract <- function(SelectedYears, SelectedLoggers, DataFlagsKept, DailyValues){}

#access the year(s) that the user requested
#target_years <- input$SelectedYears

#access the logger name(s) that the user requested
#target_loggers <- input$SelectedLoggers

#create a pairwise combination of the years and loggers to form the inputs for each query
target_matrix <- expand.grid(Years = SelectedYears, Loggers = SelectedLoggers)

#map over year and logger lists to create unique urls, pull var and dim names, use this to extract data and build a df
retrieve_data <- pmap(target_matrix, function(Years, Loggers){
  
  #create a url to the catalogue in xml format, based on year(s) selected
  catalogue_url <- glue("https://thredds.aodn.org.au/thredds/catalog/AIMS/Marine_Monitoring_Program/FLNTU_timeseries/{Years}/catalog.xml")

  #open the url as an object in R
  catalogue <- read_html(catalogue_url)

  #pull out the dataset variable from the raw xml
  nc_files <- xml_find_all(catalogue, ".//dataset")

  #pull out the id from this object (which is the name of each of the logger datasets)
  file_names <- xml2::xml_attr(nc_files, "id")

  #create a vector of logger names
  logger_names <- str_extract_all(file_names, "_.{3,5}_(?=FV01)")
  logger_names <- str_remove_all(unlist(logger_names), "_")

  #create a vector of logger deployment dates
  logger_dates <- str_extract_all(file_names, "\\d{8}(?=Z)")
  logger_dates <- str_remove_all(unlist(logger_dates), "_")

  #record the index for each logger name that matches what the user requested
  logger_indicies <- which(logger_names %in% Loggers)

  #extract the logger deployment dates associated with those loggers using the index determined based on name
  logger_dates <- logger_dates[logger_indicies]
            
  #sometimes there are multiple deployment dates per logger, so for each deployment:
  all_data_one_year <- list_rbind(map(logger_dates, \(date){
    
    #build the completed url
    completed_url <- glue("https://thredds.aodn.org.au/thredds/dodsC/AIMS/Marine_Monitoring_Program/FLNTU_timeseries/{Years}/AIMS_MMP-WQ_KUZ_{date}Z_{Loggers}_FV01_timeSeries_FLNTU.nc")

    #open the url
    nc <- nc_open(completed_url)

    #extract all variable and dimension names
    variable_names <- names(nc$var)
    dimension_names <- names(nc$dim)

    #replace the "timeseries" variable name, with the "time dimension name
    vec_of_data_names <- str_replace(variable_names, "TIMESERIES", dimension_names)

    #map over the vector and extract the data associated with each name. Store the result in a list
    target_data <- purrr::map(vec_of_data_names, function(x) ncvar_get(nc, x))
              
    #name each item in the list using the vector of variable and dimension names
    names(target_data) <- vec_of_data_names

    #extract the time vals
    time_vals <- target_data$TIME
              
    #assign an origin value to our "zero", make sure it has the UTC timezone, and contains hms
    time_origin <- ymd_hms("1950-01-01 00:00:00", tz = "UTC")

    #calculate new values by converting old values to absolute time intervals (purely total seconds), then adding that to our formatted origin
    time_vals <- time_origin + ddays(time_vals)

    #add 10 hours to bring time to EST
    time_vals <- time_vals + hours(10)

    #create a dataframe from the time, chla and turbidity values, plus their data flags
    simple_df <- data.frame(
      Time = time_vals, 
      Concentration_Chlorophyll = target_data$CPHL,
      Flags_Chlorophyll = target_data$CPHL_quality_control,
      Concentration_Turbidity = target_data$TURB,
      Flags_Turbidity = target_data$TURB_quality_control,
      Latitude = target_data$LATITUDE,
      Longitude = target_data$LONGITUDE
    )

    #add columns that track the year logger to the csv, plus the units for each variable
    simple_df <- simple_df |> 
      mutate(
        Logger = Loggers,
        Year = Years,
        Units_Chlorophyll = nc$var$CPHL$units,
        Units_Turbidity = "NTU"
      )
              
    #pivot the data longer, stacking turb and chla, and their flags
    pivot_df <- simple_df |>
      pivot_longer(
        cols = c(Concentration_Chlorophyll, Flags_Chlorophyll, Units_Chlorophyll, Concentration_Turbidity, Flags_Turbidity, Units_Turbidity),
        names_to = c(".value", "Indicator"),
        names_pattern = "(.*)_(.*)")
              

    #return the df as an element in the over arching list
    return(pivot_df)

  }))
            
})

#combine the list of dataframes into one large dataframe
final_df <- bind_rows(retrieve_data)

#return this single df as the final output of the reactive function
return(final_df)

#if the user wants to filter by flag, do that
filtered_user_data <- reactive({
  
  #pull data from previous func
  data <- return_user_data()

  #filter the data by the requested flags
  data_filtered <- data |> 
    filter(Flags %in% input$DataFlagsKept)
    
  #return the filtered data
  return(data_filtered)
})

#if the user wants to aggregate data, do that
aggregate_user_data <- reactive({
  
  #pull data from previous func
  data <- filtered_user_data()

  #pull out date values (i.e. separate time and day)
  data <- data |> 
    rename(DateTime = Time) |> 
    separate_wider_delim(cols = DateTime, delim = " ", names = c("Date", "Time"), cols_remove = FALSE) |> 
    mutate(Date = ymd(Date),
           Time = hms(Time))

  #if switch is true, aggregate, otherwise do nothing
  if(input$DailyValues){
    
    #group by date and summarise
    data <- data |> 
      group_by(Date, Latitude, Longitude, Logger, Indicator, Units) |> 
      summarise(Concentration = mean(Concentration, na.rm = T),
                Flags = as.numeric(paste(unique(Flags), collapse = ", ")))
  }
    
  #return the aggregated data
  return(data)

})



