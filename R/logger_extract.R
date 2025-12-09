#' Extract AIMS MMP Logger Data
#'
#' @param Years Numeric Vector. The year(s) of data you want to access.
#' @param Loggers Character Vector. The name(s) of the logger(s) you want to access.
#' @param Indicators Character Vector. The name(s) of the indicators you want to access, can be Chlorophyll and/or Turbidity.
#' @param FilterFlags Boolean. Do you want to filter the data by its quality flags? This defaults to TRUE (yes).
#' @param FlagTags Numeric Vector. A vector of flag tags to keep, choices are: 0 =  No_QC_performed, 
#' 1 = Good_data, 2 = Probably_good_data, 3 = Bad_data_that_are_potentially_correctable, 4 = Bad_data, 5 = Value_changed. 
#' Advice from data providers is to keep only tags 1 and 2. By default only tags 1 and 2 are kept.
#' @param Aggregate Boolean. Do you want to aggregate data to daily values? This defaults to FALSE (no) and returns
#' 10-minute interval data.
#' @param AggreationType Character String. Defines the type of aggregation to apply, one of: Hourly, or Daily
#' @param SmallTables Boolean. Do you want to return small tables (less than 1,500 rows per table). This defaults to FALSE (no)
#'
#' @returns A long format dataframe
#'
#' @export
#' 
#' @examples
#' wq_data <- logger_extract(
#' Years = 2025, 
#' Loggers = "BUR2", 
#' FilterFlags = TRUE, 
#' FlagTags = c(1,2), 
#' Aggregate = FALSE,
#' SmallTables = FALSE
#' )

logger_extract <- function(
  Years, 
  Loggers, 
  Indicators,
  FilterFlags = TRUE, 
  FlagTags = c(1,2), 
  Aggregate = FALSE,
  AggregationType,
  SmallTables = FALSE,
  RowCount
){

  #create a pairwise combination of the years and loggers to form the inputs for each query
  target_matrix <- expand.grid(Years = Years, Loggers = Loggers)

  #map over year and logger lists to create unique urls, pull var and dim names, use this to extract data and build a df
  retrieve_data <- purrr::pmap(target_matrix, function(Years, Loggers){
    
    #create a url to the catalogue in xml format, based on year(s) selected
    catalogue_url <- glue::glue("https://thredds.aodn.org.au/thredds/catalog/AIMS/Marine_Monitoring_Program/FLNTU_timeseries/{Years}/catalog.xml")

    #open the url as an object in R
    catalogue <- xml2::read_html(catalogue_url)

    #pull out the dataset variable from the raw xml
    nc_files <- xml2::xml_find_all(catalogue, ".//dataset")

    #pull out the id from this object (which is the name of each of the logger datasets)
    file_names <- xml2::xml_attr(nc_files, "id")

    #create a vector of logger names
    logger_names <- stringr::str_extract_all(file_names, "_.{3,5}_(?=FV01)")
    logger_names <- stringr::str_remove_all(unlist(logger_names), "_")

    #create a vector of logger deployment dates
    logger_dates <- stringr::str_extract_all(file_names, "\\d{8}(?=Z)")
    logger_dates <- stringr::str_remove_all(unlist(logger_dates), "_")

    #record the index for each logger name that matches what the user requested
    logger_indicies <- which(logger_names %in% Loggers)

    #extract the logger deployment dates associated with those loggers using the index determined based on name
    logger_dates <- logger_dates[logger_indicies]
              
    #sometimes there are multiple deployment dates per logger, so for each deployment:
    all_data_one_year <- purrr::list_rbind(purrr::map(logger_dates, \(date){
      
      #build the completed url
      completed_url <- glue::glue("https://thredds.aodn.org.au/thredds/dodsC/AIMS/Marine_Monitoring_Program/FLNTU_timeseries/{Years}/AIMS_MMP-WQ_KUZ_{date}Z_{Loggers}_FV01_timeSeries_FLNTU.nc")

      #open the url
      nc <- ncdf4::nc_open(completed_url)

      #extract the attribution text
      att_text <- ncdf4::ncatt_get(nc, 0, "acknowledgement")$value

      #clean the text up
      att_text <- stringr::str_extract(att_text, "(?<=\")([^\"]*)(?=\")")

      #extract all variable and dimension names
      variable_names <- names(nc$var)
      dimension_names <- names(nc$dim)

      #replace the "timeseries" variable name, with the "time dimension name
      vec_of_data_names <- stringr::str_replace(variable_names, "TIMESERIES", dimension_names)

      #map over the vector and extract the data associated with each name. Store the result in a list
      target_data <- purrr::map(vec_of_data_names, function(x) ncdf4::ncvar_get(nc, x))
                
      #name each item in the list using the vector of variable and dimension names
      names(target_data) <- vec_of_data_names

      #extract the time vals
      time_vals <- target_data$TIME
                
      #assign an origin value to our "zero", make sure it has the UTC timezone, and contains hms
      time_origin <- lubridate::ymd_hms("1950-01-01 00:00:00", tz = "UTC")

      #calculate new values by converting old values to absolute time intervals (purely total seconds), then adding that to our formatted origin
      time_vals <- time_origin + lubridate::ddays(time_vals)

      #add 10 hours to bring time to EST
      time_vals <- time_vals + lubridate::hours(10)

      #create a dataframe from the time, chla and turbidity values, plus their data flags
      simple_df <- data.frame(
        DateTime = time_vals, 
        Result_Chlorophyll = target_data$CPHL,
        Flags_Chlorophyll = target_data$CPHL_quality_control,
        Result_Turbidity = target_data$TURB,
        Flags_Turbidity = target_data$TURB_quality_control,
        Latitude = target_data$LATITUDE,
        Longitude = target_data$LONGITUDE
      )

      #create columns that track the year, logger, and units to the csv
      simple_df <- simple_df |> 
        dplyr::mutate(
          Logger = Loggers,
          Year = Years,
          Units_Chlorophyll = "mgm3",
          Units_Turbidity = "ntu"
        )
                
      #pivot the data longer, stacking turb and chla, and their flags
      pivot_df <- simple_df |>
        tidyr::pivot_longer(
          cols = c(Result_Chlorophyll, Flags_Chlorophyll, Units_Chlorophyll, Result_Turbidity, Flags_Turbidity, Units_Turbidity),
          names_to = c(".value", "Indicator"),
          names_pattern = "(.*)_(.*)")
      
      #unite the indicator and units columns
      pivot_df <- pivot_df |> 
        tidyr::unite("Indicator", c(Indicator, Units), sep = "_")
      
      #include one final column (acknowledgement)
      pivot_df <- pivot_df |> 
        dplyr::mutate(Attribution = att_text)

      #return the df as an element in the over arching list
      return(pivot_df)

    }))
              
  })

  #this returns a list the length of nrow(target_matrix)

  


  #combine the list of dataframes into one large dataframe
  #final_df <- dplyr::bind_rows(retrieve_data)

  #if the user only wants chla 
  if (all(Indicators == "Chla")){final_df <- dplyr::filter(final_df, Results != "Turbidity")}

  #if the user only wants turbidity
  if (all(Indicators == "Turbidity")){final_df <- dplyr::filter(final_df, Results != "Chlorophyll")}

  #if the user wants to filter data by quality flag, do that (defaults to only having flags 1 and 2)
  if (FilterFlags){final_df <- dplyr::filter(final_df, Flags %in% FlagTags)}

  #if the user wants to do some kind of aggregation
  if (Aggregate){

    if (AggregationType == "Hourly"){

      #round DateTime to the nearest hour, then group and summarise
      final_df <- final_df |> 
        dplyr::mutate(DateTime = lubridate::round_date(DateTime, unit = "hour")) |> 
        dplyr::group_by(DateTime, Latitude, Longitude, Logger, Indicator, Units, Attribution) |> 
        dplyr::summarise(
          Result = mean(Result, na.rm = TRUE),
          Flags = paste(unique(Flags), collapse = ", ")
        )

    }

    if (AggregationType == "Daily"){

      #round DateTime to the nearest day, then group and summarise
      final_df <- final_df |> 
        dplyr::mutate(DateTime = lubridate::round_date(DateTime, unit = "day")) |> 
        dplyr::group_by(DateTime, Latitude, Longitude, Logger, Indicator, Units, Attribution) |> 
        dplyr::summarise(
          Result = mean(Result, na.rm = TRUE),
          Flags = paste(unique(Flags), collapse = ", ")
        )


    }
  }

  return(final_df)

}


for (i in 1:length(retrieve_data)){

  #get one of the dataframes
  data <- retrieve_data[[i]]

  #slice it up into chunks

  #build a vector of row indicies and table names
  min_indicies <- seq(1, nrow(data), 1500)
  max_indicies <- pmin(min_indicies + 1499, nrow(data))
  table_names <- paste0(target_matrix[i, 2], "_", target_matrix[i,1], "_rows_", min_indicies, "_to_", max_indicies)

  #list of dfs
  test <- purrr::map2(min_indicies, max_indicies, ~dplyr::slice(data, .x:.y))

  #name dfs in list
  names(test) <- table_names

}


test_result <- purrr::map2(retrieve_data, seq_along(retrieve_data), function(df, count){

  #build a vector of row indicies and table names
  min_indicies <- seq(1, nrow(df), 1500)
  max_indicies <- pmin(min_indicies + 1499, nrow(df))
  table_names <- paste0(
    target_matrix[count, 2], "_", 
    target_matrix[count, 1], "_rows_", 
    min_indicies, "_to_", max_indicies
  )

  #list of dfs
  test <- purrr::map2(min_indicies, max_indicies, ~dplyr::slice(df, .x:.y))

  #name dfs in list
  names(test) <- table_names

  test
})






