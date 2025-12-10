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
#' @param AggregationType Character String. Defines the type of aggregation to apply, one of: Hourly, or Daily
#' @param SmallTables Boolean. Do you want to return small tables (less than 1,500 rows per table). This defaults to FALSE (no)
#' @param RowCount Numeric String. The number of rows in each "small table", defaults to 1500
#'
#' @returns A list of dataframes
#'
#' @export
#' 
#' @examples
#' wq_data <- logger_extract(
#' Years = 2025, 
#' Loggers = "BUR2"
#' )

logger_extract <- function(
  Years, 
  Loggers, 
  Indicators = c("chlorophyll", "turbidity"),
  FilterFlags = TRUE, 
  FlagTags = c(1,2), 
  Aggregate = FALSE,
  AggregationType,
  SmallTables = FALSE,
  RowCount = 1500
){

  #check required arguments
  if (missing(Years)){stop("You must supply at least one year to target.")}
  if (missing(Loggers)){stop("You must supply at least one logger to target.")}

  #standardised character inputs (note, loggers go to caps, indicators go to lowercase)
  Loggers <- stringr::str_to_upper(Loggers)
  Indicators <- stringr::str_to_lower(Indicators)
  
  #define input choices
  logger_choices <- c(
    "BUR1", "BUR13", "BUR2", "BUR4", "FTZ1", "FTZ2", "FTZ6", "RM1",  "RM10", "RM7",  
    "RM8", "TUL10", "TUL3", "WHI1", "WHI4", "WHI5", "WHI6"
  )
  indicator_choices <- c("chlorophyll", "turbidity")
  flag_choices <- c(0, 1, 2, 3, 4, 5)

  #check input choices
  if (!any(Loggers %in% logger_choices)){
    stop("Invalid 'Loggers' argument: must be one of ", paste(logger_choices, collapse = ", "))
  }
  if (!any(Indicators %in% indicator_choices)){
    stop("Invalid 'Indicators' argument: must be one of ", paste(indicator_choices, collapse = ", "))
  }
  if (!any(FlagTags %in% flag_choices)){
    stop("Invalid 'FlagTags' argument: must be one of ", paste(flag_choices, collapse = ", "))
  }

  #create a specialised check for the case when aggregate is true
  if (Aggregate){
    
    #check if the argument is present
    if (missing(AggregationType)){stop("If aggregating data, you must supply an aggregation type.")}

    #standardise argument
    AggregationType <- stringr::str_to_lower(AggregationType)

    #define input choices
    aggregation_choices <- c("hourly", "daily")

    #check input choices
    if (!(AggregationType %in% aggregation_choices)){
      stop("Invalid 'AggregationType' argument: must be one of ", paste(aggregation_choices, collapse = ", "))
    }

  }

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

  if (SmallTables){#if the user wants to return small tables (rather than full series) slice up all the tables
    
    #map over the list of datasets (plus the number of total datasets)
    list_of_dfs <- purrr::map2(retrieve_data, seq_along(retrieve_data), function(df, count){

      #build a vector of min and max row indicies
      min_indicies <- seq(1, nrow(df), RowCount)
      max_indicies <- pmin(min_indicies + (RowCount-1), nrow(df))
      
      #and a vector of table names
      table_names <- paste0(
        target_matrix[count, 2], "_", 
        target_matrix[count, 1], "_rows_", 
        min_indicies, "_to_", max_indicies
      )

      #slice up the dataframe into small chunks and return it as a list
      small_tables <- purrr::map2(min_indicies, max_indicies, ~dplyr::slice(df, .x:.y))

      #name each of the mini dataframes in the list
      names(small_tables) <- table_names

      #return the list of dataframes
      small_tables
    
    })

    #if the purr map occured more than once, flatten the list of lists
    #if (!all(seq_along(retrieve_data) <= 1)){
    list_of_dfs <- unlist(list_of_dfs, recursive = FALSE)#}

  } else {#if the user wants to keep each dataframe intact

    #map over the list of datasets and create a vector of names
    table_names <- purrr::map(seq_along(retrieve_data), ~paste0(target_matrix[.x, 2], "_", target_matrix[.x, 1]))

    #update the names of each item in the list
    names(retrieve_data) <- table_names

    #then update the object name
    list_of_dfs <- retrieve_data
  }

  #if the user only wants chla 
  if (all(Indicators == "chlorophyll")){list_of_dfs <- purrr::map(list_of_dfs, ~dplyr::filter(.x, Indicator != "Turbidity_ntu"))}

  #if the user only wants turbidity
  if (all(Indicators == "turbidity")){list_of_dfs <- purrr::map(list_of_dfs, ~dplyr::filter(.x, Indicator != "Chlorophyll_mgm3"))}

  #if the user wants to filter data by quality flag, do that (defaults to only having flags 1 and 2)
  if (FilterFlags){list_of_dfs <- purrr::map(list_of_dfs, ~dplyr::filter(.x, .data$Flags %in% FlagTags))}

  #if the user wants to do some kind of aggregation
  if (Aggregate){

    if (AggregationType == "hourly"){

      list_of_dfs <- purrr::map(list_of_dfs, function(df){

        #round DateTime to the nearest hour, then group and summarise
        df |> 
          dplyr::mutate(DateTime = lubridate::round_date(DateTime, unit = "hour")) |> 
          dplyr::group_by(DateTime, Latitude, Longitude, Logger, Indicator, Attribution) |> 
          dplyr::summarise(
            Result = mean(Result, na.rm = TRUE),
            Flags = paste(unique(Flags), collapse = ", ")
        )
      })      
    }

    if (AggregationType == "daily"){

      list_of_dfs <- purrr::map(list_of_dfs, function(df){

        #round DateTime to the nearest day, then group and summarise
        df |> 
          dplyr::mutate(DateTime = lubridate::round_date(DateTime, unit = "day")) |> 
          dplyr::group_by(DateTime, Latitude, Longitude, Logger, Indicator, Attribution) |> 
          dplyr::summarise(
            Result = mean(Result, na.rm = TRUE),
            Flags = paste(unique(Flags), collapse = ", ")
          )
      })      
    }
  }

  return(list_of_dfs)

}
