
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RcLoggers

<!-- badges: start -->

<!-- badges: end -->

The goal of RcLoggers is to get a complex function that requires too
much memory to run via Shiny onto your computer so you can run it
yourself.

## Installation

To install this package you will first need to download the [RTools
package](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html)
and install it on your computer (accept the defaults everywhere during
the installation process).

Following this, you can install the development version of RcLoggers
from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("add-am/RcLoggers")
```

## Using The Function

Currently only one function is provided: `logger_extract()`. A basic
example of using this function is as follows:

``` r
#load the library
library(RcLoggers)

#run the function (basic)
data_extract <- logger_extract(
 Years = 2025, 
 Loggers = "BUR2"
)
```

However, this function does have some more advanced options:

``` r
#run the function (detailed)
data_extract <- logger_extract(
 Years = c(2024, 2025), #you can dowload more than one year
 Loggers = c("BUR1", "BUR2"), #you can download more than one logger
 Indicators = "Chlorophyll", # if you only want one indicator you can define that (it defaults to chlorophyll and turbidity)
 FilterFlags = TRUE, #you can filter data by quality flag
 FlagTags = c(1,2),  #if you decide to filter you then provide the flag tags to keep
 Aggregate = TRUE, #you can aggregate data
 AggregationType = "Hourly" #if you do decide to aggregate, you need to define the type of aggregation (hourly or daily)
 SmallTables = TRUE #you can request that outputs are broken into several small tables 
 RowCount = 1500 #if you do want to break up tables, you can define the number of rows per table
)
```

The output of the function (regardless of if small tables is true or
false) is a list of dataframes. Each item in the list is assigned an
appropriate name, thus it is reccomended that data is saved using the
following code:

``` r
#save
purrr::walk2(data_extract, names(data_extract), ~readr::write_csv(.x, paste0(.y,".csv")))
```

where .x = data_extract, and .y = the names of items in the list:

![](%22man/figures/save_code_example.png%22)
