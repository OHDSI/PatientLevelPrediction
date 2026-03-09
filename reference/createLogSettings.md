# Create the settings for logging the progression of the analysis

Create the settings for logging the progression of the analysis

## Usage

``` r
createLogSettings(
  verbosity = "DEBUG",
  timeStamp = TRUE,
  logName = "runPlp Log"
)
```

## Arguments

- verbosity:

  Sets the level of the verbosity. If the log level is at or higher in
  priority than the logger threshold, a message will print. The levels
  are:

  - DEBUG Highest verbosity showing all debug statements

  - TRACE Showing information about start and end of steps

  - INFO Show informative information (Default)

  - WARN Show warning messages

  - ERROR Show error messages

  - FATAL Be silent except for fatal errors

- timeStamp:

  If TRUE a timestamp will be added to each logging statement.
  Automatically switched on for TRACE level.

- logName:

  A string reference for the logger

## Value

An object of class `logSettings` containing the settings for the logger

## Details

Returns an object of class `logSettings` that specifies the logger
settings

## Examples

``` r
# create a log settings object with DENUG verbosity, timestamp and log name 
# "runPlp Log". This needs to be passed to `runPlp`.
createLogSettings(verbosity = "DEBUG", timeStamp = TRUE, logName = "runPlp Log")
#> Use timeStamp: TRUE
#> $verbosity
#> [1] "DEBUG"
#> 
#> $timeStamp
#> [1] TRUE
#> 
#> $logName
#> [1] "runPlp Log"
#> 
#> attr(,"class")
#> [1] "logSettings"
```
