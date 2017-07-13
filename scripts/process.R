# ---- Install missing dependencies ----

packages <- c("magrittr", "data.table", "readr", "units")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library(magrittr)

# ---- Load functions ----

parse_noaa_lcd <- function(dt) {
  result <- list()
  # Parse metadata
  result$meta <- with(dt[1, ], data.table::data.table(
    id = {
      STATION
    },
    name = {
      STATION_NAME
    },
    longitude = {
      LONGITUDE %>%
        as.numeric()
    },
    latitude = {
      LATITUDE %>%
        as.numeric()
    },
    elevation = {
      ELEVATION %>%
        as.numeric()
    }
  ))
  # Parse data
  # s = suspect value (appears with value)
  result$data <- with(dt, data.table::data.table(
    # Time of observation given as a 4-digit number using a 24-hour clock in local standard time (e.g. 1751 = 5:51 pm LST). No adjustments are made to account for Daylight Savings Time (DST).
    t = {
      DATE %>%
        readr::parse_datetime("%Y-%m-%d %H:%M") %>%
        add(as.difftime(9, units = "hours")) %>%
        format("%Y-%m-%dT%H:%MZ")
    },
    # This is the dry-bulb temperature and is commonly used as the standard air temperature reported. It is given here in tenths of a degree Celsius.
    air_temperature = {
      HOURLYDRYBULBTEMPC %>%
        as.numeric()
    },
    # This is the dew point temperature. It is given here in tenths of a degree Celsius.
    dew_point = {
      HOURLYDewPointTempC %>%
        as.numeric()
    },
    # This is the relative humidity given to the nearest whole percentage.
    relative_humidity = {
      HOURLYRelativeHumidity %>%
        as.numeric()
    },
    # Speed of the wind at the time of observation given in miles per hour (mph).
    wind_speed = {
      HOURLYWindSpeed %>%
        as.numeric() %>%
        convert_units(from = mi/hr, to = m/s)
    },
    # Wind direction from true north using compass directions (e.g. 360 = true north, 180 = south, 270 = east, etc.). Note: A direction of “000” is given for calm winds.
    # NOTE: Assuming direction wind is coming from in degrees clockwise from true north.
    wind_direction = {
      HOURLYWindDirection %>%
        as.numeric() %>%
        replace(. == 0, NA) %>%
        multiply_by(-1) %>%
        subtract(90) %>%
        mod(360) %>%
        convert_units(from = `°`, to = rad)
    },
    # Atmospheric pressure observed at the station during the time of observation. Given in inches of Mercury (in Hg).
    air_pressure = {
      HOURLYStationPressure %>%
        as.numeric() %>%
        convert_units(from = inHg, to = Pa)
    },
    # Amount of precipitation in inches to hundredths over the past hour. For certain automated stations, precipitation will be reported at sub-hourly intervals (e.g. every 15 or 20 minutes) as an accumulated amount of all precipitation within the preceding hour. "T" = trace precipitation amount or snow depth (an amount too small to measure, usually < 0.005 inches water equivalent).
    precipitation = {
      HOURLYPrecip %>%
        replace(. %in% c("T", "TRUE"), 0.005) %>%
        as.numeric() %>%
        convert_units(from = `in`, to = m)
    }
  )) %>%
    remove_empty(ignore = "t")
  # Return results
  return(result)
}

# ---- Rename source files ----
# <NETWORK><STATION>_YYYYMMDDHHMM_YYYYMMDDHHMM.csv

files <- list.files("sources", "*.csv", full.names = TRUE)
for (file in files) {
  dt <- data.table::fread(file, select = c("DATE", "STATION"))
  station <- dt$STATION[1] %>%
    gsub(":", "", .)
  ends <- dt$DATE %>%
    sort() %>%
    extract(c(1, length(.))) %>%
    gsub("\\-|:| ", "", .)
  newfile <- paste(station, ends[1], ends[2], sep = "_") %>%
    paste("csv", sep = ".")
  file.rename(file, file.path("sources", newfile))
}

# ---- Process each station ----

# List files and stations
files <- list.files("sources", "*.csv", full.names = TRUE)
stations <- files %>%
  gsub("^.*\\/|_.*$", "", .) %>%
  unique()
# Read and parse files for each station
results <- stations %>%
  lapply(function(station) {
    files %>%
      subset(grepl(station, .)) %>%
      lapply(function(file) {
        colClasses <- data.table::fread(file, nrows = 0) %>%
          ncol() %>%
          rep("character", .)
        # M / -9999 = missing value (appears instead of value)
        data.table::fread(file, na.strings = c("", "-9999", "M"), colClasses = colClasses)
      }) %>%
      data.table::rbindlist() %>%
      parse_noaa_lcd()
  }) %>%
  set_names(stations)
# Write joined station metadata to file
results %>%
  lapply("[[", "meta") %>%
  data.table::rbindlist() %>%
  data.table::fwrite(file.path("data", "stations.csv"))
# Write station data to file(s)
for (station in names(results)) {
  results[[station]] %$% data %>%
    data.table::fwrite(file.path("data", paste0(station, ".csv")))
}
