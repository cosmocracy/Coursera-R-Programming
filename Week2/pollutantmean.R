pollutantmean <- function(directory, pollutant, monitor_ids = 1:332) {
  # Create an empty data set into which we will accumulate readings from
  # the monitors of interest
  data <- data.frame()
  # For each monitor we'd like to consider
  for(monitor_id in monitor_ids) {
    filename <- sprintf("specdata/%03d.csv", monitor_id)
    data <- rbind(data, read.csv(file = filename, header = T, na.strings = "NA"))
  }
  return(mean(data[, pollutant], na.rm = T))
}