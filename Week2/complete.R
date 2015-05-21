complete <- function(directory, monitor_ids = 1:332) {
  # Create an empty data set into which we will accumulate file info
  file_stats <- data.frame() #col.names = c("monitor", "nobs"))
  # For each monitor we'd like to consider
  for(monitor_id in monitor_ids) {
    filename <- sprintf("specdata/%03d.csv", monitor_id)
    data <- read.csv(file = filename, header = T, na.strings = "NA")
    complete_data <- data[! is.na(data$sulfate) & ! is.na(data$nitrate), ]
    file_stats <- rbind(file_stats, c(monitor_id, nrow(complete_data)))
  }
  names(file_stats) <- c("id", "nobs")
  return(file_stats)
}