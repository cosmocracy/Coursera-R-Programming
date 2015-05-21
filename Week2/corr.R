corr <- function(directory, threshold = 0) {
  complete_list <- complete(directory)
  monitor_ids <- complete_list[ which(complete_list$nobs >= threshold), "id"]
  # Create an empty data set into which we will accumulate readings from
  # the monitors of interest
  data <- data.frame()
  # For each monitor meeting the threshold
  for(monitor_id in monitor_ids) {
    filename <- sprintf("specdata/%03d.csv", monitor_id)
    file_data <- read.csv(file = filename, header = T, na.strings = "NA")
    file_data_cleansed <- file_data[! is.na(file_data$sulfate) & ! is.na(file_data$nitrate), ]
    data <- rbind(data, c(monitor_id, cor(file_data_cleansed$sulfate, file_data_cleansed$nitrate)))
  }
  if(nrow(data) == 0) {
    return(numeric())
  }
  return(data[,2])
}