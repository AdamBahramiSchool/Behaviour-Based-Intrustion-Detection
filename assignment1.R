# ****************************************************************************
# CMPT-318 Group Assignment 1
# Group: 318programmers_Grp1
# Members:
# - Adam Bahrami
# - Bevan Nutakor
# - James Park
# CMPT-318 Group Assignment 1
# ****************************************************************************

# SET UP
# ------------------------------------------------
wd <- getwd()
setwd(wd)
library(ggplot2)
library(corrplot)

# ---------- Question 1  ----------
df <- read.delim('Group_Assignment_Dataset.txt', sep = ",")

# Ensure the `Date` and `Time` are in POSIXlt format
df$Date <- as.POSIXlt(df$Date, format = "%d/%m/%Y")
df$Time <- as.POSIXlt(df$Time, format = "%H:%M:%S")

df$DayOfWeek <- as.POSIXlt(df$Date)$wday

df$Month <- as.numeric(format(df$Date, "%m"))
df$Day <- as.numeric(format(df$Date, "%d"))

# Subset dataframe where month and day are both less than or equal to 7
new_df <- df[df$Month <= 1 & df$Day <= 7, ]

# Remove the Month and Day columns since they're not needed in the final dataframe
first_week_data <- new_df[, !names(new_df) %in% c("Month", "Day")]

# View(first_week_data)

linear_interpolation <- function(x) {
  na_index <- which(is.na(x))  # Find indices of NA values
  
  if (length(na_index) == 0) {
    return(x)  # If no NA values, return the original vector
  }
  
  # Linear interpolation for NA values
  non_na_index <- which(!is.na(x))
  x[na_index] <- approx(x = non_na_index, y = x[non_na_index], xout = x, method = "linear")$y
  
  return(x)
}

df_interpolated <- lapply(first_week_data, linear_interpolation)

# Convert the result back to a data frame
df_interpolated <- as.data.frame(df_interpolated) #NA points gone
print(any(is.na(df_interpolated)))
#calculate z-scores
z_score <- function(feature_values) {
  mean <- mean(feature_values)
  std_dev <- sd(feature_values)
  
  z_scores <- (feature_values - mean) / std_dev
  
  return(z_scores)
}

anomalies_df <- data.frame()
exclude_columns <- c("Date", "Time", "DayOfWeek")
for (col_name in names(df_interpolated)) {
  if (col_name %in% exclude_columns) {
    next
  }
  if (sd(df_interpolated[[col_name]]) != 0) {
    z_scores <- z_score(df_interpolated[[col_name]])
    
    anomalies <- z_scores[z_scores > 3 | z_scores < -3]
    
    if (length(anomalies) > 0) {
      # Create a dataframe of anomalies and bind it to anomalies_df
      anomalies_df <- rbind(anomalies_df, data.frame(Column = col_name, Absolute_Z_Score = anomalies))
    }
  }
}

# View(anomalies_df)

percentage_feature_anomalies <- list()
percentage_total_anomalies <- list()

# Iterate over each unique feature in the anomalies dataframe
for (feature in unique(anomalies_df$Column)) {
  # Filter anomalies for the current feature
  feature_anomalies <- anomalies_df$Absolute_Z_Score[anomalies_df$Column == feature]
  
  # Calculate the percentage of anomalies for the current feature
  total_obs <- length(df_interpolated[[feature]])
  total_anomalies <- length(feature_anomalies)
  percentage <- total_anomalies / total_obs * 100
  
  # Store the percentage in the percentage_anomalies list
  percentage_feature_anomalies[[feature]] <- percentage
}

# View(percentage_feature_anomalies)

#total num of anomalies for entire dataset
total_anomalies <- sum(sapply(anomalies_df$Absolute_Z_Score, length))

total_observations <- nrow(df_interpolated)

# Calculate the percentage of anomalies across the whole dataset
percentage_total_anomalies <- total_anomalies / total_observations * 100

print(percentage_total_anomalies)
# View the interpolated data frame
#df_interpolated to be used for question 2

# ---------- Question 2  ----------

#Turning each disjoint pair needed for the correlation calculation into a numeric value, 7 columns
selected_col <- df_interpolated[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
selected_col$Global_active_power <- as.numeric(selected_col$Global_active_power)
selected_col$Global_reactive_power <- as.numeric(selected_col$Global_reactive_power)
selected_col$Voltage <- as.numeric(selected_col$Voltage)
selected_col$Global_intensity <- as.numeric(selected_col$Global_intensity)
selected_col$Sub_metering_1 <- as.numeric(selected_col$Sub_metering_1)
selected_col$Sub_metering_2 <- as.numeric(selected_col$Sub_metering_2)
selected_col$Sub_metering_3 <- as.numeric(selected_col$Sub_metering_3)
# computing correlation coefficient for the 7 disjoint pairs using Pearson's correlation
correlation_calc <- cor(selected_col, method = "pearson")
# generates Rplots.pdf, demonstrating visulization of correlation matrix with color coding
corrplot(correlation_calc, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE, type = "upper", order = "hclust", col = colorRampPalette(c("blue", "white", "red"))(200))


# ---------- Question 3  ----------

df <- df_interpolated

day_interval <- list(start ="07:30:00", end  = "17:00:00")
night_interval <- list(start ="17:00:00", end  = "07:30:00")

weekdays_day <- subset(df, format(df$Date, "%u") %in% 1:5 & 
                         format(df$Time, "%H:%M:%S") >= day_interval$start & 
                         format(df$Time, "%H:%M:%S") < day_interval$end)

weekends_day <- subset(df, format(df$Date, "%u") %in% c(6, 7) &  
                         format(df$Time, "%H:%M:%S") >= day_interval$start & 
                         format(df$Time, "%H:%M:%S") < day_interval$end)

weekdays_night <- subset(df, format(df$Date, "%u") %in% 1:5 & 
                           (format(df$Time, "%H:%M:%S") < night_interval$start | 
                              format(df$Time, "%H:%M:%S") >= night_interval$end))

weekends_night <- subset(df, format(df$Date, "%u") %in% c(6, 7) &  
                           (format(df$Time, "%H:%M:%S") < night_interval$start | 
                              format(df$Time, "%H:%M:%S") >= night_interval$end))

# Calculate the average Global_intensity for each minute for weekdays day time
average_global_intensity_weekdays_day <- aggregate(weekdays_day$Global_intensity ~ format(weekdays_day$Time, "%H:%M:%S"), 
                                                   FUN = mean, data = weekdays_day)
# Calculate the average Global_intensity for each minute for weekends day time
average_global_intensity_weekends_day <- aggregate(weekends_day$Global_intensity ~ format(weekends_day$Time, "%H:%M:%S"), 
                                                   FUN = mean, data = weekends_day)
# Calculate the average Global_intensity for each minute for weekdays nigth time
average_global_intensity_weekdays_night <- aggregate(weekdays_night$Global_intensity ~ format(weekdays_night$Time, "%H:%M:%S"), 
                                                     FUN = mean, data = weekdays_night)
# Calculate the average Global_intensity for each minute for weekends night time
average_global_intensity_weekends_night <- aggregate(weekends_night$Global_intensity ~ format(weekends_night$Time, "%H:%M:%S"), 
                                                     FUN = mean, data = weekends_night)

# Rename the aggregated column to 'AverageGlobalIntensity'
names(average_global_intensity_weekdays_day)[2] <- "AverageGlobalIntensity"
names(average_global_intensity_weekends_day)[2] <- "AverageGlobalIntensity"
names(average_global_intensity_weekdays_night)[2] <- "AverageGlobalIntensity"
names(average_global_intensity_weekends_night)[2] <- "AverageGlobalIntensity"

# Convert 'Time' to minutes since start of day interval (07:30 AM)
convert_time_to_minutes <- function(time) {
  hours <- as.numeric(substr(time, 1, 2))
  minutes <- as.numeric(substr(time, 4, 5))
  total_minutes <- (hours * 60 + minutes) - (7 * 60 + 30)
  return(total_minutes)
}

average_global_intensity_weekdays_day$TimeNumeric <- sapply(average_global_intensity_weekdays_day$`format(weekdays_day$Time, "%H:%M:%S")`, convert_time_to_minutes)
average_global_intensity_weekends_day$TimeNumeric <- sapply(average_global_intensity_weekends_day$`format(weekends_day$Time, "%H:%M:%S")`, convert_time_to_minutes)
average_global_intensity_weekdays_night$TimeNumeric <- sapply(average_global_intensity_weekdays_night$`format(weekdays_night$Time, "%H:%M:%S")`, convert_time_to_minutes)
average_global_intensity_weekends_night$TimeNumeric <- sapply(average_global_intensity_weekends_night$`format(weekends_night$Time, "%H:%M:%S")`, convert_time_to_minutes)

# Linear Fit
lm_weekdays_day <- lm(AverageGlobalIntensity ~ TimeNumeric, data = average_global_intensity_weekdays_day)
lm_weekends_day <- lm(AverageGlobalIntensity ~ TimeNumeric, data = average_global_intensity_weekends_day)
lm_weekdays_night <- lm(AverageGlobalIntensity ~ TimeNumeric, data = average_global_intensity_weekdays_night)
lm_weekends_night <- lm(AverageGlobalIntensity ~ TimeNumeric, data = average_global_intensity_weekends_night)

# Polynomial Fit
poly_weekdays_day <- lm(AverageGlobalIntensity ~ poly(TimeNumeric, 2), data = average_global_intensity_weekdays_day)
poly_weekends_day <- lm(AverageGlobalIntensity ~ poly(TimeNumeric, 2), data = average_global_intensity_weekends_day)
poly_weekdays_night <- lm(AverageGlobalIntensity ~ poly(TimeNumeric, 2), data = average_global_intensity_weekdays_night)
poly_weekends_night <- lm(AverageGlobalIntensity ~ poly(TimeNumeric, 2), data = average_global_intensity_weekends_night)


plot_lm_model <- function(data, lm_model, title) {
  # Prepare a data frame for predictions
  time_range <- seq(min(data$TimeNumeric), max(data$TimeNumeric), length.out = 100)
  prediction_data <- data.frame(TimeNumeric = time_range)
  
  # Generate predictions for the linear model
  prediction_data$LMPredictions = predict(lm_model, newdata = prediction_data)
  
  # Plot linear model 
  ggplot(data, aes(x = TimeNumeric, y = AverageGlobalIntensity)) +
    geom_point() +
    geom_line(data = prediction_data, aes(x = TimeNumeric, y = LMPredictions), colour = "red") +
    labs(x = "Time Numeric", y = "Average Global Intensity", title = paste("Linear Model -", title)) +
    theme_minimal()
}

plot_poly_model <- function(data, poly_model, title) {
  # Prepare a data frame for predictions
  time_range <- seq(min(data$TimeNumeric), max(data$TimeNumeric), length.out = 100)
  prediction_data <- data.frame(TimeNumeric = time_range)
  
  # Generate predictions for the polynomial model
  prediction_data$PolyPredictions = predict(poly_model, newdata = prediction_data)
  
  # Plot polynomial model 
  ggplot(data, aes(x = TimeNumeric, y = AverageGlobalIntensity)) +
    geom_point() +
    geom_line(data = prediction_data, aes(x = TimeNumeric, y = PolyPredictions), colour = "blue") +
    labs(x = "Time Numeric", y = "Average Global Intensity", title = paste("Polynomial Model -", title)) +
    theme_minimal()
}

plot_lm_model(average_global_intensity_weekdays_day, lm_weekdays_day, "Weekdays Day Time")
plot_poly_model(average_global_intensity_weekdays_day, poly_weekdays_day, "Weekdays Day Time")

plot_lm_model(average_global_intensity_weekends_day, lm_weekends_day, "Weekends Day Time")
plot_poly_model(average_global_intensity_weekends_day, poly_weekends_day, "Weekends Day Time")

plot_lm_model(average_global_intensity_weekdays_night, lm_weekdays_night, "Weekdays Night Time")
plot_poly_model(average_global_intensity_weekdays_night, poly_weekdays_night, "Weekdays Night Time")

plot_lm_model(average_global_intensity_weekends_night, lm_weekends_night, "Weekends Night Time")
plot_poly_model(average_global_intensity_weekends_night, poly_weekends_night, "Weekends Night Time")