setwd("C:/Users/edwin/OneDrive - The University of Manchester/Data_Science/Msc_Data_Science/Extended Research Project/melanopsin-IRL-Iok-dev/repository")
rm(list=ls(all=TRUE))

# Load required libraries
library(here)      # For handling file paths
library(dplyr)     # Data manipulation
library(purrr)     # Functional programming
library(lubridate) # Date-time manipulation
library(cosinor)   # Cosinor analysis
library(zoo)       # Handling missing values

# tell here where we are
here::i_am("repository/Data_Preprocessing_and_Aggregation.R")

# where to get data from an where to output to
input_path_bt <- here("repository","data","processed", "brightertime")
input_path_sw <-  here("repository", "data", "processed", "spectrawear")



################################################################################
## Light history against spectrawear data ####
# Brightertime data 
load(here(input_path_bt, "brightertime_kss.RData")) #bt_kss
load(here(input_path_bt, "brightertime_baseline.RData")) #bt_baseline
load(here(input_path_bt, "brightertime_cognitive_variables.RData")) #bt_cognitive

# Spectrawear data 
load(here(input_path_sw, "spectrawear_long.RData")) 


# Filter out instances where any reaction time is less than or equal to 100 ms
bt_cognitive_filtered <- bt_cognitive %>%
                          mutate(reaction_times = map(reaction_times, ~ .x[.x > 100])) %>%  # Filter each list in the column
                          filter(map_lgl(reaction_times, ~ length(.x) > 0))  # Remove rows where the resulting list is empty

# Merge bt_kss, bt_cognitive
bt_merged <- bt_cognitive %>%
  dplyr::select(-datetime.creat) %>%
  left_join(bt_kss, by= c("subj", "datetime.uploaded"))


# Check NA values for kss
time_without_kss <- bt_merged[is.na(bt_merged$kss), ]

# filter bt_merged and MEDI
bt_merged_filtered <- bt_merged %>%
  rename("datetime" = "datetime.uploaded") %>%
  mutate(date = as.Date(datetime))

## Timestamp for task initiation
# Function that converts dayhour to game playe timestamp
convert_float_to_timestamp <- function(date, hour_float) {
  # Extract the integer part (hours)
  hours <- floor(hour_float)
  
  # Extract the fractional part
  fractional_hour <- hour_float - hours
  
  # Convert fractional part to minutes and seconds
  total_seconds <- round(fractional_hour * 3600)
  minutes <- floor(total_seconds / 60)
  seconds <- total_seconds %% 60
  
  # Create the time string
  time_str <- sprintf("%02d:%02d:%02d", hours, minutes, seconds)
  
  # Combine date and time to form the timestamp
  timestamp <- as.POSIXct(paste(date, time_str), format="%Y-%m-%d %H:%M:%S", tz = 'GMT')
  
  return(timestamp)
}


# Apply the conversion function to the data frame
bt_merged_filtered <- bt_merged_filtered %>%
                      mutate(game_datetime = as.POSIXct(mapply(convert_float_to_timestamp, date, dayhour)))




## MEDI history in the past 24 hour ####
# Initialize the empty data frame to store results
light_summary_df <- data.frame()

# Define the number of hours to look back
hrs_before <- 24

# Create reversed sequence from 24 to 1
reversed_sequence <- rev(1:24)

# Loop through each subject
for(subj_ in unique(sw_data_all$subj)){
  light_subj <- subset(sw_data_all, subj == subj_)
  bt_subj <- subset(bt_merged_filtered, subj == subj_) %>%
             filter(game_datetime >= min(light_subj$datetime) + hrs_before*60*60)
  
  # Loop through all unique time stamps
  for(datetime_num in unique(bt_subj$game_datetime)){
    # Convert datetime_num from numeric to POSIXct
    datetime_ <- as.POSIXct(datetime_num, origin="1970-01-01", tz="GMT")
    
    # Extract data that is 24 hours prior to the time stamp
    light_day <- subset(light_subj, 
                        datetime < datetime_ & 
                          datetime >= datetime_ - hrs_before*60*60) %>%
                  dplyr::select(datetime, Mel)
    
    # Calculate summary statistics without 0.1 records
    logMEDI_mean <- log10(mean(light_day$Mel[light_day$Mel != 0.1], na.rm = TRUE))
    logMEDI_median <- log10(median(light_day$Mel[light_day$Mel != 0.1], na.rm = TRUE))
    logMEDI_sd <- log10(sd(light_day$Mel[light_day$Mel != 0.1], na.rm = TRUE))
    logMEDI_min <- log10(min(light_day$Mel[light_day$Mel != 0.1], na.rm = TRUE))
    logMEDI_max <- log10(max(light_day$Mel[light_day$Mel != 0.1], na.rm = TRUE))
    logMEDI_total <- log10(sum(light_day$Mel[light_day$Mel != 0.1], na.rm = TRUE))
    
    # Calculate hourly averages for the past 24 hours
    light_day <- light_day %>%
      mutate(hour = floor(as.numeric(difftime(datetime, datetime_ - hrs_before*60*60, units = "hours"))))
    
    hourly_mean <- light_day %>%
      group_by(hour) %>%
      summarise(hourly_mean = log10(mean(Mel, na.rm = TRUE)))
    
    # Check if there are any NAs in hourly averages or if the lengths don't match
    if(any(is.na(hourly_mean$hourly_mean)) || nrow(hourly_mean) != length(reversed_sequence)){
      next  # Skip to the next iteration if there's an NA or a length mismatch
    }
    
    # Prepare hourly average and max change columns
    hourly_mean_cols <- setNames(as.list(hourly_mean$hourly_mean), paste0("logMEDI_mean_hour_", reversed_sequence, "_before"))
    
    # Create a data frame with the results for this timestamp
    summary_df <- data.frame(
      subj = subj_,
      game_datetime = datetime_,
      logMEDI_mean = logMEDI_mean,
      logMEDI_median = logMEDI_median,
      logMEDI_sd = logMEDI_sd,
      logMEDI_min = logMEDI_min,
      logMEDI_max = logMEDI_max,
      logMEDI_total = logMEDI_total
    )
    
    # Combine the temp_df with hourly averages and max changes
    summary_df <- cbind(summary_df, as.data.frame(t(hourly_mean_cols)))
    
    # Bind the results to the main data frame
    light_summary_df <- rbind(light_summary_df, summary_df)
  }
}



## Cosinor fit on MEDI history ####
# Initialize an empty list to store results
cos_results <- list()

# Iterate over each row of light_summary_df
for(i in 1:nrow(light_summary_df)) {
  
  # Extract relevant subject and time range
  subj_ <- light_summary_df$subj[i]
  datetime_ <- light_summary_df$game_datetime[i]
  
  # Filter the original data for the past 24 hours for the specific subject
  light_subj <- subset(sw_data_all, subj == subj_)
  light_day <- subset(light_subj, 
                      datetime < datetime_ & 
                      datetime >= datetime_ - hrs_before*60*60  
                      ) %>%
                dplyr::select(datetime, Mel)
  
  # Prepare data for cosinor model
  sub_hist <- log10(light_day$Mel)  # Log-transform the Mel values
  time_numeric <- as.numeric(difftime(light_day$datetime, min(light_day$datetime), units = "secs")) / 3600  # Convert to hours since the start of the 24-hour period
  
  # Handle the case where sub_hist has no data (e.g., all NAs or empty)
  if(length(sub_hist) == 0 || all(is.na(sub_hist))) {
    next  # Skip to the next iteration if there's no valid data
  }
  
  sub_data <- data.frame(sub_hist = sub_hist, time_numeric = time_numeric)
  
  # Fit cosinor model
  cosinor_model <- cosinor.lm(sub_hist ~ time(time_numeric), data = sub_data, period = 24)
  
  # Summary of the fit
  model_summary <- summary(cosinor_model)
  
  # Extract mesor, amplitude, and acrophase from the transformed coefficients section
  mesor <- model_summary$transformed.table["(Intercept)", "estimate"]
  amplitude <- model_summary$transformed.table["amp", "estimate"]
  acrophase <- model_summary$transformed.table["acr", "estimate"]
  
  # Append results to the results list
  cos_results[[i]] <- data.frame(logMEDI_amplitude = amplitude,
                                 logMEDI_mesor = mesor,
                                 logMEDI_acrophase = acrophase
  )
}

# Combine all results into a single data frame
cos_results_df <- do.call(rbind, cos_results) 

# Merge cosinor results to main data
light_summary_with_cos <- cbind(light_summary_df, cos_results_df) %>%
                          mutate(across(contains("MEDI"), as.numeric))



## Merge light history with cognitive task
cog_with_light_df <- bt_merged_filtered %>%
                     left_join(light_summary_with_cos, by = c("subj", "game_datetime"))

# Remove rows with NA values in the "V1" column
cog_with_light_df <- cog_with_light_df %>%
                      filter(!if_any(contains("MEDI"), is.na))
                      

### Data for Analysis ####
# Form training data and test data
bt_with_history <- cog_with_light_df %>%
  mutate(
    date = as.Date(datetime.creat),
    hour = hour(game_datetime)
  ) %>%
  dplyr::select(                       # Extract variables of interest #
                subj,
                game_played,
                kss,                    # Subjective sleepiness #
                falsealarm,
                trialno,
                light.task.log,
                contains("logMEDI_"),
                TPR,                    # Cognitive task metrics # 
                FPR,
                FDR,
                FOR,
                ACC,
                DPRIME,
                medianrt,
                meanrt,
                ies,
                slow10,
                fast10,
                lapse,
                slope,
                date,                    # Time variables #
                hour,
                datetime,
                dayhour,
                game_datetime,
                timeawake
              )


## Imputation ####
# Check NA values
columns_with_na <- colSums(is.na(bt_with_history)) > 0
names(bt_with_history)[columns_with_na]

# Remove instances with NA TPR and medianrt
bt_with_history <- bt_with_history[!is.na(bt_with_history$TPR), ]
bt_with_history <- bt_with_history[!is.na(bt_with_history$medianrt), ]

# Some imputation of NA
bt_with_history$FDR[is.na(bt_with_history$FDR)] <- 100
bt_with_history$FOR[bt_with_history$TPR == 100] <- 0


# Check NA values again
columns_with_na <- colSums(is.na(bt_with_history)) > 0
names(bt_with_history)[columns_with_na]

# Set Subject as a factor column
bt_with_history$subj <- factor(bt_with_history$subj)

# Identify character (factor) columns
char_cols <- sapply(bt_with_history, is.character)

# Initialize label encoding dictionary
bt_encoding_dict <- list()
bt_with_history_encoded <- bt_with_history

# Function to extract numeric part from the string and convert to integer
label_encoder <- function(x) {
  as.integer(sub("P200", "", x))
}

# Label encode 'subj' into numeric id
bt_with_history_encoded <- bt_with_history_encoded %>%
                           mutate(subj = label_encoder(bt_with_history_encoded$subj))


# Identify columns to process
logMEDI_columns <- grep("^logMEDI", names(bt_with_history_encoded), value = TRUE)

# Sort the dataset by 'game_datetime' in ascending order
bt_with_history_encoded <- bt_with_history_encoded %>%
  arrange(subj, game_datetime)


# Forward fill the missing values in 'light.task.log', 'date' and 'kss'
bt_with_history_encoded$light.task.log <- zoo::na.locf(bt_with_history_encoded$light.task.log, na.rm = FALSE)  
bt_with_history_encoded$date <- zoo::na.locf(bt_with_history_encoded$date, na.rm = FALSE)  
bt_with_history_encoded$kss <- zoo::na.locf(bt_with_history_encoded$kss, na.rm = FALSE) 



## Exclusions
# Calculate counts per subject and game
count_df <- bt_with_history_encoded %>%
  group_by(subj, game_played) %>%
  summarise(count = n()) %>%
  ungroup()  # Ungroup to avoid issues in the next step

# Filter the main dataframe to exclude groups with count less than 8
bt_with_history_encoded <- bt_with_history_encoded %>%
  inner_join(count_df %>% filter(count >= 8), by = c("subj", "game_played"))


# save the data for all analysis
save(bt_with_history_encoded, file = "bt_with_history_hour_encoded.RData")
