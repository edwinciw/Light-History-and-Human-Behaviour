# Set working directory and clear workspace
setwd("C:/path/Light-History-and-Human-Behaviour/repository")
rm(list=ls(all=TRUE))

# tell here where we are
here::i_am("repository/RScripts/EDA.R")

# Load libraries ####
library(progress)       # Provides progress bars for long-running operations
library(here)           # Manage paths to files relative to the project root
library(ggplot2)        # Create plots and visualizations
library(ggpubr)         # Convenient functions for adding statistical annotations and combining plots
library(dplyr)          # Data manipulation with pipe (%>%) operator
library(lubridate)      # Date-time manipulation (e.g., extracting weekdays)
library(stringr)        # String manipulation
library(tidyr)          # Data tidying (e.g., pivoting data)
library(purrr)          # Functional programming tools (e.g., map functions)
library(corrplot)       # Plot correlation matrices
library(zoo)            # Imputation and other operations on time-series data
library(data.table)     # Fast data manipulation 
library(viridis)        # Color scales for visualization
library(patchwork)      # Combine multiple ggplots into one
library(LightLogR)      # MEDI plot

# where to get data from an where to output to
input_path_bt <- here("repository","data","processed", "brightertime")
input_path_sw <-  here("repository", "data", "processed", "spectrawear")


# Set the output directory using `here`
output_dir <- here("repository", "figures", "EDA")
dir.create(output_dir, showWarnings = FALSE)  # Create the directory if it doesn't exist

################################################################################
# Spectrawear
load(here(input_path_sw, "spectrawear_long.RData"))

# Pearson correlation
# Compute the correlation matrix
correlation_matrix <- cor(select_if(sw_data_all, is.numeric))

# Set up the plotting area to combine all plots
jpeg(filename = file.path(output_dir, "spectrawear_corr.jpg"), 
     width = 765, height = 632, quality = 95)  # Adjust dimensions and quality as needed

# Plot the heat map
corrplot(correlation_matrix, method = "circle", type = "upper", diag = FALSE,
         tl.col = "black", tl.srt = 45, 
         col = colorRampPalette(c("blue", "white", "red"))(200)) # Color palette

# Close the JPEG device to save the file
dev.off()

# Plot the MEDI of each subject
for (subject in unique(sw_data_all$subj)){
  
  sub_df <- sw_data_all %>%
            subset(subj == subject) %>%
            dplyr::select(
              datetime,
              Mel
            )
  
  p <-gg_day(sub_df,
            x.axis = datetime,
            y.axis = Mel,
            aes_col = Mel > 250,
            geom = "point",
            y.scale = "symlog",
            x.axis.label = "Time of Day",
            y.axis.label = "Melanopic Illuminance",
            title = subject)

  # Save the ggplot2 plot as a JPG in the specified folder
  file_name = paste0(subject, ".jpg")
  ggsave(file.path(here("repository", "figures", "MEDI_plots"), file_name), plot = p, width = 9, height = 10, dpi = 300)
}

## Boxplots to subjects' logMEDI
# Create a mapping of labels to numeric values
label_mapping <- setNames(1:60, paste0("P200", sprintf("%02d", 1:60)))

# Add a new column with numeric encoding
sw_data_all$subj_encoded <- label_mapping[sw_data_all$subj]
sw_data_all$subj_encoded <- factor(sw_data_all$subj_encoded)
sw_data_all$logMEDI <- sapply(sw_data_all$Mel, log10)

# Filter out observations with -1 loglux
sw_sub <- sw_data_all %>%
          filter(logMEDI != -1)

# Create the boxplot for logMEDI
Mel_p <- ggplot(sw_sub, aes(x = subj_encoded, y = logMEDI)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplots of logMEDI during wake across subjects",
       x = "Subject",
       y = "logMEDI") +
  theme_minimal()

# Save the ggplot2 plot as a JPG in the specified folder
file_name <- "Subject_MEDI_wake.jpg"
ggsave(file.path(here("repository", "figures", "EDA"), file_name), plot = Mel_p, width = 10, height = 6, dpi = 300)

################################################################################
# Brighter Time data
load(here(input_path_bt, "brightertime_kss.RData")) #bt_kss
load(here(input_path_bt, "brightertime_baseline.RData")) #bt_baseline
load(here(input_path_bt, "brightertime_cognitive_variables.RData")) #bt_cognitive

## Boxplots to subjects' KSS
# Label encoding
# Create a mapping of labels to numeric values
label_mapping <- setNames(1:60, paste0("P200", sprintf("%02d", 1:60)))

# Add a new column with numeric encoding
bt_kss$subj_encoded <- label_mapping[bt_kss$subj]
bt_kss$subj_encoded <- factor(bt_kss$subj_encoded)

# Create the boxplot for kss
kss_p <- ggplot(bt_kss, aes(x = subj_encoded, y = kss)) +
            geom_boxplot() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(title = "KSS boxplots for 60 Subjects",
                 x = "Subject",
                 y = "KSS") +
            theme_minimal()

# Save the ggplot2 plot as a JPG in the specified folder
file_name <- "KSS boxplots for 60 Subjects.jpg"
ggsave(file.path(here("repository", "figures", "EDA"), file_name), plot = kss_p, width = 10, height = 6, dpi = 300)


# Create hour column
bt_kss$hour <- factor(hour(bt_kss$datetime.creat))

# Create the boxplot for kss across hours
kss_p <- ggplot(bt_kss, aes(x = hour, y = kss)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "KSS boxplots across the day",
       x = "Hour",
       y = "KSS") +
  theme_minimal()

# Save the ggplot2 plot as a JPG in the specified folder
file_name <- "KSS boxplots across the day.jpg"
ggsave(file.path(here("repository", "figures", "EDA"), file_name), plot = kss_p, width = 10, height = 6, dpi = 300)


## bt baseline barcharts ####
# Age
age_count <- bt_baseline %>%
  count(age) %>%
  ggplot(aes(x = age, y = n, fill = age)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "cividis") +  # Use a color palette with distinct colors
  labs(title = "Age",
       x = "Age",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.5),
        legend.position = "none")  # Hide legend

# Sex
# Calculate the counts
patient_counts <- bt_baseline %>%
  mutate(
    any_disease = if_else(!is.na(mentaldis) | !is.na(braindis), "Has Disorder", "No Disorder"),
    all_na = if_else(is.na(mentaldis) & is.na(braindis), "Asymptomatic", "Not Asymptomatic")
  ) %>%
  group_by(any_disease, all_na, sex) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(category = case_when(
    any_disease == "Has Disorder" ~ "Has Disorder",
    all_na == "Asymptomatic" ~ "Asymptomatic",
    TRUE ~ "Other"
  )) %>%
  dplyr::select(
    category, 
    sex, 
    count) %>%
  pivot_wider(names_from = sex, values_from = count, values_fill = list(count = 0)) %>%
  arrange(category) %>%
  pivot_longer(cols = c(Male, Female), names_to = "sex", values_to = "count") %>%
  filter(count > 0) 

# Plot the results
sex_count <- ggplot(patient_counts, aes(x = category, y = count, fill = sex)) +
              geom_bar(stat = "identity", position = "dodge") +
              labs(title = "Gender and disorder",
                   x = "Disorder Status",
                   y = "Count") +
              scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 30, vjust = 0.8, hjust = 0.5))

# Chronotype
chronotype_count <- bt_baseline %>%
  count(chronotype) %>%
  ggplot(aes(x = chronotype, y = n, fill = chronotype)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  # Use a color palette with distinct colors
  labs(title = "",
       x = "Chronotype",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Occupation
# Replace NA values with 'Unknown'
bt_baseline <- bt_baseline %>%
  mutate(occup_modified = ifelse(is.na(occup), "Unknown", as.character(occup))) %>%
  separate(occup_modified, into = c("occup_modified", "rest"), sep = "\\|", extra = "drop", fill = "right") %>%
  mutate(occup_modified = str_to_title(occup_modified)) %>%
  dplyr::select(-rest) %>%
  mutate(occup_modified = case_when(
    str_detect(occup_modified, "Technician|Develop") ~ "Technician",
    str_detect(occup_modified, "Research") ~ "Researcher",
    str_detect(occup_modified, "Teaching|Ta") ~ "Teaching Assistant",
    str_detect(occup_modified, "Student") ~ "Student",
    str_detect(occup_modified, "Nurs|Psycho") ~ "Medical Related",
    str_detect(occup_modified, "Proj|Ad|Visitor") ~ "Others",
    TRUE ~ occup_modified
  ))

# Convert back to factor
bt_baseline$occup_modified <- factor(bt_baseline$occup_modified)

# Calculate the count for each unique factor
occup_count <- bt_baseline %>%
  count(occup_modified) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = occup_modified, y = n, fill = occup_modified)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "cividis") +  # Use a color palette with distinct colors
  labs(title = "Occupations",
       x = "Occupations",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Combine all plots using patchwork
combined_plots <- (sex_count | chronotype_count) / (age_count | occup_count)
  

# Print combined plots
print(combined_plots)

# Save the ggplot2 plot as a JPG in the specified folder
file_name <- "Demographic.jpg"
ggsave(file.path(here("repository", "figures", "EDA"), file_name), plot = combined_plots, width = 8, height = 6, dpi = 600)


## bt_cognitive unique session order
game_order <- bt_cognitive %>%
  dplyr::select(
    session_order
  )
  
  # Print all unique session order
  unique_values <- unique(game_order$session_order)
print(unique_values)



## Working data ####
# Load data set
load(here(here("repository", "data", "working_data"), "bt_with_history_hour_encoded.RData"))

# Split the data into a list of data frames by game_played
game_split <- split(bt_with_history_encoded, bt_with_history_encoded$game_played)

pvt_df <- game_split[["PVT"]]
vs_df <- game_split[["VisualSearch"]]
# Determine the grid layout based on the number of games
num_games <- length(game_split)
num_cols <- ceiling(sqrt(num_games))  # Number of columns
num_rows <- ceiling(num_games / num_cols)  # Number of rows

# Set up the plotting area to combine all plots
jpeg(filename = file.path(output_dir, "combined_corr_plots_summary_with_rt.jpg"), 
     width = 765, height = 632, quality = 95)  # Adjust dimensions and quality as needed

par(mfrow = c(num_rows, num_cols))  # Adjusts the grid layout
par(mar = c(5, 5, 1, 1))  # Adjust margins to fit title at bottom left

# Loop through each group and plot the correlation matrix
for (game in names(game_split)) {
  # Extract the subset of data for the current game
  game_data <- game_split[[game]]
  
  if (game == "PVT") {
    visualise_df <- game_data %>%
      dplyr::select(
        contains('logMEDI'),
        'meanrt',
        'medianrt',
        'slow10',
        'fast10',
        'TPR',
        'FDR',
        'ACC',
        'ies',
        'lapse'
      ) %>%
      dplyr::select(-contains('before')) %>%
      na.omit() %>%
      rename_with(~ gsub('logMEDI_', '', .), starts_with('logMEDI_')) 
  } else if (game == 'VisualSearch'){
    visualise_df <- game_data %>%
      dplyr::select(
        contains('logMEDI'),
        'meanrt',
        'medianrt',
        'slow10',
        'fast10',
        'TPR',
        'FPR',
        'FOR',
        'FDR',
        'ACC',
        'ies',
        'DPRIME',
        'slope'
      ) 
  } else {
    visualise_df <- game_data %>%
      dplyr::select(
        contains('logMEDI'),
        'meanrt',
        'medianrt',
        'slow10',
        'fast10',
        'TPR',
        'FPR',
        'FOR',
        'FDR',
        'ACC',
        'ies',
        'DPRIME'
      )
  }
  
  visualise_df <- visualise_df %>%
                  dplyr::select(-contains('before')) %>%
                  na.omit() %>%
                  rename_with(~ gsub('logMEDI_', '', .), starts_with('logMEDI_')) 
  
  
  # Select only numeric columns
  numeric_data <- select_if(visualise_df, is.numeric)
  
  # Check if there are enough columns for a valid correlation matrix
  if (ncol(numeric_data) > 1) {
    # Calculate the Pearson correlation matrix with pairwise complete observations
    corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "pearson")
    
    # Plot the correlation matrix without the diagonal
    corrplot(corr_matrix, method = "circle", type = "upper", diag = FALSE, 
             title = "", tl.srt = 45, tl.col = 'black')
    
    # Manually add the title at the bottom-left
    mtext(paste("Correlation Plot: ", game), side = 1, line = 3, cex = 0.8, font = 2, adj = 0)
  } else {
    message(paste("Not enough numeric columns for game:", game))
  }
}

# Close the JPEG device to save the file
dev.off()

# Reset the plotting area to default
par(mfrow = c(1, 1))


# Adjust margins to fit title at bottom left
par(mar = c(5, 10, 10, 10))  

# Set up the plotting area to combine all plots
jpeg(filename = file.path(output_dir, "past_MEDI.jpg"), 
     width = 765, height = 632, quality = 95)  # Adjust dimensions and quality as needed

## MEDI history correlation
MEDI_history <- bt_with_history_encoded %>%
                dplyr::select(
                  contains('before')
                ) %>%
                rename_with(~ gsub('logMEDI_mean_', '', .), starts_with('logMEDI_mean_'))




# Calculate the Pearson correlation matrix with pairwise complete observations
corr_matrix <- cor(MEDI_history, use = "pairwise.complete.obs", method = "pearson")


# Plot the correlation matrix without the diagonal
corrplot(corr_matrix, method = "circle", type = "upper", diag = FALSE, 
           title = "", tl.srt = 90, tl.col = "black") +
           theme_minimal() +
           theme(axis.text.x = element_blank())

# Manually add the title at the bottom-left
mtext('Past hourly MEDI correlation', side = 1, line = 3, cex = 0.8, font = 2, adj = 0)


# Close plotting area
dev.off()

