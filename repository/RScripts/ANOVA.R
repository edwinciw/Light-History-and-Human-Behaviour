# Set working directory and clear workspace
setwd("C:/path/Light-History-and-Human-Behaviour/repository")
rm(list=ls(all=TRUE))

# Specify the current script location
here::i_am("repository/RScripts/ANOVA.R")

# Load libraries ####
library(here)        # Manages file paths relative to the project root
library(ggplot2)     # Creates plots and visualizations
library(ggpubr)      # Enhances ggplot2 functionality with additional tools
library(readr)       # Reads data from files into R
library(dplyr)       # Handles data manipulation tasks
library(lubridate)   # Facilitates date-time operations
library(reshape2)    # Reshapes and manipulates data formats
library(patchwork)   # Combines multiple ggplot2 plots into a single layout
library(lsr)         # Provides functions for Levene's test to check homogeneity of variances
library(multcomp)    # Implements procedures for multiple comparisons in statistical tests
library(broom)       # Tidies up model outputs for easier analysis
library(tidyr)       # Cleans and tidies data for analysis
library(purrr)       # Applies functional programming techniques for data manipulation

# Load data set
load(here(here("repository", "data", "working_data"), "bt_with_history_hour_encoded.RData"))

# Subset all unique MEDI light histories
kss_df <- bt_with_history_encoded %>%
          dplyr::select(
            subj,
            kss,
            hour,
            dayhour,
            contains("MEDI_")
          ) %>%
          filter(!is.na(kss)) %>%
          mutate( 
            subj = factor(subj),
            kss = factor(kss),
            hour = factor(hour)
            ) %>%
          distinct() %>%
          dplyr::select(          # Remove correlated variables.
            -'logMEDI_sd',
            -'logMEDI_total',
            -'logMEDI_acrophase'   # Remove none normal variables.
          )


# Distinct counts
diff_kss_count <- kss_df %>%
  group_by(kss)%>%
  summarise(count = n())


# Subject and KSS group count
diff_subj_kss_count <- kss_df %>%
  group_by(subj,kss)%>%
  summarise(count = n())



## Balance KSS group size
# Define a function to map old categories to new ones
reassign_categories <- function(kss) {
 if (kss %in% 9:10) {
    return('9 ~ 10')
  } else {
    return(as.character(kss))
  }
}

# Apply Function
kss_df$kss <- sapply(kss_df$kss, reassign_categories)



## Boxplots of MEDI summaries and cosinor parameters by KSS 
# Create the directory if it doesn't exist
if (!dir.exists("figures/Light_History_By_KSS")) {
  dir.create("figures/Light_History_By_KSS", recursive = TRUE)
}


# Select columns containing "MEDI_"
MEDI_only_df <- kss_df %>%
  dplyr::select(
    contains("MEDI_"),
    -contains("before")
  ) 

# Loop through each MEDI variable
for (var in colnames(MEDI_only_df)) {
  
  # Set the label of the x-axis
  x_axis = paste0(var, " (loglux)")
  
  
  # Create histogram
  g <- ggplot(kss_df, aes(x = .data[[var]])) +
    geom_histogram(bins = 20, fill = "skyblue", color = "black") +
    facet_wrap(~ kss, scales = "free", ncol = 3) +  # Create grid plot with KSS groups
    labs(
      title = paste0("Histogram for each KSS score"),
      x = x_axis,
      y = "Count"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))  # Bold title
  
  # Save each plot to the specified directory
  file_name <- paste0("figures/Light_History_By_KSS/Histogram_", var, ".png")
  ggsave(file_name, plot = g, width = 10, height = 8, dpi = 300)
  
  # Print the plot
  print(g)
}



## KSS ANOVA ####
# Initialize lists to store results
kss_anova_results_list <- list()

# Loop through each variable in the MEDI dataframe
for (var in names(MEDI_only_df)) {
  

  ## Assumption check for ANOVA test
  # Check Normality for each group
  group_data <- kss_df %>%
                dplyr::select(
                  kss,
                  all_of(var)
                ) %>%
                group_by(kss) %>%
                nest() %>%
                mutate(        # Shapiro-Wilk test
                  shapiro_test = map(data, ~ shapiro.test(.x[[var]])$p.value)
                  )
              
  # Subset groups that pass the normality test (p-value > 0.05)
  valid_groups <- group_data %>%
                  filter(shapiro_test > 0.05) %>%
                  pull(kss)
 
  # Filter the original data to include only valid groups
  filtered_data <- kss_df %>%
                   filter(kss %in% valid_groups) 
  
  # Only proceed if there are at least two groups remaining
  if (length(unique(filtered_data$kss)) > 1) {
    
    
    ## Check Homogeneity of Variance
    # Perform Levene's Test for Homogeneity of Variances on the filtered data
    levene_test <- leveneTest(formula, data = filtered_data)
    
    # Decide which ANOVA to use based on Levene's test result
    if (levene_test$`Pr(>F)`[1] > 0.05) {
      
      # Set the formula for ANOVA
      formula <- as.formula(paste(var, "~ kss"))
      print(formula)
      
      # Perform Standard ANOVA
      anova_result <- aov(formula, data = filtered_data)
      anova_summary <- summary(anova_result)
      
      # Convert ANOVA summary to data frame
      anova_df <- as.data.frame(anova_summary[[1]])
      anova_df$Variable <- var  # Add a column indicating the variable
      
      # Store the data frame in the list
      kss_anova_results_list[[var]] <- anova_df
      
    } 
  } else {
    print(paste("Not enough valid groups satisfying normality for", var))
  }
}

# Combine all ANOVA results into a single data frame
kss_anova_combined_df <- do.call(rbind, kss_anova_results_list)



