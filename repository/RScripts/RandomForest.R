# Set working directory and clear workspace
setwd("C:/path/Light-History-and-Human-Behaviour/repository")
rm(list=ls(all=TRUE))

# tell here where we are
here::i_am("repository/RScripts/RandomForest.R")

# Load packages
library(here)          # Helps manage file paths relative to the project root
library(ggplot2)       # Used for creating plots and visualizations
library(ggpubr)        # Provides easy-to-use functions for publication-ready plots (not used in this script)
library(gridExtra)     # Used for arranging multiple ggplot objects into a grid layout
library(readr)         # Efficiently reads data into R
library(dplyr)         # Provides functions for data manipulation and cleaning
library(lubridate)     # Handles date-time manipulation (not used in this script)
library(foreach)       # Facilitates parallel processing (not used in this script)
library(doParallel)    # Provides parallel backend support for foreach (not used in this script)
library(progress)      # Provides progress bars for long-running operations (not used in this script)
library(randomForest)  # Implements the random forest algorithm for classification and regression
library(caret)         # Provides tools for training and evaluating machine learning models
library(data.table)    # Efficient for handling and manipulating large datasets
library(xtable)        # Converts R objects to LaTeX or HTML tables (not used in this script)

# Load data set
load(here(here("repository", "data", "working_data"), "bt_with_history_hour_encoded.RData"))

# Remove all correlated variables
bt_with_history_encoded <- bt_with_history_encoded %>%
  dplyr::select(
    -matches("^logMEDI_mean_hour_1[2-9]_before$|^logMEDI_mean_hour_2[0-4]_before$")
  ) %>%
  dplyr::select(
    -'logMEDI_sd',
    -'logMEDI_total'
  )



## FUNCTIONS ####
# Function for feature and target selection
feature_select <- function(df, target, subj_included = FALSE, game_included = FALSE,
                           summary_included = TRUE, hourly_included = FALSE) {
  

  # Start by selecting all variables of interest
  selected_df <- df %>%
    dplyr::select(
      all_of(target), 
      subj, 
      game_played,
      starts_with("logMEDI_"), 
      contains("before")
      )
  
  # Remove 'subj' column if subj_included is FALSE
  if (!subj_included) {
    selected_df <- selected_df %>%
      dplyr::select(-subj)
  }
  
  # Remove 'subj' column if subj_included is FALSE
  if (!game_included) {
    selected_df <- selected_df %>%
      dplyr::select(-game_played)
  }
  
  # Remove MEDI summary columns if summary_included is FALSE
  if (!summary_included) {
    # Keep history columns (those containing "before")
    # Identify summary columns by their prefix and exclude them if they are not history columns
    summary_columns <- selected_df %>%
      dplyr::select(starts_with("logMEDI_")) %>%
      dplyr::select(-contains("before")) %>%
      names()
    
    selected_df <- selected_df %>%
      dplyr::select(-all_of(summary_columns))
  }
  
  # Remove MEDI hourly mean
  if (!hourly_included) {
    selected_df <- selected_df %>%
      dplyr::select(-contains("before"))
  }
  
  # Remove rows with NA values
  selected_df <- selected_df %>%
    na.omit()
  
  return(selected_df)
}


# Function to compute RMSE and R^2
compute_metrics <- function(model, data, target) {
  predictions <- predict(model, data)
  rmse <- sqrt(mean((data[[target]] - predictions)^2))
  r2 <- postResample(pred = predictions, obs = data[[target]])["Rsquared"]
  return(list(rmse = rmse, r2 = r2))
}

# Refactored train_random_forest function
train_random_forest <- function(df, targets, subj_included, game_included, summary_included, hourly_included, ntree_opt = 100, mtry_opt = NULL) {
 
  # Set up storage for results
  all_results <- data.table()
  all_importances <- data.table()
  
  # Loop through each target
  for (target in targets) {
    
    # Select features
    feature_df <- feature_select(df, target, subj_included, game_included, summary_included, hourly_included)
    
    # Entire data check
    if (ncol(feature_df) <= 1) {
      warning(paste("No valid predictors found for target:", target))
      next
    }
    
    # Stratified train-test split based on target
    set.seed(123)
    training_indices <- createDataPartition(feature_df[[target]], p = 0.8, list = FALSE)
    tar_train <- feature_df[training_indices, ]
    tar_test <- feature_df[-training_indices, ]
    
    # Default maximum features select for each split
    if (is.null(mtry_opt)) {
      mtry_opt <- floor(ncol(tar_train) / 3)
    }
    
    
    # Train Random Forest
    set.seed(123)
    final_model <- randomForest(as.formula(paste(target, "~ .")), data = tar_train, mtry = mtry_opt, ntree = ntree_opt, importance = TRUE)
    
    
    # Compute the RMSE and R2 for train and test sets
    train_metrics <- compute_metrics(final_model, tar_train, target)
    test_metrics <- compute_metrics(final_model, tar_test, target)
    
    
    # Store the metrics in a dataframe
    opt_res <- data.table(target = target, 
                          test_RMSE = test_metrics$rmse, train_RMSE = train_metrics$rmse, 
                          test_r2 = test_metrics$r2, train_r2 = train_metrics$r2)
    all_results <- rbind(all_results, opt_res)
    
    
    # Extract feature importance of the Random Forest and store it in a dataframe
    importance_df <- as.data.frame(importance(final_model, type = 1))
    importance_df$feature <- rownames(importance_df)
    importance_df$target <- target
    all_importances <- rbind(all_importances, importance_df)
  }
  
  
  # Return all results
  return(list(results = all_results, feature_importances = all_importances))
}



# Function to train Random Forests for each game/task sequentially
train_rf_for_each_game <- function(df_split, subj_included, summary_included, hourly_included, ntree_opt = 100, mtry_opt = NULL) {
  # Initialize result storage
  combined_results <- list()
  
  # Loop through each game
  for (game in names(df_split)) {
    df <- df_split[[game]]
    
    if (game == "PVT") {
      targets <- c("TPR", "FDR", "ACC", "medianrt", "meanrt", "slow10", "fast10", "lapse")
    } else if (game == "VisualSearch") {
      targets <- c("TPR", "FPR", "FOR", "FDR", "ACC", "medianrt", "meanrt", "slow10", "fast10", "slope")
    } else {
      targets <- c("TPR", "FPR", "FOR", "FDR", "ACC", "medianrt", "meanrt", "slow10", "fast10", "DPRIME")
    }
    
    # Train the random forest model for the current game
    rf_output <- train_random_forest(df, targets, subj_included, game_included = FALSE, summary_included, hourly_included, ntree_opt = 100, mtry_opt)
    
    # Combine results
    game_results <- data.table(game = game, rf_output$results)
    game_importances <- data.table(game = game, rf_output$feature_importances)
    
    # Store results in a list
    combined_results[[game]] <- list(
      results = game_results,
      importances = game_importances
    )
    
  }
  # Combine all results dataframes into a single dataframe
  all_results <- rbindlist(lapply(combined_results, function(x) x$results), fill = TRUE)
  
  # Combine all importances dataframes into a single dataframe
  all_importances <- rbindlist(lapply(combined_results, function(x) x$importances), fill = TRUE)
  
  return(list(results = all_results, feature_importances = all_importances))
}



# Function to train both all data and each game
train_rf_process <- function(df, df_split, targets, subj_included = FALSE, summary_included = TRUE, hourly_included = FALSE, 
                             ntree_opt = 100, mtry_opt = NULL) {
  
  # Train Random Forests with the entire data
  all_data_rf <- train_random_forest(df, targets, subj_included, game_included = TRUE, summary_included, hourly_included, 
                                     ntree_opt, mtry_opt)
  
  
  # Train Random Forests for each game/task
  each_game_rf <- train_rf_for_each_game(df_split, subj_included, summary_included, hourly_included, ntree_opt, mtry_opt)
  
  
  return(list(all_data_rf = all_data_rf, each_game_rf = each_game_rf))
}



# Function to create bar plots for feature importances
plot_feature_importances <- function(all_data_rf) {
  
  # Extract the feature_importances dataframe
  feature_importances <- all_data_rf$feature_importances
  
  # Remove 'logMEDI_' prefix from feature names
  feature_importances$feature <- gsub("^logMEDI_", "", feature_importances$feature)
  
  # Create a list to store ggplot objects
  plot_list <- list()
  
  # Get unique targets
  targets <- unique(feature_importances$target)
  
  # Loop through each target and create a plot
  for (target in targets) {
    # Filter the data for the current target
    target_data <- feature_importances %>%
      filter(target == !!target) %>%
      arrange(desc(`%IncMSE`))
    
    # Create the bar plot
    plot <- ggplot(target_data, aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
                     geom_bar(stat = "identity", fill = "steelblue") +
                     coord_flip() +
                     labs(title = paste("Feature Importances for", target),
                          x = "Feature",
                          y = "% IncMSE") +
                     theme_minimal() +
                     theme(axis.text.y = element_text(size = 10, face = "bold")) # Ensure feature names are readable
                   
                   # Add the plot to the list
                   plot_list[[target]] <- plot
  }
  
  # Arrange all plots in a grid
  do.call(grid.arrange, c(plot_list, ncol = 2))
}



# Function to create grouped bar plots for feature importances by game task
plot_feature_importances_by_game <- function(each_game_rf) {
  
  # Extract the feature_importances dataframe
  feature_importances <- each_game_rf$feature_importances
  
  # Remove 'logMEDI_' prefix from feature names
  feature_importances$feature <- gsub("^logMEDI_", "", feature_importances$feature)
  
  # Create a list to store ggplot objects for each game
  plot_list <- list()
  
  # Get unique games
  games <- unique(feature_importances$game)
  
  # Loop through each game
  for (game in games) {
    # Filter the data for the current game
    game_data <- feature_importances %>%
      filter(game == !!game) 
    
    # Get unique targets within this game
    targets <- unique(game_data$target)
    
    # Create a temporary list to store plots for each target in the game
    temp_plot_list <- list()
    
    # Loop through each target within the game
    for (target in targets) {
      # Filter the data for the current target within the current game
      target_data <- game_data %>%
        filter(target == !!target) %>%
        arrange(desc(`%IncMSE`))
      
      # Create the bar plot
      plot <- ggplot(target_data, aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = paste(game, "-", target),
             x = "Feature",
             y = "% IncMSE") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 10, face = "bold"))
      
      # Add the plot to the temporary list, using the target as the name
      temp_plot_list[[target]] <- plot
    }
    
    # Combine the plots for this game into a grid
    plot_list[[game]] <- arrangeGrob(grobs = temp_plot_list, nrow = 2, ncol = 5)
  }
  
  return(plot_list)
}



# Function to group, display, and save plots by task
plot_and_save_task_importances <- function(task_plots_list, save_dir = "figures/Feature_Importance_Plots", base_width = 8, base_height_per_plot = 2.5) {
  
  # Create the save directory if it doesn't exist
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Extract unique task names
  task_names <- unique(sapply(names(task_plots_list), function(x) strsplit(x, "_")[[1]][1]))
  
  # Loop through each task
  for (task in task_names) {
    # Extract the plots for the current task
    task_plots <- task_plots_list[grepl(task, names(task_plots_list))]
    
    # Combine the plots into a grid
    grid_plot <- grid.arrange(grobs = task_plots, ncol = 1, top = paste("Feature Importances for", task))
    
    # Display the grid plot
    print(grid_plot)
    
    # Calculate the height based on the number of plots
    plot_height <- base_height_per_plot * length(task_plots)
    
    # Save the plot to a file with dynamic sizing
    ggsave(filename = file.path(save_dir, paste0(task, "_feature_importances.png")),
           plot = grid_plot, width = base_width, height = plot_height, units = "in")
    
    # Pause to allow viewing in the console
    Sys.sleep(2)
  }
}



# Function to reorder columns and output LaTeX table with the specific format
print_latex_table <- function(result_df, game_name = "", caption = "") {
  
  if (game_name != ""){
    # Filter the dataframe for the specified game name
    result_df <- result_df %>%
      filter(game == game_name)
  }
  
  # Reorder and select the necessary columns
  result_df_reordered <- result_df %>% 
    dplyr::select(target, test_RMSE = test_RMSE, train_RMSE = train_RMSE, test_r2 = test_r2, train_r2 = train_r2)
  
  # Start building the LaTeX table
  latex_code <- paste0(
    "\\begin{subtable}{\\textwidth}\n",
    "    \\centering\n",
    "    \\caption{", caption, "}\n",
    "    \\begin{tabular}{|l|c|c|c|c|}\n",
    "    \\hline\n",
    "    & \\multicolumn{2}{|c|}{RMSE} & \\multicolumn{2}{|c|}{R$^2$}\\\\ \\hline\n",
    "    target & test & train & test & train \\\\ \n",
    "    \\hline\n"
  )
  
  # Add rows to the table
  for (i in 1:nrow(result_df_reordered)) {
    latex_code <- paste0(
      latex_code,
      result_df_reordered$target[i], " & ",
      formatC(result_df_reordered$test_RMSE[i], format = "f", digits = 2), " & ",
      formatC(result_df_reordered$train_RMSE[i], format = "f", digits = 2), " & ",
      formatC(result_df_reordered$test_r2[i], format = "f", digits = 2), " & ",
      formatC(result_df_reordered$train_r2[i], format = "f", digits = 2), " \\\\ \\hline\n"
    )
  }
  
  # Close the table
  latex_code <- paste0(
    latex_code,
    "    \\end{tabular}\n",
    "\\end{subtable} \n"
  )
  
  # Print the LaTeX code
  cat(latex_code)
}



#####################################################
## Results ####
# Define the list of targets
targets <- c("TPR", "FDR", "ACC", "medianrt", "meanrt", "slow10", "fast10")

# Split each game into different dataset
df_split <- split(bt_with_history_encoded, bt_with_history_encoded$game_played)


######################################################
## MEDI hourly means ####
rf_hourly_mean <- train_rf_process(bt_with_history_encoded, df_split, targets, summary_included = FALSE, hourly_included =  TRUE)

## Results tables
rf_hourly_mean_result_df <- rf_hourly_mean$all_data_rf$results
rf_hourly_mean_task_result_df <- rf_hourly_mean$each_game_rf$results

# Print latex table for results
print_latex_table(rf_hourly_mean_result_df)
print_latex_table(rf_hourly_mean_task_result_df, "PVT")
print_latex_table(rf_hourly_mean_task_result_df, "TwoBackGold")
print_latex_table(rf_hourly_mean_task_result_df, "ThreeBackGold")
print_latex_table(rf_hourly_mean_task_result_df, "VisualSearch")

## Feature importance plots
rf_hourly_mean_plots <- plot_feature_importances(rf_hourly_mean$all_data_rf)
rf_hourly_mean_task_plots <- plot_feature_importances_by_game(rf_hourly_mean$each_game_rf)

# Save and plot the feature importance for RF of each task
plot_and_save_task_importances(rf_hourly_mean_task_plots, save_dir = "figures/Feature_Importance_Plots/RF_with_past_MEDI_hourly_mean", base_width = 20, base_height_per_plot = 5)


#######################################################
## Subj with MEDI summaries and cosinor parameters ####
rf_subj_with_summary_and_cosinor <- train_rf_process(bt_with_history_encoded, df_split, targets, subj_included = TRUE, hourly_included = FALSE)

## Results tables
rf_subj_with_summary_and_cosinor_result_df <- rf_subj_with_summary_and_cosinor$all_data_rf$results
rf_subj_with_summary_and_cosinor_task_result_df <- rf_subj_with_summary_and_cosinor$each_game_rf$results

# Print latex table for results
print_latex_table(rf_subj_with_summary_and_cosinor_result_df)
print_latex_table(rf_subj_with_summary_and_cosinor_task_result_df, "PVT")
print_latex_table(rf_subj_with_summary_and_cosinor_task_result_df, "TwoBackGold")
print_latex_table(rf_subj_with_summary_and_cosinor_task_result_df, "ThreeBackGold")
print_latex_table(rf_subj_with_summary_and_cosinor_task_result_df, "VisualSearch")

## Feature importance plots
rf_subj_with_summary_and_cosinor_plots <- plot_feature_importances(rf_subj_with_summary_and_cosinor$all_data_rf)
rf_subj_with_summary_and_cosinor_task_plots <- plot_feature_importances_by_game(rf_subj_with_summary_and_cosinor$each_game_rf)

# Save and plot the feature importance for RF of each task
plot_and_save_task_importances(rf_subj_with_summary_and_cosinor_task_plots, save_dir = "figures/Feature_Importance_Plots/RF_with_subj_and_MEDI_summaries", base_width = 20, base_height_per_plot = 5)


