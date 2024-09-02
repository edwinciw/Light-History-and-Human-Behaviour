# Set working directory and clear workspace
setwd("C:/Users/edwin/OneDrive - The University of Manchester/Data_Science/Msc_Data_Science/Extended Research Project/melanopsin-IRL-Iok-dev/repository")
rm(list=ls(all=TRUE))

# tell here where we are
here::i_am("repository/RScripts/Linear_Mixed_Models.R")

# Load libraries ####
library(here)        # Manages file paths relative to the project root
library(ggplot2)     # Creates visualizations and plots
library(ggpubr)      # Enhances ggplot2 with additional functionalities
library(readr)       # Reads data from external files into R
library(dplyr)       # Performs data manipulation tasks
library(patchwork)   # Combines multiple ggplot2 plots into a unified layout
library(xtable)      # Converts R data frames to LaTeX tables for publication
library(multcomp)    # Conducts multiple comparison procedures in statistical tests
library(broom)       # Tidies up statistical model outputs for easier interpretation
library(tidyr)       # Cleans and tidies data to make it ready for analysis
library(purrr)       # Applies functional programming techniques to data manipulation
library(lme4)        # Fits Linear Mixed Models for analyzing data with multiple levels
library(lmerTest)    # Provides additional functions for testing linear mixed models
library(fs)          # Facilitates filesystem operations, such as file and directory handling


# Load data set
load(here(here("repository", "data", "working_data"), "bt_with_history_hour_encoded.RData"))

## Calcuate metrics ####
# calculate False Alarm Rate
bt_with_history_encoded$FAR <- (bt_with_history_encoded$falsealarm / bt_with_history_encoded$trialno)*100

# calculate cosine and sine term 
bt_with_history_encoded$cosine <- cos(2*pi*bt_with_history_encoded$dayhour / 24)
bt_with_history_encoded$sine <- sin(2*pi*bt_with_history_encoded$dayhour / 24)



## Split dataset
# Split the dataset by cognitive tasks 
df_split <- split(bt_with_history_encoded, bt_with_history_encoded$game_played)

# Extract variable of interest for each task
PVT_df <- df_split$PVT %>%
          dplyr::select(
            subj,
            kss,
            timeawake,
            light.task.log,
            cosine,
            sine,
            hour,
            medianrt,
            lapse,
            ACC,
            FAR,
            ies
          )

N_back_df <- rbind(df_split$TwoBackGold) %>%
             dplyr::select(
               subj,
               kss,
               timeawake,
               light.task.log,
               cosine,
               sine,
               hour,
               medianrt,
               DPRIME,
               ACC,
               FAR,
               ies
             )
VisualSearch_df <- df_split$VisualSearch %>%
                  dplyr::select(
                    subj,
                    kss,
                    timeawake,
                    light.task.log,
                    cosine,
                    sine,
                    hour,               
                    medianrt,
                    slope,
                    ACC,
                    FAR,
                    ies
                  )

# Create a list and store the extracted data
df_split <- list(
  PVT = PVT_df,
  `2-back` = N_back_df,
  VisualSearch = VisualSearch_df
)


# Function to select the targets from each task data
feature_select <- function(df, game){
  if (game == "PVT"){
    selected_df <- df %>%
                    dplyr::select(
                      medianrt,
                      lapse,
                      ACC,
                      FAR,
                      ies
                    )
  } else if (game == "VisualSearch"){
    selected_df <- df %>% 
                  dplyr::select(
                    medianrt,
                    slope,
                    ACC,
                    FAR,
                    ies
                  )
  } else {
    selected_df <- df %>%
                    dplyr::select(
                      medianrt,
                      DPRIME,
                      ACC,
                      FAR,
                      ies
                    )
  }
  
  return(selected_df)
}


## Linear Mixed Model ####
kss_results <- list()
awake_results <- list()
trig_results <- list()
descriptive_summaries <- list()

# Loop through each task
for (game in names(df_split)) {
  
  # Extract task data
  game_df <- df_split[[game]] %>%
             na.omit()
  print(count(game_df))
  
  # Select relevant features
  feature_df <- feature_select(game_df, game)

  # Store descriptive summary
  descriptive_summaries[[game]] <- data.frame(
                                    min = sapply(feature_df, min),
                                    q1 = sapply(feature_df, quantile, 0.25),
                                    q3 = sapply(feature_df, quantile, 0.75),
                                    max = sapply(feature_df, max),
                                    median = sapply(feature_df, median),
                                    mean = sapply(feature_df, mean),
                                    sd =sapply(feature_df,sd)
                                  )
  
  # Loop through each target variable
  for (tar in colnames(feature_df)) {
    
    ## KSS models
    set.seed(123)  # set seed
    lmm_kss <- lmer(as.formula(paste(tar, " ~ kss + (1 | subj)")), data = game_df)
    kss_summary <- summary(lmm_kss)
    
    # Extract results for KSS model
    kss_results[[paste(game, tar, "kss", sep = "_")]] <- data.frame(
      model = "KSS",
      task = game,
      target = tar,
      term = rownames(coef(kss_summary)),
      estimate = coef(kss_summary)[, "Estimate"],
      std_error = coef(kss_summary)[, "Std. Error"],
      p_value = coef(kss_summary)[, "Pr(>|t|)"]
    )
    
    ## Time awake and ambient light for circadian phase 
    set.seed(123)  # set seed
    lmm_awake <- lmer(as.formula(paste(tar, " ~ timeawake * light.task.log + (1 | subj)")), data = game_df)
    awake_summary <- summary(lmm_awake)
    
    # Extract results for circadian phase model
    awake_results[[paste(game, tar, "awake", sep = "_")]] <- data.frame(
      model = "Time Awake",
      task = game,
      target = tar,
      term = rownames(coef(awake_summary)),
      estimate = coef(awake_summary)[, "Estimate"],
      std_error = coef(awake_summary)[, "Std. Error"],
      p_value = coef(awake_summary)[, "Pr(>|t|)"]
    )
    
    ## Trigonometric model for daily variation of performance
    set.seed(123)  # set seed
    lmm_trig <- lmer(as.formula(paste(tar, " ~ cosine + sine + (1 | subj)")), data = game_df)
    trig_summary <- summary(lmm_trig)
    
    # Extract results for Trigonometric model
    trig_results[[paste(game, tar, "trig", sep = "_")]] <- data.frame(
      model = "Trigonometric",
      task = game,
      target = tar,
      term = rownames(coef(trig_summary)),
      estimate = coef(trig_summary)[, "Estimate"],
      std_error = coef(trig_summary)[, "Std. Error"],
      p_value = coef(trig_summary)[, "Pr(>|t|)"]
    )
  }
}

# Combine results into data frames
kss_results_df <- do.call(rbind, kss_results)
awake_results_df <- do.call(rbind, awake_results)
trig_results_df <- do.call(rbind, trig_results)


# Combine all results together
combined_df <- bind_rows(kss_results_df, awake_results_df, trig_results_df)


## Generate LateX table #### 
# Create a new environment to store datasets
env <- new.env()

# Create the output directory if it does not exist
output_dir <- "latex"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Group by 'task' and 'model'
grouped_df <- combined_df %>%
  group_by(task, model)

# Split the grouped data into individual data frames and store in the environment
grouped_df %>%
  group_split() %>%
  walk(~ {
    group_name <- paste0(.$task[1], "_", .$model[1])
    assign(group_name, ., envir = env)
  })

# Function to convert small numbers to LaTeX scientific notation
format_scientific <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (abs(x) < 0.01) {
    exponent <- floor(log10(abs(x)))
    mantissa <- x / (10^exponent)
    formatted <- sprintf("\\textbf{%.2f} \\times  \\textbf{10$^{%d}$}", mantissa, exponent)
    return(formatted)
  } else {
    exponent <- floor(log10(abs(x)))
    mantissa <- x / (10^exponent)
    formatted <- sprintf("%.2f \\times  10$^{%d}$", mantissa, exponent)
    return(formatted)
  }
}

# Function to generate LaTeX subtable code from a data frame with custom term formatting
generate_latex_subtable <- function(df) {
  # Replace specific terms with their LaTeX equivalents
  df$term <- dplyr::recode(df$term,
                    "timeawake:light.task.log" = "\\text{Time awake} \\\\ \\times \\\\ \\text{Ambient light}",
                    "timeawake" = "\\text{Time awake}",
                    "light.task.log" = "\\text{Ambient light}")
  
  # Replace specific terms with their LaTeX equivalents
  df$target <- dplyr::recode(df$target,
                      "DPRIME" = "D'")
  
  task <- unique(df$task)
  targets <- unique(df$target)
  terms <- unique(df$term)
  
  # Determine the width based on the number of targets
  num_targets <- length(targets)
  subtable_width <-  1 # min(0.9, 0.3 + 0.15 * num_targets)  # Adjust the width proportionally
  
  # Prepare the LaTeX subtable header
  header <- paste0("\\begin{subtable}[t]{", subtable_width, "\\textwidth}\n",
                   "\\caption{\\raggedright ", task, "}\n",
                   "\\begin{tabular}{ll|", paste(rep("c", num_targets), collapse = "|"), "}\n",
                   "\\hline\n",
                   "\\textbf{Target} & & ", paste(targets, collapse = " & "), " \\\\\n",
                   "\\hline\n")
  
  # Add rows for each term
  rows <- ""
  for (term in terms) {
    if (term == "(Intercept)") {
      term_row <- paste0(term, " & & ", 
                         paste(sprintf("%.2f", df$estimate[df$term == term]), collapse = " & "), " \\\\ \\hline \n")
    } else if (term == "\\text{Time awake} \\\\ \\times \\\\ \\text{Ambient light}") {
      term_row <- paste0("\\text{Time awake} & Coef & ", 
                         paste(sprintf("%.2f", df$estimate[df$term == term]), collapse = " & "), " \\\\\n",
                         "\\times & Std Error & ", 
                         paste(sprintf("%.3f", df$std_error[df$term == term]), collapse = " & "), " \\\\\n",
                         "\\text{Ambient light} & p-value & ", 
                         paste(sapply(df$p_value[df$term == term], format_scientific), collapse = " & "), " \\\\\n",
                         "\\hline\n")
    } else {
      term_row <- paste0(term, " & Coef & ", 
                         paste(sprintf("%.2f", df$estimate[df$term == term]), collapse = " & "), " \\\\\n",
                         "& Std Error & ", 
                         paste(sprintf("%.3f", df$std_error[df$term == term]), collapse = " & "), " \\\\\n",
                         "& p-value & ", 
                         paste(sapply(df$p_value[df$term == term], format_scientific), collapse = " & "), " \\\\\n",
                         "\\hline\n")
    }
    rows <- paste0(rows, term_row)
  }
  
  # Close the LaTeX subtable
  footer <- "\\hline\n\\end{tabular}\n\\end{subtable}\n"
  
  # Combine header, rows, and footer
  latex_code <- paste0(header, rows, footer)
  return(latex_code)
}

# Generate individual LaTeX files for each model-task combination as subtables
walk(ls(envir = env), ~ {
  df <- get(.x, envir = env)
  latex_code <- generate_latex_subtable(df)
  file_name <- paste0(output_dir, "/", .x, ".tex")
  writeLines(latex_code, con = file_name)
})
