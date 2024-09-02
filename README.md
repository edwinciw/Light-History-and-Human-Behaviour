# Light-History-and-Human-Behaviour

## Author
Iok Weng Chan

## Project Objective
This project aims to uncover relationships between light exposure history and human behaviors by applying statistical methods such as ANOVA, linear mixed models, and Random Forest models to experimental data.

## Project Structure
The project is conducted using RStudio 4.4. The data and analyses are organized as follows:

- **data:** This directory contains all data files.
  - **processed:** This folder contains the original datasets used in this project, which were obtained from the private GitHub repository [https://github.com/neuroTom/melanopsin-IRL-Iok](https://github.com/neuroTom/melanopsin-IRL-Iok).
    - **brightertime:** Data collected by the Brighter Time smartphone app.
        - **`bt_cognitive_variables.RData`:** Contains variables related to cognitive task performances.
        - **`bt_kss.RData`:** Contains subjective sleepiness measured by the KSS (Karolinska Sleepiness Scale) scores.
        - **`bt_baseline.RData`:** Contains questionnaire answers of subjects.
    - **SpectraWear:** Data collected by the SpectraWear light-logger device.
        - **`spectrawear_long.RData`:** Contains light exposure measures, including the α-opic EDI, throughout the experiment.
    - **working_data:** Data used for all analyses.
        -**bt_with_history_hour.RData:** Aggregated data derived from the original datasets, it contains the cognitive task performances, subjective sleepiness, and 24-hour melanopic-EDI history before the initiation of the tasks.

- **RScripts:** This folder contains the Rscripts for all data manipulation and analysis.
    - **EDA.R:** Exploratory data analysis on all datasets.
    - **Data_Prepocessing_and_Aggregation.R:** This RScript derived the data 'bt_with_history_hour_encoded.RData' from the original datasets. All imputations and aggregations were performed here.
    - **RandomForest.R:** Training process of all Random Forest models on cognitive task metrics with MEDI features, aims to find the impact of MEDI history on cognitive performance.
    - **ANOVA.R:** ANOVA tests on KSS and MEDI variables with normality and homogeneity of variance check.
    - **Linear_Mixed_Model.R:** Linear Mixed Models on cognitive task metrics with subject as the random effect, aims to find the impact of KSS, circadian phase, and daily variation on cognitive performance.
- **figures:** This folder contains all figures produced from the RScripts.
  
## Data Preprocessing
- Data from the original datasets were aggregated and processed to prepare the working dataset for analysis. Preprocessing steps, including data cleaning and feature encoding, are detailed in the `preprocessing.R` script.

## Usage Instructions

1. **Setup:**
   - Ensure you have RStudio 4.4 installed.
   - Install the required R packages listed in `requirements.txt` (if applicable).

2. **Running Analysis:**
   - Load the datasets into RStudio using the provided R scripts.
   - Execute the scripts `anova_analysis.R`, `linear_mixed_models.R`, and `random_forest_models.R` to perform the analyses.

   ```R
   # Example command to load data
   load("data/processed/bt_cognitive_variables.RData")

