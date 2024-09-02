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
    - **EDA.R:** Exploratory data analysis on all datasets. Most discoveries of the project were found here.
    - **Data_Preprocessing_and_Aggregation.R:** This RScript derived the data 'bt_with_history_hour_encoded.RData' from the original datasets. All imputations and aggregations were performed here.
    - **RandomForest.R:** Training process of all Random Forest models on cognitive task metrics with MEDI features, aims to find the impact of MEDI history on cognitive performance.
    - **ANOVA.R:** ANOVA tests on KSS and MEDI variables with normality and homogeneity of variance check.
    - **Linear_Mixed_Model.R:** Linear Mixed Models on cognitive task metrics with subject as the random effect, aims to find the impact of KSS, circadian phase, and daily variation on cognitive performance.

- **figures:** This folder contains all figures produced from the RScripts.
    - **EDA:** Figures generated by 'EDA.R'.
    - **Feature_Importance_Plots:** The feature importance plots from the Random Forest models trained by 'RandomForest.R'.
    - **Light_History_By_KSS:** The distributions of MEDI variables by each KSS scores, generated by 'ANOVA.R'.
    - **MEDI_plots:** Scatterplot of all MEDI observations of the subjects, also generated by 'EDA.R'.

 - **latex_tables:**
   The Random Forest performance metrics tables in LaTeX code. All tables are generated from 'RandomForest.R'

- **Additional_materials:**
  Contains pdf files of some details in data attributes, imputation, and supplementory figures useful for the report of the Project.

## Detail data attributed and imputation
The detail about the data variables in 'bt_with_history_encoded.R' and imputation of missing values are in the file 'Data Attributes and Imputation.pdf' in the folder 'Additional_materials'


## Usage Instructions

1. **Setup:**
   - Ensure you have RStudio 4.4.1 installed.
   - Install the required R packages listed in 'RPackages.txt'.

2. **Running Analysis:**
    1. **Data_Preprocessing_and_Aggregation.R**: Create the working data 'bt_with_history_hour_encoded.R'.
    2. **EDA.R:** Data visualisation.
    3. **Linear_Mixed_Model.R:** Linear Mixed Models on cognitive performance with KSS, circadian phase, and daily variation.
    4. **ANOVA.R:** MEDI variables by KSS scores.
    5. **RandomForest.R:** Random Forest Models.

   ```R
   # Example command to load data
   load("data/processed/bt_cognitive_variables.RData")

