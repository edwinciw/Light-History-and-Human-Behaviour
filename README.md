# Light-History-and-Human-Behaviour

## Author
Iok Weng Chan

## Project Objective
This project aims to uncover relationships between light exposure history and human behaviors by applying statistical methods such as ANOVA, linear mixed models, and Random Forest models to experimental data.

## Project Structure
The project is conducted using RStudio 4.4. The data and analyses are organized as follows:

- **data:** This directory contains all data files.
  - **processed:** This folder contains the original datasets used in this project, which were obtained from the private GitHub repository [https://github.com/neuroTom/melanopsin-IRL-Iok](https://github.com/neuroTom/melanopsin-IRL-Iok). The datasets are listed below:
    - **brightertime:** Data collected by the Brighter Time smartphone app.
        - **`bt_cognitive_variables.RData`:** Contains variables related to cognitive task performances.
        - **`bt_kss.RData`:** Contains KSS (Karolinska Sleepiness Scale) scores.
        - **`bt_baseline.RData`:** Contains questionnaire answers of subjects.
    - **SpectraWear:** Data collected by the SpectraWear light-logger device.
        - **`spectrawear_long.RData`:** Contains light exposure measures, including the α-opic EDI, throughout the experiment.

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

