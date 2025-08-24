# CFPB Complaint Equity Analysis

This repository contains data and code for the PNAS Brief Report:

**Sun Y., He D., Kejriwal M. (2025). Persistent Inequities in Financial Complaint Resolution for Older Adults and Service-Members.**

## Files

- `model.R` – R script for data cleaning, Model 4 estimation, and output generation  
- `predicted_probabilities_by_group_year.csv` – predicted probabilities by group and year  
- `state_period_changes_by_group.csv` – state-level changes between early (2014–2017) and modern (2018–2022) periods  
- `state_adi_disparities.csv` – state-level disparities between advantaged and disadvantaged areas  

## Data Sources

- CFPB Consumer Complaint Database: [link](https://www.consumerfinance.gov/data-research/consumer-complaints/)  
- Area Deprivation Index (ADI): [link](https://www.neighborhoodatlas.medicine.wisc.edu/)

## How to use

Run the R script:

```r
source("model4_time_interactions.R")
