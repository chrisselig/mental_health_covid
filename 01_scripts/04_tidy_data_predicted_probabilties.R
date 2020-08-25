# Libraries ----
library(tidyverse)
library(writexl)
library(readxl)

# Read in data ----
raw_data_predicted <- readxl::read_xlsx('00_data/Predicted values_raw.xlsx',col_names = F)

# Tidy data ---
tidy_data_predictive <- raw_data_predicted %>% 
    fill(...1,.direction = 'down') %>% 
    # Create the metric column
    mutate(
        metric = case_when(
            ...1 == "1. Predicted 'bad' mental health" ~ "Predicted 'bad' mental health",
            ...1 == "1. Predicted elevated anxiety" ~ "Predicted elevated anxiety"
        )
    ) %>% 
    fill(metric, .direction = 'down') %>% 
    filter(!is.na(...2)) %>% 
    rename(
        economic_variable = ...1,
        response = ...2,
        `Predicted probability` = ...3,
        `CI (low)` = ...4,
        `CI (high)` = ...5
    ) %>% 
    # Reorder all columns
    select(
        metric, everything()
    )

# Write excel file
writexl::write_xlsx(tidy_data_predictive, '00_data/tidied_data_via_code/predicted_probabilities_tidy.xlsx')
