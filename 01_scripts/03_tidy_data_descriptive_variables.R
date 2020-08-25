# Load Libraries ----
library(tidyverse) # data manipulation, plotting, and pipe operator ( %>% )
library(readxl)
library(writexl) # write excel file

# Load data ---
# Load data and do not load column headers
raw_data <- read_xlsx('00_data/Descriptives for 3 variables_raw.xlsx',col_names = FALSE)

# Tidy data ----
tidy_data_descriptive <- raw_data %>% 
    # fill down information in first column
    fill(...1,.direction = 'down') %>% 
    # remove rows that have NA values
    filter(!is.na(...2)) %>% 
    # rename columns: Note, I didn't rename columns w1/w2 to March/May because it is done in the original data visualization script
    rename(
        metric = ...1,
        response = ...2,
        w1 = ...3,
        w2 = ...4
    )
    
# Manually reorder rows so variables appear in correct order in plots
 response_order <- c('Excellent','Very good','Good','Fair','Poor','No impact', 
                'Too soon to tell', 'Minor','Moderate','Major','Not expecting to lose job','Might lose job',
                'Not employed'
)

 # Reorders rows
tidy_data_descriptive <- tidy_data_descriptive %>% 
    slice(match(response_order, response))

# Export data ----
writexl::write_xlsx(tidy_data_descriptive, '00_data/tidied_data_via_code/Descriptives_tidy.xlsx')
