# Load libraries ----
library(tidyverse) # data manipulation, plotting, and pipe operator ( %>% )
library(readxl) # read excel files
library(janitor) # clean column names quickly
library(cowplot) # arranging plots into a single plot


# Load data ----
prediction_raw <- readxl::read_xlsx('00_data/Predicted values of mental health by econ concerns.xlsx')
# prediction_raw <- readxl::read_xlsx('00_data/tidied_data_via_code/predicted_probabilities_tidy.xlsx')

# Tidy the data ----
prediction_tidy <- prediction_raw %>% 
    # clean column names
    janitor::clean_names() %>% 
    mutate(
        # Turn variable into factor and set order they will appear in plot
        economic_variable = factor(economic_variable, levels = c('Job security','Financial impact','Food security'))
    )

# Define plotting function ----
pred_probability_function <- function(data = prediction_tidy,
                                      metric, 
                                      # economic_variable, 
                                      ylab){
    
    data %>% 
        
        # Filter on chosen metric... !! just tells ggplot to evaluate in function, not earlier
        filter(metric == !!metric) %>% 
        
        # filter(economic_variable == !!economic_variable) %>% 
        ggplot() +
        
        # Geometries
        # Add horizontal line for low/high confidence interval
        geom_linerange(aes(xmin = ci_low, xmax = ci_high, y = response), size=1, color="grey") +
        
        # Add point for predicted probability
        geom_point(aes(x=predicted_probability, y=response), size=4, color = 'black') +
        
        # Trellis/show group by economic variable
        facet_wrap(~economic_variable, nrow = 3,strip.position = "right", scales='free_y') +
        
        # Change labels
        labs(
            y = ylab,           # comment this line for horizontal labels
            x = ''#,
            #y = '',           # uncomment this line for horizontal labels
            #subtitle = ylab   # uncomment this line for horizontal labels
        ) +
        # Change theme
        theme_minimal() +
        theme(
            # Turn off grid
            panel.grid = element_blank(),
            # Change text size of all elements
            text = element_text(size = 14)#,
            #strip.text.y.right = element_text(angle=0)   # Rotate right hand side y-axis labels
        ) 
}


# Function Testing ----
# data <- prediction_tidy
# metric <- "Predicted 'bad' mental health"
# 
# pred_probability_function(data = prediction_tidy,metric = metric, ylab = 'test')


# Create final plot ----
p1 <- pred_probability_function(data = prediction_tidy, metric = "Predicted 'bad' mental health", ylab = "Predicted 'bad' mental health")
p2 <- pred_probability_function(data = prediction_tidy, metric = "Predicted elevated anxiety", ylab = "Predicted elevated anxiety")

# # Arrange plots into single column
combined_predictive_prob <- cowplot::plot_grid(p1,p2, 
                                           ncol = 1, nrow =2)

# Save plot
ggsave('02_images/predicted_prob_combined.png', combined_predictive_prob, dpi = 300, height=8.5, width = 8)
