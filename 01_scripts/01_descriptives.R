# library(tidypredict)
library(tidyverse) # data manipulation, plotting, and pipe operator ( %>% )
library(readxl)
library(RColorBrewer) # color palette
library(cowplot) # arranging plots into a single plot
library(forcats) # Rearranging variables to proper order

# Load Data
raw_data_descriptive <- readxl::read_xlsx('00_data/Descriptives for 3 variables.xlsx')

# Define color palette ----
# Create a color table so colors can be assigned to each variable consistently 
color_table <- tibble(
    response = c('Excellent','Very good','Good','Fair','Poor','No impact','Too soon to tell','Minor',
                 'Moderate','Major','Not expecting to lose job','Might lose job','Not employed' 
    ),
    Color = c("#252525","#525252","#737373","#969696","#BDBDBD","#252525","#525252","#737373","#252525","#525252","#737373","#969696","#BDBDBD")
)

# Unpivot data so W1 and W2 columns are stacked on top of each other
descriptive_tidy <- raw_data_descriptive %>% 
    # Rename column headers
    rename(March = w1,
           May = w2
    ) %>% 
    # Turn dataframe from wide to long (unpivot)
    pivot_longer(
        cols = March:May,
        names_to='period'
    ) %>% 
    # Turn to factor and set levels so they appear properly in visuals
    mutate(
        response = factor(response, levels = color_table$response)
    )


# Create a plotting function ----
descriptive_plot_function <- function(data = descriptive_tidy ,
                                      metric,
                                      xlab){
    
    plot <- data %>% 
        filter(metric == !!metric) %>%
        # arrange(value) %>% 
        ggplot(aes(x = fct_rev(period), y = value, fill = response)) + 
        
        # Geometries
        geom_bar(stat="identity") + 
        
        # Formatting
        coord_flip() +
        labs(
            y = '',
            x = xlab#,           # comment this line for horizontal labels
            # x = '',           # uncomment this line for horizontal labels
            # subtitle = xlab   # uncomment this line for horizontal labels
        ) +
        theme_minimal() +
        
        # # Change color to greyscale
        scale_fill_manual(values = color_table$Color) +
    
        # Customize theme some more
        theme(
            panel.grid = element_blank(),
            legend.title = element_blank(),
            legend.position = "right",
            # legend.text=element_text(size=10)
            text = element_text(size = 14)
        )
    
    return(plot)
}

# Testing function ----
# data <- descriptive_tidy
# metric <- 'Job security'
# xlab <- 'SRMH'
# 
# descriptive_plot_function(data = data, metric = metric, xlab = xlab)



# Create final plot ----
p1 <- descriptive_plot_function(data = descriptive_tidy, metric = 'SRMH', xlab = 'SRMH')
p2 <- descriptive_plot_function(data = descriptive_tidy, metric = 'Financial impact', xlab = 'Financial Impact')
p3 <- descriptive_plot_function(data = descriptive_tidy,metric = 'Job security', xlab = 'Job Security')

# Arrange plots into single column
combined_descriptive <- cowplot::plot_grid(p1, p3, p2, 
                   ncol = 1, nrow =3)

ggsave('02_images/descriptive_combined.png', combined_descriptive, dpi = 300, height=7, width = 8)
