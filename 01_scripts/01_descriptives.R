# library(tidypredict)
library(tidyverse) # data manipulation, plotting, and pipe operator ( %>% )
library(ggthemes) # for the Edward Tufte theme
library(readxl)
library(RColorBrewer) # color palette
library(cowplot) # arranging plots into a single plot

# Load Data
raw_data_descriptive <- readxl::read_xlsx('00_data/Descriptives for 3 variables.xlsx')

# Unpivot data so W1 and W2 columns are stacked on top of each other
descriptive_tidy <- raw_data_descriptive %>% 
    pivot_longer(
        cols = w1:w2,
        names_to='period'
    )

# Create a plotting function ----

descriptive_plot_function <- function(data = descriptive_tidy ,
                                      metric,
                                      xlab){
    
    
    plot <- data %>% 
        filter(metric == !!metric) %>% 
        ggplot(aes(x = period, y = value, fill = response)) + 
        geom_bar(stat="identity",width = 0.5) + 
        # facet_grid(.~metric) +
        coord_flip() +
        labs(
            y = '',
            x = xlab
        ) +
        # scale_y_discrete(expand = C (0,5)) +
        theme_minimal() +
        scale_fill_brewer(palette = "Greys") +
        theme(
            panel.grid = element_blank(),
            legend.title = element_blank(),
            legend.position = "right",
            legend.text=element_text(size=8)
        )
    
    return(plot)
}

# Testing function ----
# data <- descriptive_tidy
# metric <- 'SRMH'
# xlab <- 'SRMH Responses'
# 
# descriptive_plot_function(data = data, metric = metric, xlab = xlab)



# Create final plot ----
p1 <- descriptive_plot_function(data = data, metric = 'SRMH', xlab = 'SRMH')
p2 <- descriptive_plot_function(data = data, metric = 'Financial impact', xlab = 'Financial Impact')
p3 <- descriptive_plot_function(data = data, metric = 'Job security', xlab = 'Job Security')

combined_descriptive <- cowplot::plot_grid(p1, p3, p2 + remove("x.text"), 
                   ncol = 1, nrow =3)

ggsave('02_images/descriptive_combined.png', combined_descriptive, dpi = 300, height=7, width = 8)
