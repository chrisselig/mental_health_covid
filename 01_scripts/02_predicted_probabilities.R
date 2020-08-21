# Load libraries ----
library(tidyverse) # data manipulation, plotting, and pipe operator ( %>% )
library(ggthemes) # for the Edward Tufte theme
library(readxl) # read excel files
library(janitor) # clean column names quickly
library(RColorBrewer) # color palette

# Load data ----
prediction_raw <- readxl::read_xlsx('00_data/Predicted values of mental health by econ concerns.xlsx')

# Tidy the data ----
prediction_tidy <- prediction_raw %>% 
    # clean column names
    janitor::clean_names() %>% 
    # unpivot to long format for plotting
    pivot_longer(
        cols = predicted_probability:ci_high
    )

prediction_tidy_long <- prediction_raw %>% 
    # clean column names
    janitor::clean_names() %>% 
    # unpivot to long format for plotting
    pivot_longer(
        cols = predicted_probability:ci_high
    )

# Plotting ----
# prediction_tidy %>% 
#     filter(metric == "Predicted 'bad' mental health") %>% 
#     filter()
#     ggplot() +
#     geom_segment(
#         stat = 'identity',
#         aes(x = ci_low , xend = predicted_probability, y = 0, yend = 0)
#     ) +
#     geom_segment(
#         stat = 'identity',
#         aes(x = predicted_probability , xend = ci_high, y = 0, yend = 0)
#     ) +
#     geom_point(data = prediction_tidy_long %>% filter(metric == "Predicted 'bad' mental health"),
#                 aes(x =value, y = 0, color = name)
#                                                       
#     ) +
#     scale_x_comma(position = "top", limits = c(0.4, 0.7)) +
#     # scale_color_ipsum(name = "A real legend title") +
#     labs(
#         x = "Description of the value", y = NULL,
#         title = "A good plot title"
#     ) +
#     theme_ipsum_rc(grid = "X") +
#     theme(legend.position = "bottom")



financial <- prediction_tidy_long %>% 
    filter(metric == "Predicted 'bad' mental health") %>% 
    filter(economic_variable =='Financial impact') %>% 
    ggplot() +

    geom_point(aes(x=response, y=value,color = name), size=2) +
    coord_flip() +

    # facet_wrap(~economic_variable) +
    theme_tufte() +
    labs(
        x = NULL,
        y = NULL,
        title = "Financial Impact",
        subtitle = ''
    ) +
    theme(
        # change lineheight of title/subtitles
        plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1)
        # remove legend
        # legend.position = "none"
    )

ggsave('02_images/predicted_probability_financial_impact.png',financial, dpi = 600, height = 3, width = 5)    
