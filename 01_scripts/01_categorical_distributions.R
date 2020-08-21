# library(tidypredict)
library(tidyverse) # data manipulation, plotting, and pipe operator ( %>% )
library(ggthemes) # for the Edward Tufte theme
library(ggtext) # For formatting text in plots, in particular titles and subtitles 
library(treemap)
library(RColorBrewer)
library(treemapify)

raw_data_labels <- read_csv('00_data/covid-data-chris-20-08-19.csv')
# raw_data_nolabels <- read_csv('00_data/covid-data-chris-nolabel-20-08-19.csv')

# Chose Western University Colors to be used in plot
colors = c('#4F2683','#807F83')

# distribution_joblose <- raw_data_labels %>% 
#     select(srmh, joblose,w) %>% 
#     # Plotting
#     ggplot(aes(x = srmh,fill = w)) +
#     # Add geometries
#     geom_bar() +
#     # geom_text(aes(label = paste0(freq,"%")),
#     #           position = position_stack(vjust = 0.5), size = 2
#     # ) +
#     # Formatting
#     labs(
#         title = "SRMH Distribution for Job Loss", 
#         subtitle = "Displayed by <b style='color:#4F2683'>W1 </b>and <b style='color:#807F83'>W2</b>",
#         x = '',
#         y = ''
#     ) +
#     coord_flip() +
#     scale_fill_manual(values =colors) +
#     theme_tufte() +
#     theme(
#         # change lineheight of title/subtitles
#         plot.title = element_markdown(lineheight = 1.1),
#         plot.subtitle = element_markdown(lineheight = 1.1),
#         # remove legend
#         legend.position = "none"
#     )
    
ggsave('02_images/distribution_joblose.png',distribution_joblose,dpi = 600, height = 3, width =4)


raw_data_labels %>% 
    select(srmh, joblose,w) %>% 
    filter(joblose != 'Valid skip') %>% 
    group_by(w,srmh, joblose) %>% 
    summarize(count = n()) %>% 
    treemap(
        index = c('w','srmh','joblose'),
        vSize = 'count',
        n = 3,
        fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","orange",'black'),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
            c("center", "center"), 
            c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F,                      # If true, labels are bigger when rectangle is bigger.
        title = 'Job Loss Responses',
        palette = 'Set1'
    )



treemap_joblose <- raw_data_labels %>% 
    select(srmh, joblose,w) %>% 
    filter(joblose != 'Valid skip') %>% 
    group_by(w,srmh, joblose) %>% 
    summarize(count = n()) %>%
    ggplot(aes(area = count, fill = w, label = joblose,
                    subgroup = srmh)) +
    geom_treemap() +
    geom_treemap_subgroup_border(color = '#E0C5B2') +
    geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                                   "black", fontface = "italic", min.size = 0) +
    geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
    labs(
        title = "SRMH Distribution for Job Loss", 
        subtitle = "Displayed by <b style='color:#4F2683'>W1 </b>and <b style='color:#807F83'>W2</b>",
        x = '',
        y = ''
    ) +
    # theme_tufte() +
    scale_fill_manual(values =colors) +
    theme(
        # change lineheight of title/subtitles
        plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1),
        # remove legend
        legend.position = "none"
    )

ggsave('02_images/distribution_joblose.png',treemap_joblose,dpi = 600, height = 4, width =5)
