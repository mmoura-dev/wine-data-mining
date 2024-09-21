library("daltoolbox")
library(tidyr)
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library(gridExtra)

# Setup
colors <- brewer.pal(4, 'Set1')
font <- theme(text = element_text(size=16))


# Loading data set
load("data/flights/bfd_2023.rdata")
head(bfd)
summary(bfd)


# Filters and label
bfd <- bfd %>%
  select(where(is.numeric)) %>%
  mutate(target = delay_depart > 15) %>%
  select(-starts_with("arrival"), -starts_with("delay")) %>%
  drop_na()


# Probability density distribution
X_column_names <- colnames(bfd)[1:length(bfd) - 1]
create_box_plot_grf <- function(df, col_name, label_col_name) {
  num_unique_labels <- length(unique(df[[label_col_name]]))
  plot <- plot_boxplot_class(df %>% dplyr::select(label_col_name, col_name), 
                             class_label=label_col_name, label_x=col_name,
                             color=colors[c(1:num_unique_labels)]) + font
  
  ggsave(filename = paste0("plots/exploratory_analysis/box_plot_15m/", col_name, ".pdf"), plot = plot)
  return(plot)
}

grfs <- purrr::map(X_column_names,
                   purrr::partial(create_plot_density_grf, df = bfd,
                                  label_col_name = "target"))

# All plots together
do.call(grid.arrange, as.list(grfs))
