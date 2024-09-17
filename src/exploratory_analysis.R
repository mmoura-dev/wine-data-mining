library("daltoolbox")
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

# Delay bigger than threshold
filtered_df <- bfd %>% filter(delay_depart > 5)
head(filtered_df)
summary(filtered_df)

# Adding label
bfd <- bfd %>% mutate(bool_delay = delay_depart > 5)


# Density distribution
X_column_names <- colnames(bfd)[1:length(bfd) - 1]
create_plot_density_grf <- function(df, col_name, label_col_name) {
  num_unique_labels <- length(unique(df[[label_col_name]]))
  return(plot_density_class(df %>% dplyr::select(label_col_name, col_name), 
                            class_label=label_col_name, label_x=col_name,
                            color=colors[c(1:num_unique_labels)]) + font)
}

grfs <- purrr::map(X_column_names,
                   purrr::partial(create_plot_density_grf, df = bfd,
                                  label_col_name = "bool_delay"))

options(repr.plot.width=8, repr.plot.height=8)
do.call(grid.arrange, as.list(grfs))
