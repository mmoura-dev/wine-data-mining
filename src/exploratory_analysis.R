library("daltoolbox")
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library(gridExtra)

# Setup
colors <- brewer.pal(4, 'Set1')
font <- theme(text = element_text(size=16))


# Loading data set
wine_df <- read.csv("data/wine.csv")
head(wine_df)
summary(wine_df)


# Density distribution
wine_X_column_names <- colnames(wine_df)[2:length(wine_df)]
create_plot_density_grf <- function(df, col_name, label_col_name) {
  num_unique_labels <- length(unique(df[[label_col_name]]))
  return(plot_density_class(df %>% dplyr::select(label_col_name, col_name), 
                            class_label=label_col_name, label_x=col_name,
                            color=colors[c(1:num_unique_labels)]) + font)
}

grfs <- purrr::map(wine_X_column_names,
                   purrr::partial(create_plot_density_grf, df = wine_df,
                                  label_col_name = "Type"))

options(repr.plot.width=8, repr.plot.height=8)
do.call(grid.arrange, as.list(grfs))
