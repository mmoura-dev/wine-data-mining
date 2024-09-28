library("daltoolbox")
library("RColorBrewer")
library("ggplot2")
library(dplyr)
library("gridExtra")
source("src/flights_data.R")


# Configs
col_set <- brewer.pal(9, 'Set1')
colors <- col_set[1:4]
font <- theme(text = element_text(size=16))


# Loading data
jk_flights <- get_flight_data(2023, "SBBR") %>%
  select(where(is.numeric), starts_with("target"))
  drop_na()

summary(jk_flights)


X_column_names <- colnames(jk_flights)[1:length(jk_flights) - 1]
create_plot_density_grf <- function(df, col_name, label_col_name) {
  num_unique_labels <- length(unique(df[[label_col_name]]))
  plot <- plot_density_class(df %>% dplyr::select(label_col_name, col_name), 
                             class_label=label_col_name, label_x=col_name,
                             color=colors[c(1:num_unique_labels)]) + font
  
  # ggsave(filename = paste0("plots/num_col_exploratory/probability_density/", col_name, ".pdf"), plot = plot)
  return(plot)
}

grfs <- purrr::map(X_column_names,
                   purrr::partial(create_plot_density_grf, df = jk_flights,
                                  label_col_name = "target"))

# All plots together
do.call(grid.arrange, as.list(grfs))

