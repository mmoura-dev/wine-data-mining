library("daltoolbox")
library(tidyr)
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library(gridExtra)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/tutorial/graphics_extra.R")

# Setup
colors <- brewer.pal(4, 'Set1')
font <- theme(text = element_text(size=16))


# Loading data set
load("data/flights/bfd_2023.rdata")


# Filters and label
bfd <- bfd %>%
  select(where(is.numeric)) %>%
  select(-starts_with("arrival"), -starts_with("delay")) %>%
  drop_na()


plot_correlation(bfd)
