library("daltoolbox")
library(tidyr)
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library(gridExtra)
library(tidyverse)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/tutorial/graphics_extra.R")

library("reshape")
library("RColorBrewer")
library("corrplot")
library("WVPlots")
library("GGally")
library("aplpack")

library(tidyverse) #carregar outros pacotes do R
library(knitr) #formatação de tabelas
library(kableExtra) #formatação de tabelas
library(e1071) #medidas de assimetria e curtose
library(plotly)
library(questionr)

# Setup
colors <- brewer.pal(4, 'Set1')
font <- theme(text = element_text(size=16))


# Loading data set
load("data/flights/bfd_2023.rdata")
head(bfd)
summary(bfd)

summary_output <- capture.output(summary(bfd))

# Step 2: Write the output to a text file
writeLines(summary_output, "summary_output.txt")


# Filters and label
bfd <- bfd[1:57000, ]
bfd2 <- bfd %>%
  select(where(is.numeric)) %>%
  select(c("delay_depart", "depart_dew_point", "depart_wind_direction", "depart_elevation", "depart_relative_humidity", "depart_air_temperature", "depart_visibility")) %>%
  mutate(target = delay_depart > 15) %>%
  select(-starts_with("arrival"), -starts_with("delay")) %>%
  drop_na()


# bfd[["target"]] <- as.character(bfd[["target"]])
X_column_names <- colnames(bfd2)[1:length(bfd2) - 1]

for (col in X_column_names) {
  summary(df[col])
} 


# grf <- plot_pair(data=bfd2, cnames=X_column_names, clabel='target', title="flight", colors=colors[1:2])
# plot(grf)

# grf <- ggparcoord(data = bfd2, columns = 1:6, groupColumn=7) + 
#   theme_bw(base_size = 20) + scale_color_manual(values=colors[1:2]) + font
# plot(grf)

bfd3 <- bfd %>%
  mutate(target = delay_depart > 15) %>%
  select(!where(is.numeric)) %>%
  #select(c("delay_depart", "depart_dew_point", "depart_wind_direction", "depart_elevation", "depart_relative_humidity", "depart_air_temperature", "depart_visibility")) %>%
  #mutate(target = delay_depart > 15) %>%
  select(-starts_with("arrival"), -starts_with("delay")) %>%
  drop_na()

# frequency_df <- bfd3 %>%
#   select(c("company", "target")) %>%
#   filter(target == TRUE) %>%
#   group_by(company) %>%
#   summarise(Frequency = n()) %>%
#   arrange(company)

frequency_df <- bfd %>%
  mutate(target = delay_depart > 15) %>%
  select(c("company", "target")) %>%
  group_by(company) %>%
  summarise(
    Delays = sum(target == TRUE, na.rm = TRUE),
    Total = n(),
    Relative_Frequency = (Delays / Total) * 100,
    .groups = 'drop'
  ) %>%
  filter(Total > 500)

ggplot(frequency_df, aes(x = company, y = Relative_Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Frequency Distribution", x = "company", y = "Relative Frequency")

freq(bfd3$company)
