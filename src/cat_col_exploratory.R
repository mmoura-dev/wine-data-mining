library("daltoolbox")
library("RColorBrewer")
library("ggplot2")
library(dplyr)
source("src/flights_data.R")


# Configs
col_set <- brewer.pal(9, 'Set1')
colors <- col_set[1:4]
font <- theme(text = element_text(size=16))


# Loading data
jk_flights <- get_flight_data(2023, "SBBR") %>%
  select(!where(is.numeric), -starts_with("expected"))

summary(jk_flights)


# Absolute frequency histogram for: company  di  type  depart_sky_coverage  
# depart_wind_speed_scale  depart_wind_direction_cat  depart_day_period  target
ggplot(jk_flights, aes(x = reorder(company, -table(company)[company]))) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Frequência Absoluta", x = "Companhia Aérea", y = "Frequência")


# Relative freq
# Columns: company  arrival  flight  depart_sky_coverage
# depart_wind_speed_scale  depart_wind_direction_cat  depart_day_period
top_factors <- jk_flights %>%
  group_by(depart_day_period) %>%
  tally() %>%
  top_n(10, n) %>%
  pull(depart_day_period)

df_top <- jk_flights %>%
  filter(depart_day_period %in% top_factors)

ggplot(df_top, aes(x = depart_day_period, fill = target)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Comparativo com os Atrasos",
       x = "Período do Dia",
       y = "Frequência") +
  scale_fill_manual(values = c("skyblue", "salmon"), name = "Logical") +
  theme_minimal()
