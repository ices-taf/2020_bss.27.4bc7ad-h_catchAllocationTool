

library(ggplot2)
library(dplyr)
library(plotly)

load("data/globals.RData")


## Catch at age plot
catch_data <-
  cbind(
    Age = globals$ages,
    as.data.frame(catch_n)
  ) %>%
  pivot_longer(
    -Age,
    names_to = "Gear", values_to = "Catch"
  ) %>%
  mutate(
    Catch = round(Catch)
  )

catch_advice <-
  data.frame(
    Age = age_data$Age,
    Advice = round(age_data[[input$AdviceType]])
  )


p <-
  ggplot() +
  geom_area(
    data = catch_data, position = "stack",
    aes(x = Age, y = Catch, fill = Gear)
  ) +
  geom_line(
    data = catch_advice, linetype = 2,
    aes(x = Age, y = Advice)
  ) +
  ylab("Catch-at-Age (thousands)") +
  theme(plot.background = element_rect(fill = "grey96")) +
  theme(legend.background = element_rect(fill = "grey96", size = 0.5, linetype = "solid")) +
  theme(panel.background = element_rect(fill = "grey96"))
