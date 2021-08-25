---
title: "Forest Fires in Brazil"
author: "Fábio Chacon"
output:
  html_document:
    number_sections: true
    toc: true
    code_folding: "hide"
    theme: readable
    highlight: haddock
---

```{r, message = F, warning = F}
knitr::opts_chunk$set(message = F, fig.align="center", warning = F)
        
# Importing libraries
library(tidyverse)
library(maptools)
library(raster)
library(rgdal)

# Setting theme
theme_set(theme_light())

# Loading data
data <- read_csv('/kaggle/input/forest-fires-in-brazil/amazon.csv')


data <- data %>%
  mutate(state = recode(state, "Piau"="Piauí",
                               "Rio"="Rio de Janeiro",
                               "Maranhao"="Maranhão",
                               "Paraiba"="Paraíba",
                               "Ceara"="Ceará",
                               "Goias"="Goiás",
                               "Rondonia"="Rondônia",
                               "Espirito Santo"="Espírito Santo",
                               "Sao Paulo"="São Paulo",
                               "Amapa"="Amapá"))

# Correcting encoding errors
data$month <- iconv(data$month, sub = "ç")
data$state <- iconv(data$state, sub = "á")


# Translating months to english
data <- data %>%
  mutate(month = recode(month, "Janeiro"="Jan",
                               "Fevereiro"="Feb",
                               "Março"="Mar",
                               "Abril"="Apr",
                               "Maio"="May",
                               "Junho"="Jun",
                               "Julho"="Jul",
                               "Agosto"="Aug",
                               "Setembro"="Sep",
                               "Outubro"="Oct",
                               "Novembro"="Nov",
                               "Dezembro"="Dec"
                        ))
```

\

# **Trends in Brazil**


## **By Year**

```{r}
by_year <- data %>%
  group_by(year) %>%
  summarise(fires = sum(number))

plot_by_year <- ggplot(by_year, mapping = aes(x = year, y = fires))

plot_by_year + 
  geom_line(size = 1, color = "#e84d60") + 
  geom_point(size = 2, color = "#e84d60") +
  scale_x_continuous(breaks = seq(1998, 2017, 3), minor_breaks = F) +
  labs(x = "Year", 
       y = "Number of Fires", 
       title = "Forest Fires in Brazil, per Year", 
       subtitle = "(Trends over time, 1998-2017)") +
  theme(plot.subtitle = element_text(size = rel(0.9)))
```
\

Trends of reported fires increased steadily from 1998, peaked around 2003, and decreased subsequently. In 2016, forest fires assumed almost the same proportions as in 2003.

\

## **By Month**



```{r, fig.width=9, fig.height=12}
by_month_year <- data %>%
  group_by(year, month) %>%
  summarise(fires = sum(number))

by_month_year$month <- factor(by_month_year$month, ordered = T, levels = rev(unique(data$month)))
by_month_year$year <- factor(by_month_year$year, ordered = T, levels = unique(data$year))


plot_by_month_year <- ggplot(by_month_year, aes(x = month, y = fires, fill = year))
plot_by_month_year +
  geom_col() +
  coord_flip() +
  facet_wrap(~year) +
  guides(fill = F) +
  labs(x = "Month", y = "Reported Fires", title = "Reported Fires for Years 1998 to 2017") +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12))
```

\

\

```{r}
by_month <- data %>%
  group_by(month) %>%
  summarise(fires = sum(number)) %>%
  mutate(freq = fires / sum(fires))

by_month$month <- factor(by_month$month, ordered = T, levels = unique(data$month))

plot_by_month <- ggplot(by_month, aes(x = month, y = freq))
plot_by_month +
  geom_bar(aes(fill = month), stat = "identity") +
  guides(fill = F) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, max(by_month$freq), 0.02), minor_breaks = F) +
  labs(x = "Month", 
       y = "Reported Fires (%) ", 
       title = "Percentage of Reported Fires, by Month",
       subtitle = "(1998-2017)") +
  theme(axis.text.x = element_text(size=rel(1.2)))
``` 

\

Between 1998 and 2017, most forest fires were reported in July, with February and March being distinguished by a small percentage.

\

# **Analysing States**

\

## **By State**
```{r, fig.width=8}
by_state <- data %>%
  group_by(state) %>%
  summarise(fires = round(sum(number))) %>%
  arrange(desc(fires))

plot_by_state <- ggplot(by_state[1:10, ], aes(x = reorder(state, fires), y = fires)) 
plot_by_state +
  geom_bar(aes(fill = state), stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(by_state$fires), 1e4)) +
  guides(fill = F) +
  labs(x = "State", 
       y =  "Reported Fires", 
       title = "Forest Fires in Brazil",
       subtitle = "(Top 10 States)") +
  theme(title = element_text(size = 11),
        axis.text.y = element_text(size = rel(1.2)))
```

\

## **By State & Year**
```{r, fig.height=7, fig.width=9}
# Grouping by state and year  
by_state_time <- data %>%
  group_by(state, year) %>%
  summarise(fires = sum(number))

by_state_time$state <- factor(by_state_time$state, ordered = T, levels = by_state$state)

plot_state_time <- ggplot(subset(by_state_time, subset = state %in% by_state[1:9, ]$state), aes(x = year, y = fires, col = state))
plot_state_time +
  geom_line() +
  geom_point() +
  facet_wrap(~state) +
  scale_x_continuous(breaks = seq(1998, 2017, 4), minor_breaks = F) +
  labs(title = "Reported Fires Over Time",
       subtitle = "Trends from 1998 to 2017",
       x = "Year", 
       y = "Reported Fires") +
  theme(title = element_text(size = 11)) +
  guides(color = F)
```

\

## **By Month**
```{r, fig.width=8, fig.height=6}
by_month_state <- data %>%
  group_by(month, state) %>%
  summarise(fires = sum(number))

by_month_state$month <- factor(by_month_state$month, ordered = T, levels = unique(data$month))

by_month_state <- ggplot(subset(by_month_state, state %in% by_state[1:4, ]$state), aes(x = month, y = fires))
by_month_state +
  geom_bar(aes(fill = state), stat = "identity", position = "dodge2") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Month", 
       y = "Reported Fires", 
       fill = "State",
       title = "Reported Fires, by Month", 
       subtitle = "(Months for each year, from 1998 to 2017)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=rel(1.2)),
        plot.subtitle = element_text(size = rel(0.9)))
```
\

\

## **Choropleth Map for Forest Fires**

\

### ***By State***

```{r}
mapData <- getData(country = "BRA", level = 1)
mapData@data <- dplyr::select(mapData@data, "NAME_1")

mapDataState <- mapData

colnames(mapDataState@data) <- "state"
mapDataState@data$id <- rownames(mapDataState@data)
mapDataState@data <- merge(mapDataState@data, by_state, by = "state", all = T)

mapDF <- broom::tidy(mapDataState)
mapDF <- merge(mapDF, mapDataState@data, by = "id")

# Defines a function for the map theme.
map_theme <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0))
}

# Plots a map
ggplot(data = mapDF, mapping = aes(x = long, y = lat, group = group, fill = fires)) +
  geom_polygon(color = "gray", size = 0.2) + 
  coord_equal() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  map_theme() +
  labs(fill = "Fires", title = "Forest Fires in Brazil") +
  theme(plot.title = element_text(size = rel(1.3),
                                  face = "italic"))
```

\

The gray areas correspond to Mato Grosso do Sul, Rio Grande do Norte, Paraná and Rio Grande do Sul, which are missing from the data.

\

### ***By State & Year***

```{r, fig.height=17, fig.width=10}
mapDataTime <- mapData

colnames(mapDataTime@data) <- "state"
mapDataTime@data$id <- rownames(mapDataTime@data)
mapDataTime@data <- merge(mapDataTime@data, by_state_time, by = "state", all = T)

mapDFTime <- broom::tidy(mapDataTime)
mapDFTime <- merge(mapDFTime, mapDataTime@data, by = "id")

mapDFTime$year <- factor(mapDFTime$year, ordered = T, levels = unique(by_state_time$year))


ggplot(data = mapDFTime, mapping = aes(x = long, y = lat, group = group, fill = fires)) +
  geom_polygon(color = "gray60", size = 0.05) + 
  facet_wrap(~ year, ncol = 3, drop = F) +
  coord_equal() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Reported Forest Fires by State, 1998-2017", fill = "Fires") +
  map_theme() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", size = rel(1.3),
                                  margin = margin(0, 0, 30, 0)),
        strip.background = element_blank(),
        strip.text.x = element_text(color = "black", size = rel(1.5)))
```