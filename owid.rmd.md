---
title: "COVID from Our World in Data"
author: "Ian McDonald"
date: "10/7/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(RcppRoll)
library(wesanderson)
library(bbplot)

```

### Overview

These graphs summarize Covid data available from the Our World in Data project at the University of Oxford. This project will use the data to demonstrate basic features of the ggplot data visualization environment.

### Initialize

This section is used to designate the countries and continents displayed on the graph. Note that continent summaries will exclude data from any specific countries designated in the breakout\_group vector.

The country\_colours vector defines color values in alphabetic order on the graphs. This isn't correct; the values should actually attach to specific countries (e.g., Canada is always red).

```{r initialize}
graph_line_width <- 1.5
breakout_group <- c("USA", "Canada", "UK")
regions_on_graph <- c("Europe")

country_colours <- c(Canada="#bf1f2c", UK="#079411","#20d4d4",USA="#101cc7")
  

```

### Import Data from Our World in Data

```{r import_data, include=FALSE}

covid_global_raw <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", col_types = cols(.default = "d", iso_code = "c", continent = "c", location = "c", date = "D", tests_units = "c"))

```

### Adapt some variable names in a copy of the data table

filter some rows from the raw data table, adjust some variable names, change country values to something more concise.

```{r adapt_data_table, include = FALSE}

covid_global_new <- covid_global_raw %>% 
  filter(continent != "world") %>%
  rename(country = location, region = continent, pop_2020 = population) %>%
  mutate(country = ifelse(country == "United States","USA", country),
         country = ifelse(country == "United Kingdom", "UK", country)) %>%
  select(region, country, date, total_cases, new_cases, total_deaths, new_deaths, pop_2020)

```

### Country count check

Sometimes the data does not provide an update for each country on new days. If we find a short country count, we just drop the latest date. The graphs will catch up the next day, but we need more data validity checks.

```{r country_count_check}

latest_date_country_count <- covid_global_new %>%
  filter(date >= max(date) -1) %>%
  count(region, date) %>%
  pivot_wider(region, names_from = date, values_from = n)

if (sum(latest_date_country_count[2] != latest_date_country_count[3]) > 0) {
  covid_global_new <- covid_global_new %>% 
    filter(date < max(date)) 
}
```

### Pick countries and consolidate regions

We limit the data set to the countries chosen in the breakout\_group variable, and consolidate the continents.

Notice that we exclude chosen countries from the continent totals. We can report these modified totals as "Rest of \_\_\_\_\_\_\_" (e.g., "Rest of Europe"

```{r region_totals_and_preseerve_chosen_countries}

region_totals <- covid_global_new %>% 
  filter(!country %in% breakout_group) %>% 
  group_by(region, date) %>% 
  summarise(new_cases = sum(new_cases), total_cases = sum(total_cases),
            new_deaths = sum(new_deaths), total_deaths = sum(total_deaths),
            pop_2020 = sum(pop_2020)) %>%
  ungroup() %>%
  mutate(country = region)

covid_global_new <- rbind(covid_global_new, region_totals) 

covid_global_new <- covid_global_new %>% 
  filter(country == region | country %in% breakout_group)


```

```{r}

breakout_group_regions <- covid_global_new %>% filter(country %in% breakout_group) %>% 
  distinct(region)

covid_global_new <- covid_global_new %>%
  group_by(country) %>%
  filter(total_deaths > 0) %>%
  mutate(date_since_first = date - min(date)) %>%
  ungroup(country) 

```

```{r}

covid_global_new <- covid_global_new %>% 
  mutate(region =
           ifelse(country %in% breakout_group, country, region)) %>%
  mutate(country = 
           ifelse(country %in% breakout_group_regions$region & 
                    !country %in% breakout_group, str_c("Rest of ",region), country)) 

```

```{r}

covid_global_new <- covid_global_new %>% 
  mutate(new_cases_pc = new_cases / pop_2020 * 1e+6) %>%
  mutate(new_deaths_pc = new_deaths / pop_2020 * 1e+6) %>%
  mutate(cum_cases_pc = total_cases / pop_2020 * 1e+6) %>%
  mutate(cum_deaths_pc = total_deaths / pop_2020 * 1e+6) %>%
  mutate(new_cases_pc_roll = roll_mean(new_cases_pc, 14, align="right", fill=0)) %>%
  mutate(new_deaths_pc_roll = roll_mean(new_deaths_pc, 14, align="right", fill=0))

```

```{r}

covid_global_graph <- covid_global_new %>% 
  filter(date >= max(date) - 180) %>%
  filter(country %in% breakout_group | region %in% regions_on_graph) #! regions!

l_graph_5 <- covid_global_graph %>% ggplot(aes(x = date, y = cum_deaths_pc, color = country)) + 
  geom_line(size = graph_line_width) +
  labs(x = "Date Reported (2020)", y = "Cumulative Deaths per Million",
       title = "Cumulative COVID Deaths Per Million",
       caption = "Source: WHO and UN Population Division") +
  theme(legend.position = "top") +
  scale_colour_manual(values=country_colours) +
    bbc_style()

l_graph_5
ggsave("plots/l_graph_5.pdf")

```

```{r}

l_graph_6 <- covid_global_graph %>% ggplot(aes(x = date, y = cum_cases_pc, color = country)) +
  geom_line(size = graph_line_width) +
  labs(x = "Date Reported (2020)", y = "Cumulative Cases per Million",
       title = "Cumulative COVID Cases Per Million",
       caption = "Source: WHO and UN Population Division") +
  theme(legend.position = "top") +
  scale_colour_manual(values=country_colours) +
  bbc_style()

l_graph_6
ggsave("plots/l_graph_6.pdf")

```

```{r}

l_graph_7 <- covid_global_graph %>% ggplot(aes(x = date, y = new_cases_pc_roll, color = country)) +
  geom_line(size = graph_line_width) +
  labs(x = "Date Reported", y = "New Cases per Million",
       title = "New COVID Cases Per Million",
       subtitle = "Per day, 14 day rolling average",
       caption = "Source: WHO and UN Population Division")+
  theme(legend.position = "top") +
  scale_colour_manual(values=country_colours) +
 bbc_style()

l_graph_7
ggsave("plots/l_graph_7.pdf")
```

```{r}

l_graph_8 <- covid_global_graph %>% ggplot(aes(x = date, y = new_deaths_pc_roll, color = country)) +
  geom_line(size = graph_line_width) +
  labs(x = "Date Reported", y = "New Deaths per Million",
       title = "New COVID Deaths Per Million",
       subtitle = "Per day, 14 day rolling average",
       caption = "Source: WHO and UN Population Division")+
  theme(legend.position = "top") +
  scale_colour_manual(values=country_colours) +
  bbc_style()

l_graph_8
ggsave("plots/l_graph_8.pdf")
```

```{r}


l_graph_9 <- covid_global_graph %>% ggplot(aes(x = date, y = total_deaths, color = country)) +
  geom_line(size = graph_line_width) +
  labs(x = "Date Reported", y = "Cumulative Deaths",
       title = "Cumulative COVID Deaths",
       caption = "Source: WHO and UN Population Division")+
  theme(legend.position = "top") +
  scale_colour_manual(values=country_colours) +
  bbc_style()

l_graph_9
ggsave("plots/l_graph_9.pdf")

```
