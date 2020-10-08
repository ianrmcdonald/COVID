library(tidyverse)
library(here)
library(RcppRoll)
library(wesanderson)
library(bbplot)


graph_line_width <- 1.5
breakout_group <- c("USA", "Canada", "UK")
regions_on_graph <- c("Europe")

country_colours <- c(Canada="#bf1f2c", UK="#079411","#20d4d4",USA="#101cc7")
  


covid_global_raw <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", col_types = cols(.default = "d", iso_code = "c", continent = "c", location = "c", date = "D", tests_units = "c"))

covid_global_new <- covid_global_raw %>% 
  filter(continent != "world") %>%
  rename(country = location, region = continent, pop_2020 = population) %>%
  mutate(country = ifelse(country == "United States","USA", country),
         country = ifelse(country == "United Kingdom", "UK", country)) %>%
  select(region, country, date, total_cases, new_cases, total_deaths, new_deaths, pop_2020)

####
##  Problem that needs to be addressed
##  The extract does no guarantaee that every country will see new data
##  Which can screw up the continent calculations.  We say cumulative go down because of this
#### Needs to insert a check that either excludes a missing country from the calculation
## or pulls in previous day data if it doesn't find it.

latest_date_country_count <- covid_global_new %>%
  filter(date >= max(date) -1) %>%
  count(region, date) %>%
  pivot_wider(region, names_from = date, values_from = n)

if (sum(latest_date_country_count[2] != latest_date_country_count[3]) > 0) {
  covid_global_new <- covid_global_new %>% 
    filter(date < max(date)) 
}

## this might be usable in a check to see if all the countries reported.
## the best solution might be to shut of the latest day if there is a mismatch


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

#covid_global_wpop <- covid_global_wpop %>% 
#  left_join(WHO_region_names, by = c("region" = "region_code")) 

breakout_group_regions <- covid_global_new %>% filter(country %in% breakout_group) %>% 
  distinct(region)

covid_global_new <- covid_global_new %>%
  group_by(country) %>%
  filter(total_deaths > 0) %>%
  mutate(date_since_first = date - min(date)) %>%
  ungroup(country) 

#covid_global_new <- covid_global_new %>% 
#  mutate(country = ifelse(country == region, region, country))

covid_global_new <- covid_global_new %>% 
  mutate(region =
           ifelse(country %in% breakout_group, country, region)) %>%
  mutate(country = 
           ifelse(country %in% breakout_group_regions$region & !country %in% breakout_group, str_c("Rest of ",region), country)) 

#%>%
#  mutate(region = 
#          ifelse(region %in% breakout_group_regions$region, str_c("Rest of ", region), region)) 


covid_global_new <- covid_global_new %>% 
  mutate(new_cases_pc = new_cases / pop_2020 * 1e+6) %>%
  mutate(new_deaths_pc = new_deaths / pop_2020 * 1e+6) %>%
  mutate(cum_cases_pc = total_cases / pop_2020 * 1e+6) %>%
  mutate(cum_deaths_pc = total_deaths / pop_2020 * 1e+6) %>%
  mutate(new_cases_pc_roll = roll_mean(new_cases_pc, 14, align="right", fill=0)) %>%
  mutate(new_deaths_pc_roll = roll_mean(new_deaths_pc, 14, align="right", fill=0))
#fix these variable names


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

#l_graph_5 + scale_color_hue(l=60, c=30)
#l_graph_5 + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#l_graph_5 + scale_color_brewer(palette="PuOr")
#l_graph_5 + scale_x_date(date_labels = "%b")


ggsave("plots/l_graph_5.pdf")

l_graph_6 <- covid_global_graph %>% ggplot(aes(x = date, y = cum_cases_pc, color = country)) +
  geom_line(size = graph_line_width) +
  labs(x = "Date Reported", y = "Cumulative Cases per Million",
       title = "Cumulative COVID Cases Per Million",
       caption = "Source: WHO and UN Population Division") +
  theme(legend.position = "top") +
  scale_colour_manual(values=country_colours) +
  bbc_style()

l_graph_6
ggsave("plots/l_graph_6.pdf")


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
