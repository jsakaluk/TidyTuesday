#install.packages("cowplot")
#install.packages("tidytext")
#devtools::install_github("https://github.com/rensa/ggflags")
library(tidyverse)
library(ggflags)
library(tidytext)

####1) Data sets ####
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
###################################################
####2) Tidying Data####

#Recode as numeric
transit_cost$cost_km_millions <- as.numeric(transit_cost$cost_km_millions)

#Make g20 summary tibble by...
transit_g20_grp <- transit_cost %>%
  group_by(country) %>%#grouping by country
  summarise(avg_cost = mean(cost_km_millions)) %>%#calculate mean
  mutate(grp = case_when(country == "AU" ~ "g20",#categorize g20 nations
                         country == "CA" ~ "g20",
                         country == "SA" ~ "g20",
                         country == "US" ~ "g20",
                         country == "IN" ~ "g20",
                         country == "RU" ~ "g20",
                         country == "TR" ~ "g20",
                         country == "AR" ~ "g20",
                         country == "BR" ~ "g20",
                         country == "MX" ~ "g20",
                         country == "FR" ~ "g20",
                         country == "DE" ~ "g20",
                         country == "IT" ~ "g20",
                         country == "UK" ~ "g20",
                         country == "CN" ~ "g20",
                         country == "ID" ~ "g20",
                         country == "JP" ~ "g20",
                         country == "KR" ~ "g20"),
         Canada = ifelse(country == "CA", TRUE, FALSE),#highlight Canada
         country = fct_reorder(country, avg_cost),#sort countries by cost
         code = tolower(country),#need lower case to match country flag icons
         code = ifelse(code == "uk", "gb", code)) %>%#recode uk to gb for flag
  filter(grp == "g20")#only keep g20

g20mean <- transit_g20_grp %>%#caculate g20 average
  summarise(means = mean(avg_cost))

###################################################
###4) Visualizations####

g20country_cost <- transit_g20_grp %>% ggplot(aes(x = avg_cost, y = country))+
  geom_bar(stat = "identity", aes(fill = Canada))+#bars
  geom_flag(x = -1, aes(country = code), size = 4) +#snatch flag icons and put at x -1
  geom_vline(data = g20mean, mapping = aes(xintercept = means), linetype = "dashed")+#g20 average
  cowplot::theme_minimal_vgrid(16) +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    plot.caption = element_text(size = 6)
  )+
  scale_fill_manual(
    name = NULL,
    values = c("#B0B0B0D0", "#FF0000")#highlight canada
  )+
  labs(title = "Out for a (Costly) Rip?",
       subtitle = "Costs of Infrastructure Projects Among G20 Nations",
       caption = "Data Source:  Transit Costs Project (https://transitcosts.com) \n Unofficial Plot Theme Song: https://tinyurl.com/y2kctqt2",
       x = "Average Cost of Project per KM (in millions of USD)")
g20country_cost

ggsave(g20country_cost, filename = "./Infrastructure/country_cost.png", height = 7, width = 5, unit = "in", dpi = 300)

