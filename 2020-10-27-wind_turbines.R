#' ---
#' title: "Datasaurus"
#' author: "Martin Wong"
#' date: "October 13, 2020"
#' ---

# set directory
setwd("~/r_viz/tidytuesday")

#```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

```{r Load, message=FALSE, warning=FALSE}
library(tidyverse)
library(tidytuesdayR)
library(fontawesome) # for knitting, install with devtools::install_github("rstudio/fontawesome")
library(rnaturalearth)
library(gganimate)
# ggpairs for pair plots

```

df_wind <- read_csv("data/2020/2020-10-27/wind-turbine.csv")

```
# Explore data

df_wind %>%
  arrange(desc(turbine_number_in_project)) %>%
  head()

df_wind_subset <- df_wind

df_wind_subset %>%
  filter(province_territory == "Alberta")

# general plots
ggplot(data=df_wind)+
  geom_col(aes(x=province_territory, y=total_project_capacity_mw))+
  coord_flip()+
  labs(x="Territory", y="Project capcaity (mw)",size=20)+
  theme_bw()+
  theme(axis.text.y = element_text(angle = 0, size = 10,
                                   vjust = 0.5))+
  theme(axis.text.x = element_text(angle = 0, size = 10,
                                   vjust = 0.5))+
  theme(axis.title.y = element_text(angle = 0, size = 14,
                                   vjust = 0.5))+
  theme(axis.title.x = element_text(angle = 0, size = 14,
                                    vjust = 0.5))

ggplot(data=df_wind)+geom_violin(aes(x=province_territory, y=total_project_capacity_mw))

ggplot(data=df_wind)+geom_point(aes(x=manufacturer,y=province_territory, size = rotor_diameter_m))+ 
  scale_radius(range = c(1,6))+
  theme_bw()

qplot(data=df_wind,x=rotor_diameter_m, y=total_project_capacity_mw,geom="hex")+theme_dark()

df_wind_subset <- df_wind

df_wind_subset %>%
  group_by(manufacturer) %>%
  dplyr::summarize(Mean = mean(rotor_diameter_m, na.rm=TRUE))

# map
sf_canada <- rnaturalearth::ne_countries(scale=110,country="Canada",returnclass = "sf") %>%

sf_wind <-
  df_wind %>%
  st_as_sf()

ggplot(sf_canada) + geom_polygon(data = df_wind, aes(x=longitude , y = latitude)) 

# gganimate

wind_test <- df_wind
wind_test$commissioning_date <- as.integer(wind_test$commissioning_date)

wind_test <- df_wind                                                    %>% 
  group_by(`province_territory`)                                    %>%
  gather(key = commissioning_date, value = rotor_diameter_m, 
         -province_territory, -total_project_capacity_mw, -turbine_rated_capacity_k_w) %>% 
  as.data.frame
#df$GDP <- rep(europe.gdp$`GDP per Capita`, 40)
wind_test$commissioning_date <- as.integer(wind_test$commissioning_date)

p <- ggplot(
  data=wind_test, 
  aes(x = turbine_rated_capacity_k_w, y=rotor_diameter_m, size = total_project_capacity_mw, colour = province_territory)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +transition_time(commissioning_date) 
  #scale_size(range = c(2, 12)) +
  #scale_x_log10() +
  #labs(x = "GDP per capita", y = "Life expectancy")
  transition_time(commissioning_date) 
  #labs(title = "Year: {frame_time}")

library(gapminder)
  p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p  
p + transition_time(year) +
    labs(title = "Year: {frame_time}")


                       