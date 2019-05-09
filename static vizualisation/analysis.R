#install.packages("ggplot2")
#install.packages("maps")
library("ggplot2")
library("knitr")
library("dplyr")
library("tidyr")
library("maps")
options(scipen = 999)
forest_data <- read.csv("data/WBI_Forest_Area_Cleaned.csv", stringsAsFactor = FALSE)
##cleaning
forest_data <-  forest_data %>% 
  filter(Series.Name == "Forest area (% of land area)" |
           Series.Name == "Agricultural land (% of land area)" |
           Series.Name == "Forest area (sq. km)" | 
           Series.Name == "GDP (constant 2010 US$)" |
           Series.Name == "Population, total") %>%
          gather(key = 'year' , value = 'value', YR1992:YR2016)%>%
          select(-Series.Code , -X)##%>%
##mutate(year = as.numeric(substr(year, 3, nchar(year))) , GDP_per_capita = `GDP (constant 2010 US$)` / `Population, total` )

        forest_data_new <-  forest_data %>% 
          filter(year == '1992' | year == '2016')## %>% 
          filter(Series.Name == "GDP (constant 2010 US$)" ) %>%
          spread(year, Series.Name)
# Visualise 
# change in worlds average forest coverage over time.
#total countries in the world as per table
total_countries <- forest_data %>%
  filter(year == 1992) %>% nrow()
total_forest_by_year <- forest_data %>% 
  group_by(year) %>%
  summarise(total_forest_area = sum(`Forest area (sq. km)`/ total_countries, na.rm = T))

ggplot(data = total_forest_by_year) + 
  geom_point(mapping = aes(x = year , y = total_forest_area)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## visualising total agricultural area as percentage of country vs GDP per capita for certain countries
years_chose <-  c(1992 , 2010 , 2016 , 2000 )
tot_agri_perc_vs_gdp <- forest_data %>%
  select(Country.Name, year , `Agricultural land (% of land area)`, GDP_per_capita)  %>%
  filter(year %in% years_chose) 
  
##plot 1
ggplot(data = tot_agri_perc_vs_gdp, aes(log(GDP_per_capita) ,  `Agricultural land (% of land area)` )) +
geom_point() +
facet_wrap(~ year) 

## visualizing changes in agricultural land % of certain countries over the years.

countries_chose <- c("India","China","United States","Brazil","Haiti","Mexico")
tot_agri_perc_vs_time <- forest_data %>%
  select(Country.Name, year , `Agricultural land (% of land area)`)  %>%
  filter(Country.Name %in% countries_chose) 


ggplot(data = tot_agri_perc_vs_time, aes(  x = year , y = log10(`Agricultural land (% of land area)`) , color = Country.Name)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


###section 2
world_map <- map_data("world") %>%
  mutate(Country.Code = iso.alpha(region , 3))

forest_data_new <-  forest_data %>% 
  filter(year == 1992 | year == 2016) %>% 
  select(-`Agricultural land (% of land area)`, -`GDP (constant 2010 US$)` , -`Forest area (sq. km)`, -GDP_per_capita , -`Population, total`) %>%
  spread(year, `Forest area (% of land area)`)

colnames(forest_data_new)[colnames(forest_data_new) == "1992"] = "Forest area (% of land area) in 1992"
colnames(forest_data_new)[colnames(forest_data_new) == "2016"] = "Forest area (% of land area) in 2016"

forest_data_new <-  forest_data_new %>%
  mutate(change = `Forest area (% of land area) in 2016`- `Forest area (% of land area) in 1992`)

world_forest_map <- left_join(world_map, forest_data_new, by = "Country.Code") 


##finding the 5 bins 
bin_values <- quantile(world_forest_map$change , probs = c(0, 0.2, 0.4, 0.6, 0.8, 1) , na.rm = T)

world_forest_map <- world_forest_map %>% 
  mutate(`Percentage change` = cut(change, breaks=bin_values, labels=c("-30% to -2%", "-2% to -0.1%", "-0.1% to 0.4%", "0.4% to 1.5%", "1.5% to 24.6%")))
  

ggplot(data = world_forest_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = `Percentage change`)) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Change in forest cover as a percentage of a country's land area(1992 - 2016)" , x = "", y = "" , fill = "change") +
  coord_quickmap() +
  theme(legend.position = "bottom")



