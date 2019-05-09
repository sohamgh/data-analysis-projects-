library("tidyr")

library("dplyr")


x <- read.csv("populationdata.csv.csv" , stringsAsFactors = FALSE)
y <- read.csv("gdpdata.csv" , stringsAsFactors = FALSE)

View(x)
View(y)



library("httr")
library("jsonlite")
library("dplyr")
library("markdown")
library("knitr")
x <- read.csv("populationdata.csv.csv" , stringsAsFactors = FALSE)
y <- read.csv("gdpdata.csv" , stringsAsFactors = FALSE)

#####################data cleaning#########################################

countries <- c("United States", "Canada", "Afghanistan", "Albania", 
               "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", 
               "Antarctica", "Antigua and/or Barbuda", "Argentina", "Armenia", "Aruba", 
               "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", 
               "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", 
               "Bolivia", "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", 
               "British lndian Ocean Territory", "Brunei Darussalam", "Bulgaria", "Burkina Faso", 
               "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Cayman Islands", "Central African Republic", 
               "Chad", "Chile", "China", "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros", 
               "Congo", "Cook Islands", "Costa Rica", "Croatia (Hrvatska)", "Cuba", "Cyprus", "Czech Republic", 
               "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecudaor", "Egypt", "El Salvador",
               "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands (Malvinas)", 
               "Faroe Islands", "Fiji", "Finland", "France", "France, Metropolitan", "French Guiana", 
               "French Polynesia", "French Southern Territories", "Gabon", "Gambia", "Georgia", "Germany", 
               "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", 
               "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard and Mc Donald Islands", "Honduras", "Hong Kong", 
               "Hungary", "Iceland", "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Ireland", "Israel", 
               "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", 
               "Korea, Democratic People's Republic of", "Korea, Republic of", "Kuwait", "Kyrgyzstan", 
               "Lao People's Democratic Republic", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libyan Arab Jamahiriya",
               "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Macedonia", "Madagascar", "Malawi", "Malaysia", 
               "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", 
               "Mexico", "Micronesia, Federated States of", "Moldova, Republic of", "Monaco", "Mongolia", "Montserrat", 
               "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "Netherlands Antilles", 
               "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfork Island", 
               "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea",
               "Paraguay", "Peru", "Philippines", "Pitcairn", "Poland", "Portugal", "Puerto Rico", "Qatar", "Reunion", 
               "Romania", "Russian Federation", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
               "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", 
               "Senegal", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", 
               "Somalia", "South Africa", "South Georgia South Sandwich Islands", "Spain", "Sri Lanka", "St. Helena",
               "St. Pierre and Miquelon", "Sudan", "Suriname", "Svalbarn and Jan Mayen Islands", "Swaziland", "Sweden", 
               "Switzerland", "Syrian Arab Republic", "Taiwan", "Tajikistan", "Tanzania, United Republic of", "Thailand",
               "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
               "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
               "United States minor outlying islands", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City State",
               "Venezuela", "Vietnam", "Virigan Islands (British)", "Virgin Islands (U.S.)", "Wallis and Futuna Islands", 
               "Western Sahara", "Yemen", "Yugoslavia", "Zaire", "Zambia", "Zimbabwe") ;
c <- unname(unlist(x[1,]))
colnames(x) <-  c
colnames(x)[colnames(x) == ''] <- "Region"
population_data_by_country <- x %>% subset(Region %in% countries) %>% filter(Year == "2015" | Year == "2010")%>% select(Region, Year, Series, Value) %>% spread(Series, Value)


c2 <- unname(unlist(y[1,]))
colnames(y) <-  c2
colnames(y)[colnames(y) == ''] <- "Region"
gdp_data_by_country <- y %>% subset(Region %in% countries) %>% filter(Year == "2010" | Year == "2015") %>% select(Region, Year, Series, Value) %>% spread(Series, Value)

## joins data tables by country.
all_data <- full_join(population_data_by_country , gdp_data_by_country, by = NULL, type = "full" , match = "all")



####################data analysis#################################

## seperates data table into two data tables by Year(2015 ad 2010)
all_data_2015 <-  all_data %>% filter(Year == 2015)
all_data_2010 <- all_data %>% filter(Year == 2010)

#coverted the data in the GDP specific colums from a string to a numeric for 2015.
all_data_2015$`GDP in constant 2010 prices (millions of US dollars)` <- as.numeric(gsub(",","",all_data_2015$`GDP in constant 2010 prices (millions of US dollars)`))
all_data_2015$`GDP in current prices (millions of US dollars)` <- as.numeric(gsub(",","",all_data_2015$`GDP in current prices (millions of US dollars)`))
all_data_2015$`GDP per capita (US dollars)` <- as.numeric(gsub(",","",all_data_2015$`GDP per capita (US dollars)`))

#coverted the data in the GDP specific colums from a string to a numeric for 2010.
all_data_2010$`GDP in constant 2010 prices (millions of US dollars)` <- as.numeric(gsub(",","",all_data_2010$`GDP in constant 2010 prices (millions of US dollars)`))
all_data_2010$`GDP in current prices (millions of US dollars)` <- as.numeric(gsub(",","",all_data_2010$`GDP in current prices (millions of US dollars)`))
all_data_2010$`GDP per capita (US dollars)` <- as.numeric(gsub(",","",all_data_2010$`GDP per capita (US dollars)`))

#coverted the data in all other columns from a string to a numeric for 2010.
all_data_2010$`Infant mortality for both sexes (per 1,000 live births)` <- as.numeric(all_data_2010$`Infant mortality for both sexes (per 1,000 live births)`)
all_data_2010$`Life expectancy at birth for both sexes (years)` <- as.numeric(all_data_2010$`Life expectancy at birth for both sexes (years)`)
all_data_2010$`Life expectancy at birth for females (years)` <- as.numeric(all_data_2010$`Life expectancy at birth for females (years)`)
all_data_2010$`Life expectancy at birth for males (years)` <- as.numeric(all_data_2010$`Life expectancy at birth for males (years)`)
all_data_2010$`Maternal mortality ratio (deaths per 100,000 population)` <- as.numeric(all_data_2010$`Maternal mortality ratio (deaths per 100,000 population)`)
all_data_2010$`Population annual rate of increase (percent)` <- as.numeric(all_data_2010$`Population annual rate of increase (percent)`)
all_data_2010$`Total fertility rate (children per women)` <- as.numeric(all_data_2010$`Total fertility rate (children per women)`)
all_data_2010$`GDP real rates of growth (percent)` <- as.numeric(all_data_2010$`GDP real rates of growth (percent)`)

#coverted the data in all other columns from a string to a numeric for 2015.
all_data_2015$`Infant mortality for both sexes (per 1,000 live births)` <- as.numeric(all_data_2015$`Infant mortality for both sexes (per 1,000 live births)`)
all_data_2015$`Life expectancy at birth for both sexes (years)` <- as.numeric(all_data_2015$`Life expectancy at birth for both sexes (years)`)
all_data_2015$`Life expectancy at birth for females (years)` <- as.numeric(all_data_2015$`Life expectancy at birth for females (years)`)
all_data_2015$`Life expectancy at birth for males (years)` <- as.numeric(all_data_2015$`Life expectancy at birth for males (years)`)
all_data_2015$`Maternal mortality ratio (deaths per 100,000 population)` <- as.numeric(all_data_2015$`Maternal mortality ratio (deaths per 100,000 population)`)
all_data_2015$`Population annual rate of increase (percent)` <- as.numeric(all_data_2015$`Population annual rate of increase (percent)`)
all_data_2015$`Total fertility rate (children per women)` <- as.numeric(all_data_2015$`Total fertility rate (children per women)`)
all_data_2015$`GDP real rates of growth (percent)` <- as.numeric(all_data_2015$`GDP real rates of growth (percent)`)

# takes the average value of all of the relevant columns for 2010 data.
averages_2010 <- summarize(all_data_2010, average_infant_mortallity = mean(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), average_life_expectancy = mean(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
  avarage_population_rate_change = mean(`Population annual rate of increase (percent)`,na.rm = TRUE),avarage_fertility_rate = mean(`Total fertility rate (children per women)`,na.rm = TRUE),avarage_GDP_current_prices = mean(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
  avarage_GDP_per_capita = mean(`GDP per capita (US dollars)`,na.rm = TRUE))

# takes the average value of all of the relevant columns for 2015 data.
averages_2015 <- summarize(all_data_2015, average_infant_mortallity = mean(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), average_life_expectancy = mean(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
                           avarage_population_rate_change = mean(`Population annual rate of increase (percent)`,na.rm = TRUE),avarage_fertility_rate = mean(`Total fertility rate (children per women)`,na.rm = TRUE),avarage_GDP_current_prices = mean(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
                           avarage_GDP_per_capita = mean(`GDP per capita (US dollars)`,na.rm = TRUE))

# takes the minimum value of all of the relevant columns for 2010 data.
min_2010 <- summarize(all_data_2010, min_infant_mortallity = min(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), min_life_expectancy = min(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
                           min_population_rate_change = min(`Population annual rate of increase (percent)`,na.rm = TRUE),min_fertility_rate = min(`Total fertility rate (children per women)`,na.rm = TRUE),min_GDP_current_prices = min(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
                           min_GDP_per_capita = min(`GDP per capita (US dollars)`,na.rm = TRUE))

# takes the minimum value of all of the relevant columns for 2015 data.
min_2015 <- summarize(all_data_2015, min_infant_mortallity = min(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), min_life_expectancy = min(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
                      min_population_rate_change = min(`Population annual rate of increase (percent)`,na.rm = TRUE),min_fertility_rate = min(`Total fertility rate (children per women)`,na.rm = TRUE),min_GDP_current_prices = min(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
                      min_GDP_per_capita = min(`GDP per capita (US dollars)`,na.rm = TRUE))

# takes the maximum value of all of the relevant columns for 2010 data.
max_2010 <- summarize(all_data_2010, max_infant_mortallity = max(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), max_life_expectancy = max(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
                      max_population_rate_change = max(`Population annual rate of increase (percent)`,na.rm = TRUE),max_fertility_rate = max(`Total fertility rate (children per women)`,na.rm = TRUE),max_GDP_current_prices = max(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
                      max_GDP_per_capita = max(`GDP per capita (US dollars)`,na.rm = TRUE))

# takes the maximum value of all of the relevant columns for 2015 data.
max_2015 <- summarize(all_data_2015, max_infant_mortallity = max(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), max_life_expectancy = max(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
                      max_population_rate_change = max(`Population annual rate of increase (percent)`,na.rm = TRUE),max_fertility_rate = max(`Total fertility rate (children per women)`,na.rm = TRUE),max_GDP_current_prices = max(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
                      max_GDP_per_capita = max(`GDP per capita (US dollars)`,na.rm = TRUE))

# takes the standard deviation of all of the relevant columns for 2010 data.
sd_2010 <- summarize(all_data_2010, sd_infant_mortallity = sd(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), sd_life_expectancy = max(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
                      sd_population_rate_change = sd(`Population annual rate of increase (percent)`,na.rm = TRUE),sd_fertility_rate = sd(`Total fertility rate (children per women)`,na.rm = TRUE),sd_GDP_current_prices = sd(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
                      sd_GDP_per_capita = sd(`GDP per capita (US dollars)`,na.rm = TRUE))

# takes the standard deviation of all of the relevant columns for 2015 data.
sd_2015 <- summarize(all_data_2010, sd_infant_mortallity = sd(`Infant mortality for both sexes (per 1,000 live births)`,na.rm = TRUE), sd_life_expectancy = max(`Life expectancy at birth for both sexes (years)`,na.rm = TRUE),
                     sd_population_rate_change = sd(`Population annual rate of increase (percent)`,na.rm = TRUE),sd_fertility_rate = sd(`Total fertility rate (children per women)`,na.rm = TRUE),sd_GDP_current_prices = sd(`GDP in current prices (millions of US dollars)`,na.rm = TRUE),
                     sd_GDP_per_capita = sd(`GDP per capita (US dollars)`,na.rm = TRUE))

difference_gpd <- averages_2015$avarage_GDP_per_capita - averages_2010$avarage_GDP_per_capita #finds the diffrance in gpd 
difference_mortality <- averages_2015$average_infant_mortallity - averages_2010$average_infant_mortallity #finds the diffrance in mortaility 
difference_fertility <- averages_2015$avarage_fertility_rate - averages_2010$avarage_fertility_rate #finds the diffrance in fertility
population_rate <-  averages_2015$avarage_population_rate_change - averages_2010$avarage_population_rate_change #finds the diffrance in population
difference_in_5_years <- averages_2015 - averages_2010 #finds the diffracne between both data sets from 2015 to 2010

average_fert_2015 <- averages_2015$avarage_fertility_rate 
##gets the global average for population growth in 2015
average_population_growth_2015 <- averages_2015$avarage_population_rate_change
## difference in above values in 2015
difference_fert_popgrowth_2015 <- average_fert_2015 - average_population_growth_2015 
#gets the global average of fertility rate for 2010
average_fert_2010 <- averages_2010$avarage_fertility_rate 
#gets the global average for population growth in 2010
average_population_growth_2010 <- averages_2010$avarage_population_rate_change
##difference in above values in 2010
difference_fert_popgrowth_2010 <- average_fert_2010 - average_population_growth_2010 

## Which country has the greatest difference in fertility rate and population growth in 2010 and 2015. 
##selects the country with the greatest difference. 
greatest_diff <- all_data_2010 %>% 
  mutate(fert_pop_diff = `Total fertility rate (children per women)` - `Population annual rate of increase (percent)`) %>%
  filter(fert_pop_diff == max(fert_pop_diff, na.rm = TRUE)) %>%
  select(Region)

