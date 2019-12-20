library(ggplot2)
library(dplyr)
library(nycflights13)
install.packages("nycflights13")


portland_flights <- flights %>% filter(dest == "PDX")

View(portland_flights)


btv_sea_flights_fall <- flights %>% 
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)

View(btv_sea_flights_fall)


summary_temp <- weather %>% summarize(mean = mean(temp,na.rm = TRUE),
                                      std_dev = sd(temp, na.rm = TRUE))

summary_temp

summary_temp <- weather %>% summarize(mean = mean(temp,na.rm = TRUE),
                                      std_dev = sd(temp, na.rm = TRUE),
                                      count = n())

summary_temp


summary_monthly_temp <- weather %>% 
  group_by(month) %>%
  summarise(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))

summary_monthly_temp

diamonds

diamonds %>% group_by(cut)

diamonds

by_origin_monthly <- flights %>%
  group_by(origin,month) %>%
  summarize(count = n())

by_origin_monthly

View(weather)



summary_monthly_temp_NYC <- weather %>% 
  filter(origin == "EWR") %>%
  group_by(month) %>%
  summarise(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))

summary_monthly_temp_NYC

summary_temp_dayly_NYCsummary_monthly_temp <- weather %>% 
  group_by(month) %>%
  summarise(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))

summary_monthly_temp <- weather %>%
  filter(origin == "JFK" & year == 2013) %>%
  group_by(day) %>%
  summarize(mean(temp),sd(temp))

summary_monthly_temp

View(flights)


n_flights_per_airport_and_carrier <- flights %>%
  group_by(origin,carrier) %>% 
  summarize(cantidad=n())

n_flights_per_airport_and_carrier



by_origin_monthly <- flights %>%
  group_by(month,origin) %>%
  summarize(count = n())

by_origin_monthly

airlines
airports

glimpse(flights)


flights %>% select(starts_with("a")) 
flights %>% select(ends_with("delay")) 

flights %>% arrange(desc(arr_delay)) %>% top_n(5,wt = arr_delay)

flights %>% inner_join(airports, by = c("dest" = "faa")) %>% 
  select(name,arr_delay) %>% 
  arrange(desc(arr_delay)) %>% 
  top_n(5,wt = arr_delay)


glimpse(airports)
glimpse(flights)

table(flights$flight)

# Using the datasets included in the nycflights13 package, compute 
# the available seat miles for each airline sorted in descending order. 
# After completing all the necessary data wrangling steps, the resulting 
# data frame should have 16 rows (one for each airline) and 2 columns
# (airline name and available seat miles).

airlines %>% inner_join(flights,by = "carrier") %>%
  select(name,tailnum,distance) %>% 
  inner_join(planes, by = "tailnum") %>%
  select(name,tailnum,distance,seats) %>%
  mutate(dist_per_seats = distance * seats) %>%
  group_by(name) %>%
  summarise(suma = sum(dist_per_seats))
