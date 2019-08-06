# Programmer: Bilsay Varcin
# Date: 08/04/2019

#### Upload Packages ####
library(tidyverse) 
library(ggmap) #
library(viridis) # nice color scale
library(svDialogs) # taking inputs through pop up window in R

#### Data Source #### 
# Data can be downloaded from this link
# http://www.andresmh.com/nyctaxitrips/

taxi <- read_csv("datasets/taxi.csv")
glimpse(taxi)



#### Data Manipulation ####
# Keep only trips where either fare or tip is greater than 0

taxi <- taxi %>%
  rename(lat = pickup_latitude, long = pickup_longitude) %>%
  filter( fare_amount > 0 | tip_amount > 0) %>%
  mutate(total = log(fare_amount + tip_amount))

# Focus only the fares started in Manhattan
taxi <- taxi  %>% 
  filter(between(lat, 40.70, 40.83) & between(long, -74.025, -73.93)) 



# Retrieving a stored map object which originally was created by

register_google(key = dlgInput("Enter Google Static MAps API Key", "Example: mQkzTpiaLYjPqXQBotesgif3EfGL2dbrNVOrogg")$res[[1]]) 
manhattan <- get_map("manhattan", zoom = 12, color = "bw")

# Drawing a density map with the number of journey start locations
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data = taxi, aes(x = long, y = lat), bins = 60, alpha = 0.6) +
  labs(x = "Lat", y = "Long", fill = "Number of Journeys")


# Predict taxi fares using a tree
# Loading in the tree package
library(tree)

# Fitting a tree to lat and long
fitted_tree <- tree(total ~ lat + long, taxi)

# Draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)



# Add more predictors 

# Loading in the lubridate package
library(lubridate)

# Generate the three new time variables
taxi <- taxi %>% 
  mutate(hour = hour(pickup_datetime),
         wday = wday(pickup_datetime, label = TRUE),
         month = month(pickup_datetime, label = TRUE))



# Fitting a tree with total as the outcome and 
# lat, long, hour, wday, and month as predictors
fitted_tree <- tree(total ~ lat + long + hour + wday + month, taxi)

# draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)

# Summarizing the performance of the tree
summary(fitted_tree)


# Loading in the randomForest package
library(randomForest)

# Fitting a random forest
fitted_forest <- randomForest(total ~ lat + long + hour + wday + month, taxi, ntree = 80, sampsize = 10000)

# Printing the fitted_forest object
fitted_forest


# In statistical terms, both models explain only about 3% of the variance.

# Extracting the prediction from fitted_forest
taxi$pred_total <- fitted_forest$predicted

# Plotting the predicted mean trip prices from according to the random forest
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data = taxi, aes(x = long, y = lat, z = pred_total), bins = 60, alpha = 0.6, fun = mean) +
  labs(x = "Lat", y = "Long", fill = "Predicted Mean Trip Prices")


# Plot the actual fare
# Function that returns the mean *if* there are 15 or more datapoints
mean_if_enough_data <- function(x) { 
  ifelse( length(x) >= 15, mean(x), NA) 
}

# Plotting the mean trip prices from the data
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data = taxi, aes(x = long, y = lat, z = total), bins = 60, alpha = 0.6, fun = mean_if_enough_data) +
  labs(x = "Lat", y = "Long", fill = "Actual Fare")
