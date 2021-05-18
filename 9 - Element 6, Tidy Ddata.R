# Element 7: Tidyverse -- tidyr ----

# Load packages ----
# This should already be loaded if you executed the commands in the previous file.
library(tidyverse)

# Get a play data set:
PlayData <- read_tsv("data/PlayData.txt")

# Let's see the scenarios we discussed in class:
# Scenario 1: Transformation height & width
PlayData$height/PlayData$width

# For the other scenarios, tidy data would be a 
# better starting point:
# we'll use gather()
# 4 arguments
# 1 - data
# 2,3 - key,value pair - i.e. name for OUTPUT
# 4 - the ID or the MEASURE variables

# using ID variables ("exclude" using -)
pivot_longer(PlayData, 
             names_to = "key",
             values_to = "value",
             -c("type", "time"))

# using MEASURE variables
PlayData_t <- pivot_longer(PlayData, 
                           names_to = "key", 
                           values_to = "value", 
                           c("height", "width"))

# Scenario 2: Transformation across time 1 & 2
# difference from time 1 to time 2 for each type and key
# we only want one value as output

# trans func =  mutate()
# aggr func =   summarise()
PlayData_t %>% 
  group_by(type, key) %>% 
  summarise(time_diff = value[time == 2] - value[time == 1])

# group_split()
  
# standardize to time 1
new_data <- PlayData_t %>% 
  group_by(type, key) %>% 
  mutate(value_std = value / value[time == 1])

# Scenario 3: Transformation across type A & B
# A/B for each time and key
PlayData_t %>% 
  group_by(key, time) %>% 
  summarise(type_diff = value[type == "A"] / value[type == "B"])
  # group_split()





