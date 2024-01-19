getwd()

install.packages("dplyr")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("MASS")
install.packages("plyr")

library(dplyr)
library(tidyverse)
library(gridExtra)
library(ggplot)
library(MASS)
library(plyr)

# Loading 2019 data
base_19Q1 <- read.csv("./Divvy_Trips_2019_Q1/Divvy_Trips_2019_Q1.csv")
base_19Q2 <- read.csv("./Divvy_trips_2019_Q2/Divvy_Trips_2019_Q2.csv")
base_19Q3 <- read.csv("./Divvy_trips_2019_Q3/Divvy_Trips_2019_Q3.csv")
base_19Q4 <- read.csv("./Divvy_trips_2019_Q4/Divvy_Trips_2019_Q4.csv")

# Checking 2019 data
head(base_19Q1)
head(base_19Q2)
head(base_19Q3)
head(base_19Q4)

tail(base_19Q1)
tail(base_19Q2)
tail(base_19Q3)
tail(base_19Q4)

# Loading 2020 Q1 data
base_20Q1 <- read.csv("./Divvy_trips_2020_Q1/Divvy_Trips_2020_Q1.csv")

# Checking 2020 Q1 data
colnames(base_20Q1)

# Loading another package
library(data.table)

# Loading 2020 Q2 data
files <- list.files(path = "./Divvy_Trips_2020_Q2", pattern = ".csv")
setwd('./Divvy_Trips_2020_Q2')
temp <- lapply(files, read.csv)
base_20Q2 <- rbindlist( temp )

# Loading 2020 Q3 data
setwd('..')
files <- list.files(path = "./Divvy_Trips_2020_Q3", pattern = ".csv")
setwd('./Divvy_Trips_2020_Q3')
temp <- lapply(files, read.csv)
base_20Q3 <- rbindlist( temp )

# Loading 2020 Q4 data
setwd('..')
files <- list.files(path = "./Divvy_Trips_2020_Q4", pattern = ".csv")
setwd('./Divvy_Trips_2020_Q4')
temp <- lapply(files, read.csv)
base_20Q4 <- rbindlist( temp )

# Setting path back to default
setwd('..')
getwd()

# Checking 2020 data
head(base_20Q1)
head(base_20Q2)
head(base_20Q3)
head(base_20Q4)

# Checking 2020 data
tail(base_20Q1)
tail(base_20Q2)
tail(base_20Q3)
tail(base_20Q4)

# Get column names for all data
colnames(base_19Q1)
colnames(base_19Q1)
colnames(base_19Q3)
colnames(base_19Q4)
colnames(base_20Q1)
colnames(base_20Q2)
colnames(base_20Q3)
colnames(base_20Q4)

# Making sure there are no misspelled user type

usertype_19Q1 <- base_19Q1 %>%
    select(usertype) %>%
    distinct()

usertype_19Q2 <- base_19Q2 %>%
    select(User.Type) %>%
    distinct()

usertype_19Q3 <- base_19Q3 %>%
    select(usertype) %>%
    distinct()

usertype_19Q4 <- base_19Q4 %>%
    select(usertype) %>%
    distinct()

usertype_20Q1 <- base_20Q1 %>%
    select(member_casual) %>%
    distinct()

usertype_20Q2 <- base_20Q2 %>%
    select(member_casual) %>%
    distinct()

usertype_20Q3 <- base_20Q3 %>%
    select(member_casual) %>%
    distinct()

usertype_20Q4 <- base_20Q4 %>%
    select(member_casual) %>%
    distinct()

head(usertype_19Q1)
head(usertype_19Q2)
head(usertype_19Q3)
head(usertype_19Q4)
head(usertype_20Q1)
head(usertype_20Q2)
head(usertype_20Q3)
head(usertype_20Q4)

# Making sure there are no misspelled ride types
rideable_type_20Q1 <- base_20Q1 %>%
    select(rideable_type) %>%
    distinct()

rideable_type_20Q2 <- base_20Q2 %>%
    select(rideable_type) %>%
    distinct()

rideable_type_20Q3 <- base_20Q3 %>%
    select(rideable_type) %>%
    distinct()

rideable_type_20Q4 <- base_20Q4 %>%
    select(rideable_type) %>%
    distinct()

head(rideable_type_20Q1)
head(rideable_type_20Q2)
head(rideable_type_20Q3)
head(rideable_type_20Q4)

# Loading all data into working variables with standardized format, removing duplicates, and removing null data

data_19Q1 <- base_19Q1 %>% 
    mutate(tripduration = as.numeric(tripduration)) %>%
    select(trip_id, 
           start_time, 
           end_time, 
           tripduration, 
           usertype
          ) %>% 
    distinct() %>% 
    na.omit()

data_19Q2 <- base_19Q2 %>% 
    mutate(tripduration = as.numeric(X01...Rental.Details.Duration.In.Seconds.Uncapped)) %>%
    select(trip_id = X01...Rental.Details.Rental.ID, 
           start_time = X01...Rental.Details.Local.Start.Time, 
           end_time = X01...Rental.Details.Local.End.Time, 
           tripduration, 
           usertype = User.Type
          ) %>% 
    distinct() %>% 
    na.omit()

data_19Q3 <- base_19Q3 %>% 
    mutate(tripduration = as.numeric(tripduration)) %>%
    select(trip_id, 
           start_time, 
           end_time, 
           tripduration, 
           usertype
          ) %>% 
    distinct() %>% 
    na.omit()

data_19Q4 <- base_19Q4 %>% 
    mutate(tripduration = as.numeric(tripduration)) %>%
    select(trip_id, 
           start_time, 
           end_time, 
           tripduration, 
           usertype
          ) %>% 
    distinct() %>% 
    na.omit()

data_20Q1 <- base_20Q1 %>% 
    mutate(tripduration = as.numeric(difftime(ended_at, started_at, units="secs"))) %>%
    select(trip_id = ride_id, 
           start_time = started_at, 
           end_time = ended_at, 
           tripduration, 
           usertype = member_casual
          ) %>% 
    distinct() %>% 
    na.omit()

data_20Q2 <- base_20Q2 %>% 
    mutate(tripduration = as.numeric(difftime(ended_at, started_at, units="secs"))) %>%
    select(trip_id = ride_id, 
           start_time = started_at, 
           end_time = ended_at, 
           tripduration, 
           usertype = member_casual
          ) %>% 
    distinct() %>% 
    na.omit()

data_20Q3 <- base_20Q3 %>% 
    mutate(tripduration = as.numeric(difftime(ended_at, started_at, units="secs"))) %>%
    select(trip_id = ride_id, 
           start_time = started_at, 
           end_time = ended_at, 
           tripduration, 
           usertype = member_casual
          ) %>% 
    distinct() %>% 
    na.omit()

data_20Q4 <- base_20Q4 %>% 
    mutate(tripduration = as.numeric(difftime(ended_at, started_at, units="secs"))) %>%
    select(trip_id = ride_id, 
           start_time = started_at, 
           end_time = ended_at, 
           tripduration, 
           usertype = member_casual
          ) %>% 
    distinct() %>% 
    na.omit()

# Checking data
head(data_20Q1)
head(data_20Q2)
head(data_20Q3)
head(data_20Q4)

# Checking column names
colnames(data_19Q1)
colnames(data_20Q1)

# Checking whether main backup were affected
colnames(base_20Q1)

# Checking for outliers in trip duration

data_19Q1 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q1 Trip Duration Distribution", y = "Trip Duration (sec)")

data_19Q2 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q2 Trip Duration Distribution", y = "Trip Duration (sec)")

data_19Q3 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q3 Trip Duration Distribution", y = "Trip Duration (sec)")

data_19Q4 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q4 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q1 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q1 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q2 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q2 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q3 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q3 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q4 %>% ggplot(aes(y = tripduration)) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q4 Trip Duration Distribution", y = "Trip Duration (sec)")

# Removing outliers

# Finding quartiles
quant_19Q1 <- data_19Q1 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

quant_19Q2 <- data_19Q2 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

quant_19Q3 <- data_19Q3 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

quant_19Q4 <- data_19Q4 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

quant_20Q1 <- data_20Q1 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

quant_20Q2 <- data_20Q2 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

quant_20Q3 <- data_20Q3 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

quant_20Q4 <- data_20Q4 %>%
    select(tripduration) %>%
    quantile(probs = c(0.25, 0.75), na.rm = TRUE)

# Filterning out outliers
data_19Q1 <- data_19Q1 %>%
    filter(tripduration >= quant_19Q1[1] - 1.5 * (quant_19Q1[2] - quant_19Q1[1])) %>%
    filter(tripduration <= quant_19Q1[2] + 1.5 * (quant_19Q1[2] - quant_19Q1[1])) %>%
    filter(tripduration > 0)

data_19Q2 <- data_19Q2 %>%
    filter(tripduration >= quant_19Q2[1] - 1.5 * (quant_19Q2[2] - quant_19Q2[1])) %>%
    filter(tripduration <= quant_19Q2[2] + 1.5 * (quant_19Q2[2] - quant_19Q2[1])) %>%
    filter(tripduration > 0)

data_19Q3 <- data_19Q3 %>%
    filter(tripduration >= quant_19Q3[1] - 1.5 * (quant_19Q3[2] - quant_19Q3[1])) %>%
    filter(tripduration <= quant_19Q3[2] + 1.5 * (quant_19Q3[2] - quant_19Q3[1])) %>%
    filter(tripduration > 0)

data_19Q4 <- data_19Q4 %>%
    filter(tripduration >= quant_19Q4[1] - 1.5 * (quant_19Q4[2] - quant_19Q4[1])) %>%
    filter(tripduration <= quant_19Q4[2] + 1.5 * (quant_19Q4[2] - quant_19Q4[1])) %>%
    filter(tripduration > 0)

data_20Q1 <- data_20Q1 %>%
    filter(tripduration >= quant_20Q1[1] - 1.5 * (quant_20Q1[2] - quant_20Q1[1])) %>%
    filter(tripduration <= quant_20Q1[2] + 1.5 * (quant_20Q1[2] - quant_20Q1[1])) %>%
    filter(tripduration > 0)

data_20Q2 <- data_20Q2 %>%
    filter(tripduration >= quant_20Q2[1] - 1.5 * (quant_20Q2[2] - quant_20Q2[1])) %>%
    filter(tripduration <= quant_20Q2[2] + 1.5 * (quant_20Q2[2] - quant_20Q2[1])) %>%
    filter(tripduration > 0)

data_20Q3 <- data_20Q3 %>%
    filter(tripduration >= quant_20Q3[1] - 1.5 * (quant_20Q3[2] - quant_20Q3[1])) %>%
    filter(tripduration <= quant_20Q3[2] + 1.5 * (quant_20Q3[2] - quant_20Q3[1])) %>%
    filter(tripduration > 0)

data_20Q4 <- data_20Q4 %>%
    filter(tripduration >= quant_20Q4[1] - 1.5 * (quant_20Q4[2] - quant_20Q4[1])) %>%
    filter(tripduration <= quant_20Q4[2] + 1.5 * (quant_20Q4[2] - quant_20Q4[1])) %>%
    filter(tripduration > 0)

# Checking result of outlier removal
data_19Q1 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q1 Trip Duration Distribution", y = "Trip Duration (sec)")

data_19Q2 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q2 Trip Duration Distribution", y = "Trip Duration (sec)")

data_19Q3 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q3 Trip Duration Distribution", y = "Trip Duration (sec)")

data_19Q4 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2019 Q4 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q1 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q1 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q2 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q2 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q3 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q3 Trip Duration Distribution", y = "Trip Duration (sec)")

data_20Q4 %>% ggplot(aes(y = tripduration )) +
    geom_boxplot() +
    scale_x_discrete() +
    labs(title = "2020 Q4 Trip Duration Distribution", y = "Trip Duration (sec)")

# Standarize user-type terminology and saving copies of working data

# standarize user-type terminology
data_20Q1 <- within(data_20Q1, usertype[usertype == 'casual'] <- 'Customer')
data_20Q1 <- within(data_20Q1, usertype[usertype == 'member'] <- 'Subscriber')

data_20Q2 <- within(data_20Q2, usertype[usertype == 'casual'] <- 'Customer')
data_20Q2 <- within(data_20Q2, usertype[usertype == 'member'] <- 'Subscriber')

data_20Q3 <- within(data_20Q3, usertype[usertype == 'casual'] <- 'Customer')
data_20Q3 <- within(data_20Q3, usertype[usertype == 'member'] <- 'Subscriber')

data_20Q4 <- within(data_20Q4, usertype[usertype == 'casual'] <- 'Customer')
data_20Q4 <- within(data_20Q4, usertype[usertype == 'member'] <- 'Subscriber')

# Saving data
write.csv(data_19Q1, ".\\data_19Q1.csv", row.names=TRUE)
write.csv(data_19Q2, ".\\data_19Q2.csv", row.names=TRUE)
write.csv(data_19Q3, ".\\data_19Q3.csv", row.names=TRUE)
write.csv(data_19Q4, ".\\data_19Q4.csv", row.names=TRUE)
write.csv(data_20Q1, ".\\data_20Q1.csv", row.names=TRUE)
write.csv(data_20Q2, ".\\data_20Q2.csv", row.names=TRUE)
write.csv(data_20Q3, ".\\data_20Q3.csv", row.names=TRUE)
write.csv(data_20Q4, ".\\data_20Q4.csv", row.names=TRUE)

# Saving mergered version of the data
data_19n20 <- do.call("rbind", list(data_19Q1, data_19Q2, data_19Q3, data_19Q4, data_20Q1, data_20Q2, data_20Q3, data_20Q4))
write.csv(data_19n20, ".\\data_19n20.csv", row.names=TRUE)

# Checking data for issues
tail(data_19n20)
tail(data_20Q1)
tail(data_20Q2)
tail(data_20Q3)
tail(data_20Q4)

# Plotting ride count between Customer and Subscriber, then saving image of plot

ggplot(data = data_19Q1) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2019 Q1 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

ggplot(data = data_19Q2) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2019 Q2 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

ggplot(data = data_19Q3) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2019 Q3 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

ggplot(data = data_19Q4) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2019 Q4 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

ggplot(data = data_20Q1) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2020 Q1 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

ggplot(data = data_20Q2) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2020 Q2 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

ggplot(data = data_20Q3) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2020 Q3 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

ggplot(data = data_20Q4) + 
    geom_bar(mapping = aes(x = usertype)) +
    labs(title = "2020 Q4 User Type Trip Comparison",
         x = "User Type",
         y = "Number of Rides")

# Plotting average trip duration, then saving plot images

ggplot(data = data_19Q1) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2019 Q1 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_19Q1.jpeg")

ggplot(data = data_19Q2) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2019 Q2 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_19Q2.jpeg")

ggplot(data = data_19Q3) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2019 Q3 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_19Q3.jpeg")

ggplot(data = data_19Q4) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2019 Q1 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_19Q4.jpeg")

ggplot(data = data_20Q1) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2020 Q1 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_20Q1.jpeg")

ggplot(data = data_20Q2) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2020 Q2 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_20Q2.jpeg")

ggplot(data = data_20Q3) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2020 Q3 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_20Q3.jpeg")

ggplot(data = data_20Q4) +
    geom_bar(mapping = aes(x = usertype, y = tripduration), 
             stat = 'summary',
             fun.y = 'mean') +
    labs(title = "2020 Q4 Average Trip Duration",
         x = "User Type",
         y = "Trip Duration (sec)")

ggsave("avg_duration_20Q4.jpeg")

# Plot density of trip duration, then saving plot images

# Calculate mean and median of trip duration
stats_19Q1 <- data_19Q1 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

stats_19Q2 <- data_19Q2 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

stats_19Q3 <- data_19Q3 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

stats_19Q4 <- data_19Q4 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

stats_20Q1 <- data_20Q1 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

stats_20Q2 <- data_20Q2 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

stats_20Q3 <- data_20Q3 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

stats_20Q4 <- data_20Q4 %>% group_by(usertype) %>% 
  summarize(Avg = mean(tripduration), Median = median(tripduration)) %>% 
  pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")


# Plot and save trip duration distribution and their mean and median
ggplot(data_19Q1, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_19Q1, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2019 Q1 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_19Q1.jpeg")

ggplot(data_19Q2, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_19Q2, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2019 Q2 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_19Q2.jpeg")

ggplot(data_19Q3, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_19Q3, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2019 Q3 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_19Q3.jpeg")

ggplot(data_19Q4, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_19Q4, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2019 Q4 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_19Q4.jpeg")

ggplot(data_20Q1, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_20Q1, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2020 Q1 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_20Q1.jpeg")

ggplot(data_20Q2, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_20Q2, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2020 Q2 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_20Q2.jpeg")

ggplot(data_20Q3, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_20Q3, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2020 Q3 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_20Q3.jpeg")

ggplot(data_20Q4, aes(x = tripduration)) + 
    geom_density() +
    facet_wrap(~usertype) +
    geom_vline(data = stats_20Q4, mapping = aes(xintercept = Value, color = Stat)) + 
    labs(title="2020 Q4 Trip Duration Distribution",
            x="Trip Duration")

ggsave("dur_dis_20Q4.jpeg")

# Plot distribution of time of day a ride occures

# Extract time of day data
data_19Q1$Time <- hour(data_19Q1$start_time)
data_19Q2$Time <- hour(data_19Q2$start_time)
data_19Q3$Time <- hour(data_19Q3$start_time)
data_19Q4$Time <- hour(data_19Q4$start_time)
data_20Q1$Time <- hour(data_20Q1$start_time)
data_20Q2$Time <- hour(data_20Q2$start_time)
data_20Q3$Time <- hour(data_20Q3$start_time)
data_20Q4$Time <- hour(data_20Q4$start_time)

# Plot and save time of day distribution
ggplot(data = data_19Q1, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2019 Q1 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_19Q1.jpeg")

ggplot(data = data_19Q2, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2019 Q2 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_19Q2.jpeg")

ggplot(data = data_19Q3, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2019 Q3 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_19Q3.jpeg")

ggplot(data = data_19Q4, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2019 Q4 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_19Q4.jpeg")

ggplot(data = data_20Q1, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2020 Q1 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_20Q1.jpeg")

ggplot(data = data_20Q2, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2020 Q2 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_20Q2.jpeg")

ggplot(data = data_20Q3, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2020 Q3 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_20Q3.jpeg")

ggplot(data = data_20Q4, aes(x = Time)) + 
    geom_density() +
    facet_wrap(~usertype) +
    labs(title="2020 Q4 Ride Time Distribution",
            x="Time of Day")

ggsave("time_dis_20Q4.jpeg")


