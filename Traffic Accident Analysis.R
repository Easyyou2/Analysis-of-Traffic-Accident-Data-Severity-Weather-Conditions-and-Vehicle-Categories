# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the dataset: replace contents inside the brackets with your file location
df <- read.csv("/Users/easyyou2/Downloads/traffic_dataset_en.csv")

# Inspect the first few rows and column names
print(head(df, 10))
print(colnames(df))

# Data Cleaning
# Convert relevant columns to appropriate data types and filter out unknown values
#this was done based off the given legend from the dataset
df_clean <- df %>%
  mutate(C_YEAR = as.integer(C_YEAR),
         C_MNTH = as.integer(C_MNTH),
         C_WDAY = as.integer(C_WDAY),
         C_HOUR = as.integer(C_HOUR),
         C_SEV = recode_factor(C_SEV,
                               `1` = "Collision producing at least one fatality",
                               `2` = "Collision producing non-fatal injury",
                               `U` = "Unknown",
                               `X` = "Jurisdiction does not provide this data element"),
         C_VEHS = as.integer(C_VEHS),
         C_CONF = as.factor(C_CONF),
         C_WTHR = recode_factor(C_WTHR,
                                `1` = "Clear and sunny",
                                `2` = "Overcast, cloudy but no precipitation",
                                `3` = "Raining",
                                `4` = "Snowing, not including drifting snow",
                                `5` = "Freezing rain, sleet, hail",
                                `6` = "Visibility limitation e.g. drifting snow, fog, smog, dust, smoke, mist",
                                `7` = "Strong wind",
                                `Q` = "Choice is other than the preceding values",
                                `U` = "Unknown",
                                `X` = "Jurisdiction does not provide this data element"),
         C_RSUR = as.factor(C_RSUR),
         V_TYPE = as.factor(V_TYPE),
         P_SEX = as.factor(P_SEX),
         P_AGE = as.integer(P_AGE),
         P_ISEV = recode_factor(P_ISEV,
                                `1` = "No Injury",
                                `2` = "Injury",
                                `3` = "Fatality",
                                `U` = "Unknown",
                                `X` = "Jurisdiction does not provide this data element")) %>%
  filter(!C_SEV %in% c('UU', 'N'), 
         !C_WTHR %in% c('UU', 'N'), 
         !V_TYPE %in% c('UU', 'N'), 
         !P_SEX %in% c('U'), 
         !P_ISEV %in% c('U', 'N'), 
         !P_AGE %in% c('UU', 'NN'),
         !P_ISEV %in% c('N')) %>%
  mutate(C_MNTH = factor(C_MNTH, levels = 1:12, labels = month.abb),
         Vehicle_Category = case_when(
           V_TYPE %in% c('1', '2') ~ 'Sedan',
           V_TYPE %in% c('3') ~ 'SUV',
           V_TYPE %in% c('4') ~ 'Minivan',
           V_TYPE %in% c('5') ~ 'Truck',
           V_TYPE %in% c('6', '7') ~ 'Motorcycle',
           V_TYPE %in% c('8', '9', '10') ~ 'Bus',
           TRUE ~ 'Other'
         ))

# Visualization 1: Accident Severity Distribution
severity_distribution <- df_clean %>%
  group_by(C_SEV) %>%
  summarise(Count = n())

ggplot(severity_distribution, aes(x = C_SEV, y = Count, fill = C_SEV)) +
  geom_bar(stat = "identity") +
  labs(title = "Accident Severity Distribution", x = "Collision Severity", y = "Count", fill = "Collision Severity") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set1")

# Visualization 2: Injuries by Vehicle Category
injuries_by_vehicle <- df_clean %>%
  group_by(Vehicle_Category, P_ISEV) %>%
  summarise(Count = n())

ggplot(injuries_by_vehicle, aes(x = Vehicle_Category, y = Count, fill = P_ISEV)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Injuries by Vehicle Category", x = "Vehicle Category", y = "Count", fill = "Injury Severity") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization 3: Weather Conditions and Accidents
weather_conditions <- df_clean %>%
  group_by(C_WTHR, C_SEV) %>%
  summarise(Count = n())

ggplot(weather_conditions, aes(x = C_WTHR, y = Count, fill = C_SEV)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Accidents by Weather Conditions", x = "Weather Condition", y = "Count", fill = "Collision Severity") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prepare data for Accidents by Month with Fatal Accident Highlight
accidents_by_month <- df_clean %>%
  filter(C_SEV != "Collision producing at least one fatality") %>%
  group_by(C_YEAR, C_MNTH) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(C_MNTH) %>%
  summarise(Count = sum(Count)) %>%
  filter(!is.na(C_MNTH))

fatal_accidents <- df_clean %>%
  filter(C_SEV == "Collision producing at least one fatality") %>%
  group_by(C_YEAR, C_MNTH) %>%
  summarise(Fatal_Count = n()) %>%
  ungroup() %>%
  group_by(C_MNTH) %>%
  summarise(Fatal_Count = sum(Fatal_Count)) %>%
  filter(!is.na(C_MNTH))

# Visualization 4: Accidents by Month with Fatal Accident Highlight
ggplot() +
  geom_line(data = accidents_by_month, aes(x = C_MNTH, y = Count, color = "Non-fatal Accidents"), linewidth = 1, group = 1) +
  geom_point(data = accidents_by_month, aes(x = C_MNTH, y = Count, color = "Non-fatal Accidents"), size = 2) +
  geom_line(data = fatal_accidents, aes(x = C_MNTH, y = Fatal_Count * 5, color = "Fatal Accidents"), linewidth = 1, group = 1) +
  geom_point(data = fatal_accidents, aes(x = C_MNTH, y = Fatal_Count * 5, color = "Fatal Accidents"), size = 2) +
  labs(title = "Accidents by Month with Fatal Accident Highlight", x = "Month", y = "Count", color = "Accident Type") +
  theme_minimal() +
  scale_color_manual(values = c("Non-fatal Accidents" = "blue", "Fatal Accidents" = "red"))
