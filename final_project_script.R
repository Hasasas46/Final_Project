
# Final Project R Script: Business Revenue & Employment Analysis

# Load required packages
install.packages("tidyverse")
install.packages("readxl")
library(tidyverse)
library(readxl)

setwd("C:/Users/halid/OneDrive/Belgeler/final project")

# Load the data
df_raw <- read_excel("~/final project/expoAbove2million.xlsx")

# Clean revenue
df_clean <- df_raw %>%
  mutate(
    rev1 = str_remove_all(Revenue, "[$,]") %>% str_trim(),
    Revenue = case_when(
      str_detect(rev1, regex("Million$", ignore_case = TRUE)) ~
        as.numeric(str_remove(rev1, regex("\s*Million$", ignore_case = TRUE))) * 1e6,
      str_detect(rev1, regex("Billion$", ignore_case = TRUE)) ~
        as.numeric(str_remove(rev1, regex("\s*Billion$", ignore_case = TRUE))) * 1e9,
      TRUE ~ as.numeric(rev1)
    ),
    Employees = str_remove_all(Employees, ",") %>% as.numeric(),
    Company = `Company Name`
  ) %>%
  select(Company, State, Revenue, Employees) %>%
  drop_na()

# Histogram of Revenue
ggplot(df_clean, aes(x = Revenue)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Company Revenue", x = "Revenue (USD)", y = "Count")

# Boxplot of Revenue by State
ggplot(df_clean, aes(x = State, y = Revenue)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Revenue by State", x = "State", y = "Revenue")

# Summary statistics by State
df_clean %>%
  group_by(State) %>%
  summarise(
    Mean_Revenue = mean(Revenue),
    SD_Revenue = sd(Revenue),
    Min_Revenue = min(Revenue),
    Max_Revenue = max(Revenue),
    .groups = "drop"
  )

# T-test between two selected states (e.g., ND vs. CA)
states_to_compare <- c("ND", "CA")
df_ttest <- df_clean %>% filter(State %in% states_to_compare)
t.test(Revenue ~ State, data = df_ttest)

# Scatterplot of Employees vs Revenue
ggplot(df_clean, aes(x = Employees, y = Revenue)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Revenue vs. Number of Employees", x = "Employees", y = "Revenue")

# Linear regression model
model <- lm(Revenue ~ Employees, data = df_clean)
summary(model)

