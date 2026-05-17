# ==============================================================================
# PROJECT: COVID-19 Vaccination Rates and Death Rates (Refined)
# DATASET: owid-covid-data.csv (Filtered for United States)
# ==============================================================================

# 0. SETUP AND LIBRARIES
# ------------------------------------------------------------------------------
# Ensure packages are installed: install.packages(c("dplyr", "ggplot2", "corrplot", "modeest"))
library(dplyr)
library(ggplot2)
library(corrplot)
library(modeest) # For mode calculation

# Load Data
raw_data <- read.csv("owid-covid-data.csv")

# PREPROCESSING: Filter for USA and Select Variables
# We create a 'Year' column to satisfy the "Grouped Boxplot" requirement.
project_data <- raw_data %>%
  filter(location == "United States") %>%
  select(date, new_deaths_smoothed_per_million, people_fully_vaccinated_per_hundred) %>%
  rename(Date = date, 
         DeathRate = new_deaths_smoothed_per_million, 
         VaccinationRate = people_fully_vaccinated_per_hundred) %>%
  na.omit() %>%
  mutate(Date = as.Date(Date),
         Year = as.factor(format(Date, "%Y"))) # Create categorical variable

head(project_data)

# ==============================================================================
# PART I: Exploratory Data Analysis - Central Tendency
# (Mean, Median, Mode)
# ==============================================================================
cat("\n--- PART I: MEASURES OF CENTRAL TENDENCY ---\n")

# Vaccination Rate
mean_vac <- mean(project_data$VaccinationRate)
median_vac <- median(project_data$VaccinationRate)
mode_vac <- mlv(project_data$VaccinationRate, method = "mfv")[1] # Most frequent value

cat(sprintf("Vaccination Rate -> Mean: %.2f, Median: %.2f, Mode: %.2f\n", mean_vac, median_vac, mode_vac))

# Death Rate
mean_death <- mean(project_data$DeathRate)
median_death <- median(project_data$DeathRate)
mode_death <- mlv(round(project_data$DeathRate, 1), method = "mfv")[1] # Rounded mode

cat(sprintf("Death Rate      -> Mean: %.2f, Median: %.2f, Mode: %.2f\n", mean_death, median_death, mode_death))

# Visualization: Histogram (Distribution)
ggplot(project_data, aes(x=DeathRate)) +
  geom_histogram(binwidth=1, fill="salmon", color="black", alpha=0.7) +
  labs(title="Distribution of COVID-19 Death Rates (USA)", x="Deaths per Million", y="Frequency") +
  theme_minimal()

# ==============================================================================
# PART II: Exploratory Data Analysis - Measures of Spread
# (Range, IQR, Variance)
# ==============================================================================
cat("\n--- PART II: MEASURES OF SPREAD ---\n")


# 1. Vaccination Rate Spread
range_vac <- range(project_data$VaccinationRate)
iqr_vac <- IQR(project_data$VaccinationRate)
var_vac <- var(project_data$VaccinationRate)

cat(sprintf("Vaccination Rate -> Range: [%.2f - %.2f], IQR: %.2f, Variance: %.2f\n", 
            range_vac[1], range_vac[2], iqr_vac, var_vac))

# 2. Death Rate Spread
range_death <- range(project_data$DeathRate)
iqr_death <- IQR(project_data$DeathRate)
var_death <- var(project_data$DeathRate)

cat(sprintf("Death Rate      -> Range: [%.2f - %.2f], IQR: %.2f, Variance: %.2f\n", 
            range_death[1], range_death[2], iqr_death, var_death))

# --- Outlier Detection ---
# Identify specific outlier dates to support the report's conclusion
outlier_threshold <- quantile(project_data$DeathRate, 0.75) + 1.5 * IQR(project_data$DeathRate)
outliers <- project_data %>% 
  filter(DeathRate > outlier_threshold) %>%
  arrange(desc(DeathRate)) %>%
  select(Date, DeathRate)

cat("\n--- OUTLIER ANALYSIS ---\n")
cat("Upper Outlier Threshold (Q3 + 1.5*IQR):", round(outlier_threshold, 2), "\n")
cat("Top 5 Extreme Outlier Dates:\n")
print(head(outliers, 5))
# This proves the "January 2021" statement in the text

# Visualization: Grouped Boxplot (Comparing Spread by Year)
ggplot(project_data, aes(x=Year, y=DeathRate, fill=Year)) +
  geom_boxplot() +
  labs(title="Comparison of Death Rate Spread by Year", x="Year", y="Deaths per Million") +
  theme_minimal()
# ==============================================================================
# PART III: Correlation Analysis
# (Correlation Coefficient, Heatmap)
# ==============================================================================
cat("\n--- PART III: CORRELATION ANALYSIS ---\n")

# Calculate Pearson Correlation
correlation <- cor(project_data$VaccinationRate, project_data$DeathRate)
cat("Correlation Coefficient (r):", correlation, "\n")

# Visualization: Correlation Matrix Heatmap
# We select only numeric columns for the heatmap
numeric_data <- project_data %>% select(DeathRate, VaccinationRate)
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method="color", type="upper", addCoef.col = "black", tl.col="black", title="Correlation Heatmap")

# ==============================================================================
# PART IV: Simple Linear Regression
# (Model, Prediction)
# ==============================================================================
cat("\n--- PART IV: LINEAR REGRESSION ---\n")

# Fit the model: DeathRate depends on VaccinationRate
model <- lm(DeathRate ~ VaccinationRate, data=project_data)
summary(model)

# Extract Coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]
cat(sprintf("Regression Equation: y = %.3fx + %.3f\n", slope, intercept))

# Prediction: Predict Death Rate if Vaccination Rate is 60%
new_data <- data.frame(VaccinationRate = 60)
prediction <- predict(model, newdata = new_data)
cat("Predicted Death Rate at 60% Vaccination:", prediction, "\n")

# Visualization: Scatterplot with Regression Line
ggplot(project_data, aes(x=VaccinationRate, y=DeathRate)) +
  geom_point(alpha=0.4, color="blue") +
  geom_smooth(method="lm", color="red", se=TRUE) +
  labs(title="Linear Regression: Vaccination vs Death Rate", x="Vaccination Rate (%)", y="Death Rate") +
  theme_minimal()

# ==============================================================================
# PART V: Statistical Inference (Confidence Intervals)
# (Population Mean Estimation)
# ==============================================================================
cat("\n--- PART V: CONFIDENCE INTERVALS ---\n")

# Calculate 95% Confidence Interval for the Mean Death Rate
ci_test <- t.test(project_data$DeathRate, conf.level = 0.95)
print(ci_test)

# Interpretation Helper
cat(sprintf("We are 95%% confident that the true population mean of daily death rates falls between %.3f and %.3f.\n", 
            ci_test$conf.int[1], ci_test$conf.int[2]))

# End of Code
