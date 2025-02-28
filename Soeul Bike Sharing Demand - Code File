#Libraries necessary 
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages('fastDummies')
install.packages("gridExtra")
install.packages("ggcorrplot")
install.packages("PerformanceAnalytics")
install.packages("GGally")
install.packages("car")
install.packages("e1071")
install.packages("caret")
install.packages("glmnet")         
install.packages("class")           
install.packages("rpart")           
install.packages("randomForest")   
install.packages("xgboost")
install.packages("FNN")
install.packages("lmtest")
install.packages("mgcv")
install.packages("Metrics")
install.packages("WARN")

library(lubridate)
library(dplyr)
library(ggplot2)
library('fastDummies')
library(gridExtra)
library(corrplot)
library(ggcorrplot)
library(PerformanceAnalytics)
library(tidyr)
library(GGally)
library(car)
library(e1071)
library(caret)
library(glmnet)       
library(class)        
library(rpart)        
library(randomForest) 
library(FNN)
library(xgboost)
library(stats)
library(lmtest)
library(MASS)
library(mgcv) 
library(Metrics)
library(randomForest)
library(WARN)


## 1. Data Understanding and Preprocessing

##a) Data Inspection:
##Loading the dataset
seoul_bike_data = read.csv("C:/Users/ritik/Downloads/SeoulBikeData.csv", fileEncoding = "latin1")
head(seoul_bike_data, 2)
##Manually assign new column names for better accessibility of variables
new_column_names = c("Date", "RBC", "Hour", "Temp", "Humid_Percent", "Wind_Speed", "Visibility", "DPT", "Solar_Rad", "Rainfall", "Snowfall", "Seasons", "Holiday", "Functioning_Day")
colnames(seoul_bike_data) = new_column_names
colnames(seoul_bike_data)
##Exploring data types.
str(seoul_bike_data)
##Exploring Null Values.
colSums(is.na(seoul_bike_data))
##Exploring Basic statistics.
summary(seoul_bike_data)   
##Extraction of features like Day, Month, Year, and Weekday from Date. 
seoul_bike_data$Date = as.Date(seoul_bike_data$Date, format = "%d/%m/%Y")
seoul_bike_data$Day = day(seoul_bike_data$Date)
seoul_bike_data$Month = month(seoul_bike_data$Date)
seoul_bike_data$Year = year(seoul_bike_data$Date)
seoul_bike_data$Weekday = weekdays(seoul_bike_data$Date)
head(seoul_bike_data)

##b) Data Cleaning 
##Since there are no missing values in the dataset we can move on the analysis of the ouliers in the datset. 
##Checking for outliers in numerical columns. 
num_cols = seoul_bike_data %>% select_if(is.numeric)
colnames(num_cols)
##Visualising outliers
for (col_name in colnames(num_cols)) {
  boxplot(num_cols[[col_name]], 
          main = paste("Boxplot of", col_name), 
          col = "lightblue", 
          ylab = col_name)
  readline(prompt = "Press[Enter] to see the next plot...")
}


##2. Exploratory Data Analysis (EDA)
##a) Univariate Analysis
rbc_histogram = ggplot(seoul_bike_data, aes(x = RBC)) +
  geom_histogram(binwidth = 50, fill ="lightblue", color = "black", alpha = 0.7) +  # Adjust binwidth as needed
  labs(
    title = "RENTED BIKE COUNT (RBC)",
    x = "Rented Bike Count (RBC)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) + theme_minimal()

print(rbc_histogram)

skewness(seoul_bike_data$RBC)
##The skewness value of 1.153 indicates that the distribution of the RBC (Rented Bike Count) variable is significantly positively skewed. 
##We might encounter model assumption violation due to this, but lets tackle that issue in the later stages if it gives rises to model assumption violation. 


##b)Bivariate Analysis 
#Average Rented Bike Counts (RBC) for each weekday
plot_data = seoul_bike_data %>%
  group_by(Weekday, Hour) %>%
  summarise(Mean_RBC = mean(RBC, na.rm = TRUE))

ggplot(plot_data, aes(x = Hour, y = Mean_RBC, color = Weekday)) +
  geom_line(size = 1) + # Add lines for each Weekday
  labs(
    title = "Average Bike Sharing Demand on Different Days of the Week",
    x = "Hour",
    y = "Rented Bike Count",
    color = "Day Name"
  ) +
  theme_minimal()

# RBC vs Hour (Line plot)
rbc_hour_plot = seoul_bike_data %>%
  group_by(Hour) %>%
  summarise(Mean_RBC = mean(RBC, na.rm = TRUE))

ggplot(rbc_hour_plot, aes(x = Hour, y = Mean_RBC)) +
  geom_line(color = "lightblue", size = 1) +  # Line for RBC against Hour
  labs(
    title = "Average Bike Rentals by Hour of the Day",
    x = "Hour",
    y = "Average Rented Bike Count"
  ) +
  theme_minimal()

#RBC vs Holiday/No Holiday (Line plot)
rbc_holiday_plot = seoul_bike_data %>%
  group_by(Holiday, Hour) %>%
  summarise(Mean_RBC = mean(RBC, na.rm = TRUE))

ggplot(rbc_holiday_plot, aes(x = Hour, y = Mean_RBC, color = Holiday)) +
  geom_line(size = 1) +  # Line for RBC against Hour, grouped by Holiday
  labs(
    title = "Average Bike Rentals by Hour on Holiday vs No Holiday",
    x = "Hour",
    y = "Average Rented Bike Count",
    color = "Holiday Status"
  ) +
  theme_minimal()

#Bike Demand on Weekdays vs Weekend
seoul_bike_data$Day_Type = ifelse(seoul_bike_data$Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

rbc_weekend_weekday_plot = seoul_bike_data %>%
  group_by(Day_Type, Hour) %>%
  summarise(Mean_RBC = mean(RBC, na.rm = TRUE))

ggplot(rbc_weekend_weekday_plot, aes(x = Hour, y = Mean_RBC, color = Day_Type)) +
  geom_line(size = 1) +  # Line for RBC against Hour, grouped by Day Type
  labs(
    title = "Average Bike Rentals on Weekdays vs Weekends",
    x = "Hour",
    y = "Average Rented Bike Count",
    color = "Day Type"
  ) +
  theme_minimal()

#Bike Demand During Different Seasons
rbc_season_plot = seoul_bike_data %>%
  group_by(Seasons, Hour) %>%
  summarise(Mean_RBC = mean(RBC, na.rm = TRUE))

# Plot: RBC vs Hour (by Season)
ggplot(rbc_season_plot, aes(x = Hour, y = Mean_RBC, color = Seasons)) +
  geom_line(size = 1) +  # Line for RBC against Hour, grouped by Seasons
  labs(
    title = "Average Bike Rentals During Different Seasons",
    x = "Hour",
    y = "Average Rented Bike Count",
    color = "Season"
  ) +
  theme_minimal()

#Bike Demand during Different Times  of the day
seoul_bike_data <- seoul_bike_data %>%
  mutate(Time_of_Day = case_when(
    Hour >= 0 & Hour < 6 ~ "Night",
    Hour >= 6 & Hour < 12 ~ "Morning",
    Hour >= 12 & Hour < 18 ~ "Afternoon",
    Hour >= 18 & Hour <= 23 ~ "Evening"
  )) %>%
  mutate(Time_of_Day = factor(Time_of_Day, levels = c("Morning", "Afternoon", "Evening", "Night")))

# Summarize and plot data
ggplot(seoul_bike_data, aes(x = Time_of_Day, y = RBC)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(
    title = "Bikes Rented Fluctuation on Different Times of the Day",
    x = NULL,
    y = "Rented Bike Count"
  ) +
  theme_minimal()

#Distribution plot of each numerical columns (excluding dummy variables) Scatter Plot
variables_to_plot = c("Hour", "Temp", "Humid_Percent", "Wind_Speed", 
                      "Visibility", "DPT", "Solar_Rad", "Rainfall", "Snowfall")

scatter_plots = lapply(variables_to_plot, function(var) {
  ggplot(seoul_bike_data, aes_string(x = var, y = "RBC")) +
    geom_point(color = "lightblue", alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = toupper(var),
      x = var,
      y = "Rented Bike Count"
    ) +
    theme_minimal()
})

grid.arrange(
  grobs = scatter_plots,
  ncol = 3,
  top = "Relationship Between Rented Bike Count and Various Factors"
)

#Distribution plot of each numerical columns (excluding dummy variables) Histograms
histograms = lapply(variables_to_plot, function(var) {
  ggplot(seoul_bike_data, aes_string(x = var)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
    labs(
      title = toupper(var),
      x = var,
      y = "Frequency"
    ) +
    theme_minimal()
})

for (plot in histograms) {
  print(plot)
}

# Barplot for Seasons vs Rented Bike Count
ggplot(seoul_bike_data, aes(x = Seasons, y = RBC, fill = Seasons)) +
  geom_bar(stat = "summary", fun = "mean", color = "black") +
  labs(
    title = "Average Rented Bike Count by Seasons",
    x = "Seasons",
    y = "Average Rented Bike Count"
  ) +
  scale_fill_manual(values = c("Spring" = "lightblue", "Summer" = "lightpink", "Autumn" = "lightyellow", "Winter" = "lightcoral")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )


# Barplot for Month vs Rented Bike Count
ggplot(seoul_bike_data, aes(x = factor(Month), y = RBC, fill = factor(Month))) +
  geom_bar(stat = "summary", fun = "mean", color = "black") +
  labs(
    title = "Average Rented Bike Count by Month",
    x = "Month",
    y = "Average Rented Bike Count"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Automatically use a color palette
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Barplot for Weekday vs Rented Bike Count
ggplot(seoul_bike_data, aes(x = factor(Weekday), y = RBC, fill = factor(Weekday))) +
  geom_bar(stat = "summary", fun = "mean", color = "black") +
  labs(
    title = "Average Rented Bike Count by Weekday",
    x = "Weekday",
    y = "Average Rented Bike Count"
  ) +
  scale_fill_manual(values = c("Monday" = "lightskyblue", "Tuesday" = "lightgreen", "Wednesday" = "lightcoral", 
                               "Thursday" = "lightyellow", "Friday" = "lightpink", "Saturday" = "lightgray", "Sunday" = "lightblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Barplot for Hour vs Rented Bike Count
ggplot(seoul_bike_data, aes(x = factor(Hour), y = RBC, fill = factor(Hour))) +
  geom_bar(stat = "summary", fun = "mean", color = "black") +
  labs(
    title = "Average Rented Bike Count by Hour",
    x = "Hour",
    y = "Average Rented Bike Count"
  ) +
  scale_fill_viridis_d() +  # Use a color palette from the viridis package
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )


#RBC trend Analysis Over Time
#Daily Trends
daily_trends <- seoul_bike_data %>%
  group_by(Date) %>%
  summarise(Daily_RBC = sum(RBC, na.rm = TRUE))

ggplot(daily_trends, aes(x = Date, y = Daily_RBC)) +
  geom_line(color = "lightblue") +
  labs(title = "Daily Trend of Rented Bike Counts", x = "Day of Month", y = "Daily Bike Rentals") +
  theme_minimal()

#Monthly Trends
monthly_trends = seoul_bike_data %>%
  group_by(Year, Month) %>%
  summarise(Monthly_RBC = sum(RBC, na.rm = TRUE))
monthly_trends = monthly_trends %>%
  mutate(YearMonth = as.Date(paste(Year, Month, "01", sep = "-")))

ggplot(monthly_trends, aes(x = YearMonth, y = Monthly_RBC)) +
  geom_line(color = "lightblue", size = 1) +
  labs(
    title = "Monthly Trend of Rented Bike Counts",
    x = "Month",
    y = "Monthly Bike Rentals"
  ) +
  theme_minimal()

seoul_bike_data <- seoul_bike_data[, !(names(seoul_bike_data) %in% c("Day_Type", "Time_of_Day"))]
head(seoul_bike_data)

##c) Correlation Analysis for numerical Columns
seoul_bike_data$Hour = as.numeric(seoul_bike_data$Hour)
numeric_cols = seoul_bike_data[, sapply(seoul_bike_data, is.numeric)]
corr_matrix = cor(numeric_cols, use = "complete.obs")
corrplot(corr_matrix, 
         method = "circle",        # Use circle method for visualization
         type = "upper",           # Show the upper triangle of the matrix
         tl.cex = 0.8,             # Adjust text label size
         addCoef.col = "black",    # Add correlation numbers in black color
         tl.col = "black",         # Add color to the axis labels
         number.cex = 0.7,         # Adjust the size of the correlation numbers
         title = "Correlation Matrix with Numbers")

##The correlation between Temp and DPT is 0.91, which indicates a very high correlation. This suggests that both variables are likely capturing similar information, which could lead to multicollinearity if both are included in a model.
##There is also a high correlation (0.54) between Humid_Percent and DPT. These variables might be related because humidity and dew point temperature are often closely related in meteorological contexts.
##Solar_Rad and Temp have a correlation of 0.35, which is moderate, but still might raise concerns about collinearity depending on the context.
##To Address multicollinearity we will examine VIF (Variance Inflation Factor)for each variable to check the extent of multicollinearity and might might consider removing one of the highly correlated variables by conducting appropriate tests. 




##3. Feature Engineering
##a) Variance Inflation Factor (VIF)
seoul_bike_data_numeric = seoul_bike_data %>% select_if(is.numeric)  # Select only numeric columns
vif_result = vif(lm(RBC ~ ., data = seoul_bike_data_numeric))  # RBC as the dependent variable
print(vif_result)
##Just as the Correlation Matrix the VIF shows that Temp, Humid_Percent, DPT have a really large VIF score, and thus are highly correlated. 
##We might conider dropping one of this and testing how the model performs in further stpes by checking the VIF scores by dropping one of these varaibles after conducting appropriate tests. 
##First lest check if dropping DPT improves the VIF scores of the other predictors. 
seoul_bike_data_numeric_1 = dplyr::select(seoul_bike_data_numeric, -DPT)
vif_result_1 = vif(lm(RBC ~ ., data = seoul_bike_data_numeric_1))  # RBC as the dependent variable
print(vif_result_1)
##We see that by dropping DPT predictor our VIF scores for all the other predictors presnt within the dataset improves and none of them show significant signs of collinearity. 


##b) Encode categorical variables. 
table(seoul_bike_data$Seasons)
table(seoul_bike_data$Holiday)
table(seoul_bike_data$Functioning_Day)
table(seoul_bike_data$Weekday)

seoul_bike_data$Holiday = ifelse(seoul_bike_data$Holiday == "No Holiday", 0, 1)
seoul_bike_data$`Functioning_Day`= ifelse(seoul_bike_data$`Functioning_Day` == "No", 0, 1)

seoul_bike_data_season <- fastDummies::dummy_cols(seoul_bike_data, select_columns = "Seasons", remove_first_dummy = TRUE)
seoul_bike_data_weekday <- fastDummies::dummy_cols(seoul_bike_data, select_columns = "Weekday", remove_first_dummy = TRUE)
seoul_bike_data_season <- seoul_bike_data_season[, grep("Seasons", colnames(seoul_bike_data_season))]
seoul_bike_data_weekday <- seoul_bike_data_weekday[, grep("Weekday", colnames(seoul_bike_data_weekday))]
seoul_bike_data <- cbind(seoul_bike_data, seoul_bike_data_season, seoul_bike_data_weekday)
seoul_bike_data <- seoul_bike_data[, !(names(seoul_bike_data) %in% c("Seasons", "Weekday"))]
head(seoul_bike_data)
seoul_bike_data = dplyr::select(seoul_bike_data, -Date)
head(seoul_bike_data)
ncol(seoul_bike_data)


model_with_dpt = lm(RBC ~ . , data = seoul_bike_data)
model_without_dpt = lm(RBC ~ . - DPT, data = seoul_bike_data)
summary(model_with_dpt)
summary(model_without_dpt)
pred_with_dpt <- predict(model_with_dpt, newdata = seoul_bike_data)
pred_without_dpt <- predict(model_without_dpt, newdata = seoul_bike_data)
mse_with_dpt <- mse(seoul_bike_data$RBC, pred_with_dpt)
mse_without_dpt <- mse(seoul_bike_data$RBC, pred_without_dpt)
cat("MSE with DPT:", mse_with_dpt, "\n")
cat("MSE without DPT:", mse_without_dpt, "\n")
anova(model_without_dpt, model_with_dpt)

seoul_bike_data = seoul_bike_data[, !names(seoul_bike_data) %in% "DPT"]  ##Drop DPT 
head(seoul_bike_data)
ncol(seoul_bike_data)

##Fitting a preliminary linear model using the untransformed RBC
model_raw = lm(RBC ~ ., data = seoul_bike_data)
summary(model_raw)
##Plotting residuals vs. fitted values to check for model assumptions
plot(model_raw$fitted.values, residuals(model_raw),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values (Untransformed RBC)")
abline(h = 0, col = "red", lty = 2)
##Q-Q plot 
qqnorm(residuals(model_raw), main = "Q-Q Plot of Residuals")
qqline(residuals(model_raw), col = "red", lty = 2)  # Add a reference line


##The Q-Q plot of Residuals shows significant deviations from the diagonal line, especially at the tails.This suggests that the residuals are not normally distributed, particularly with heavy tails or skewness.
##The Residuals vs. Fitted Values plot shows a funnel-like shape. This indicates non equal variance of residuals across the range of fitted values.
##Similary it also violates Linearity assumption. 


##Lets check for normality and EV using the tests
bp_test_raw = bptest(model_raw)
print(bp_test_raw)

##Since Shapiro test can only be done on sample size between 3 and 5000 we will use Kolmogorov-Smirnov Test for normality assumption. 
residuals = resid(model_raw)
ks_test = ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
print(ks_test)
##From the BP test and the KS test we get the same results that the model assumptions are not satisfied by the model. 


# Calculate residuals and influence metrics
cooks_distances = cooks.distance(model_raw) # Cook's distance
standardized_residuals = rstandard(model_raw) # Standardized residuals
cooks_threshold = 4 / nrow(seoul_bike_data) # Common threshold for Cook's distance
residuals_threshold = 2 # Common threshold for standardized residuals (|Residual| > 2 is considered an outlier)
influential_points = which(cooks_distances > cooks_threshold)
outliers = which(abs(standardized_residuals) > residuals_threshold)
overlap_points = intersect(influential_points, outliers)
cat("Number of influential points:", length(influential_points), "\n")
cat("Number of outliers:", length(outliers), "\n")
cat("Number of overlapping points (outliers and influential):", length(overlap_points), "\n")
cat("Indices of influential points:", influential_points, "\n")
cat("Indices of outliers:", outliers, "\n")
cat("Indices of overlapping points:", overlap_points, "\n")


##Checking if removing these points improves the model
cleaned_data <- seoul_bike_data[-outliers, ]
model_cleaned <- lm(RBC ~ ., data = cleaned_data)
summary(model_cleaned)
summary(model_raw)
seoul_bike_data_clean = seoul_bike_data[-overlap_points, ]
head(seoul_bike_data_clean)

##By removing the outliers, we have improved the fit and accuracy of your model, as reflected by the better residual standard error and higher R-squared values.
model_clean = lm(RBC ~ ., data = seoul_bike_data_clean)
summary(model_clean) 

##Plotting residuals vs. fitted values to check for model assumptions
plot(model_clean$fitted.values, residuals(model_clean),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values (Untransformed RBC)")
abline(h = 0, col = "red", lty = 2)
##Q-Q plot 
qqnorm(residuals(model_clean), main = "Q-Q Plot of Residuals")
qqline(residuals(model_clean), col = "red", lty = 2)  # Add a reference line

##Removing the outliers doesn't correct model assumptions. 

##Lets check for normality and EV using the tests
bp_test_raw_1 = bptest(model_clean)
print(bp_test_raw_1)

##Since Shapiro test can only be done on sample size between 3 and 5000 we will use Kolmogorov-Smirnov Test for normality assumption. 
residuals_1 = resid(model_clean)
ks_test_1 = ks.test(residuals, "pnorm", mean(residuals_1), sd(residuals_1))
print(ks_test_1)
##From the BP test and the KS test we get the same results that the model assumptions are not satisfied by the model. 
##However, comparing the R^2 of the two models we see that the model without outliers performs better. 


full_model = lm(RBC ~ ., data = seoul_bike_data_clean)
stepwise_model = step(full_model, 
                      scope = list(lower = ~1, upper = full_model),
                      direction = "both")
summary(stepwise_model)
##In stepwise selection, the significant predictors to include are: Hour, Temp, Humid_Percent, Visibility, DPT, Rainfall, Holiday, Functioning_Day, Month, Year, Seasons_Spring, Seasons_Summer, Seasons_Winter, Weekday_Monday, Weekday_Saturday, Weekday_Sunday. 
head(seoul_bike_data_clean)
# Specify the columns we want to keep
columns_to_keep = c("RBC", "Hour", "Temp", "Humid_Percent", "Wind_Speed", "Solar_Rad", "Rainfall", "Snowfall", 
                    "Holiday", "Functioning_Day", "Day", "Month", "Year", "Seasons_Spring", "Seasons_Summer", 
                    "Seasons_Winter", "Weekday_Monday", "Weekday_Sunday")

seoul_bike_data_clean <- seoul_bike_data_clean[, columns_to_keep]


full_model = lm(RBC ~ ., data = seoul_bike_data_clean)
summary(full_model)
residuals <- residuals(full_model)
fitted_values <- fitted(full_model)

plot(fitted_values, residuals, main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# QQ Plot
qqnorm(residuals)
qqline(residuals, col = "red")



##Box-Cox Transformation 
seoul_bike_data_clean$RBC_shifted = seoul_bike_data_clean$RBC + abs(min(seoul_bike_data_clean$RBC)) + 1
full_model_shifted = lm(RBC_shifted ~ Hour + Temp + Humid_Percent + Wind_Speed + Solar_Rad + Rainfall + 
                          Snowfall + Holiday + Functioning_Day + Day + Month + Year + 
                          Seasons_Spring + Seasons_Summer + Seasons_Winter + Weekday_Monday + Weekday_Sunday,
                        data = seoul_bike_data_clean)
boxcox_transformation_shifted = boxcox(full_model_shifted, plotit = TRUE)
optimal_lambda_shifted = boxcox_transformation_shifted$x[which.max(boxcox_transformation_shifted$y)]
print(paste("The optimal lambda for the shifted response is:", optimal_lambda_shifted))


seoul_bike_data_clean$RBC_transformed = (seoul_bike_data_clean$RBC^0.222 - 1) / 0.222
head(seoul_bike_data_clean)


trans_model = lm(RBC_transformed ~ . -RBC, data = seoul_bike_data_clean)
summary(trans_model)

##The R^2 of the model is significantly improved, lets check if the model assumptions are satisfied.  
##Plotting residuals vs. fitted values to check for model assumptions
plot(trans_model$fitted.values, residuals(trans_model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values (Untransformed RBC)")
abline(h = 0, col = "red", lty = 2)
##Q-Q plot 
qqnorm(residuals(trans_model), main = "Q-Q Plot of Residuals")
qqline(residuals(trans_model), col = "red", lty = 2)  # Add a reference line

##The transformation does not improve the model assumptions. 

##Lets check for normality and EV using the tests for the transformed model
bp_test_raw_2 = bptest(trans_model)
print(bp_test_raw_2)

residuals_2 = resid(trans_model)
ks_test_2 = ks.test(residuals_2, "pnorm", mean(residuals_2), sd(residuals_2))
print(ks_test_1)

##The p-value (< 2.2e-16) from the KS test suggests that the residuals are not normally distributed. This indicates that the normality assumption is still violated, even after the transformation.
##The p-value from the BP test (< 2.2e-16) suggests that there is still significant unequal variance in the residuals, meaning the assumption of constant variance is still violated after transformation. 
##The linear model appears to be a better fit for the data, providing clearer results and better diagnostic metrics (e.g., residual standard error, adjusted R-squared). The Poisson regression model is likely not suitable, considering the infinite AIC and issues with convergence. Therefore, we prefer the linear model for further analysis.


##Prefered model so far
seoul_bike_data_clean <- subset(seoul_bike_data_clean, select = -c(RBC_transformed, RBC_shifted))
full_model = lm(RBC ~ ., data = seoul_bike_data_clean)
summary(full_model)
lin_r2 = summary(full_model)$r.squared
print(lin_r2)

# Make predictions
full_model_pred <- predict(full_model, newdata = seoul_bike_data_clean)

# Calculate residuals
full_model_residuals <- seoul_bike_data_clean$RBC - full_model_pred

# Calculate MSE
mse_full_model <- mean(full_model_residuals^2)

# Print the MSE
cat("MSE for the full model:", mse_full_model, "\n")


# Apply square transformation to all numeric variables except the target variable
seoul_bike_data_clean_sq <- seoul_bike_data_clean
seoul_bike_data_clean_sq[, -which(names(seoul_bike_data_clean) == "RBC")] <- 
  seoul_bike_data_clean_sq[, -which(names(seoul_bike_data_clean) == "RBC")]^2
full_model_sq <- lm(RBC ~ ., data = seoul_bike_data_clean_sq)
residuals_sq <- residuals(full_model_sq)
fitted_values_sq <- fitted(full_model_sq)
plot(fitted_values_sq, residuals_sq, main = "Residuals vs Fitted (Squared)",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
qqnorm(residuals_sq)
qqline(residuals_sq, col = "red")
summary(full_model_sq)
##Model performance is not good compared to the original model.


##Since the linear model hasn't satisfied assumptions even after transformations, let's explore alternative models.


# Set seed for reproducibility
set.seed(42)
trainIndex <- createDataPartition(seoul_bike_data_clean$RBC, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

train_data <- seoul_bike_data_clean[trainIndex, ]
test_data <- seoul_bike_data_clean[-trainIndex, ]

# Ridge Regression (alpha = 0)
X_train <- model.matrix(RBC ~ . -1, data = train_data)
y_train <- train_data$RBC
ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0, standardize = TRUE)
ridge_best_lambda <- ridge_cv$lambda.min
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = ridge_best_lambda, standardize = TRUE)

# Predictions on test set
X_test <- model.matrix(RBC ~ . -1, data = test_data)
ridge_pred <- predict(ridge_model, s = ridge_best_lambda, newx = X_test)

# Evaluate Ridge
ridge_mse <- mean((test_data$RBC - ridge_pred)^2)
ridge_r2 <- 1 - sum((test_data$RBC - ridge_pred)^2) / sum((test_data$RBC - mean(test_data$RBC))^2)

cat("Ridge Regression MSE on test data:", ridge_mse, "\n")
cat("Ridge Regression R-squared on test data:", ridge_r2, "\n")

# Lasso Regression (alpha = 1)
lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, standardize = TRUE)
lasso_best_lambda <- lasso_cv$lambda.min
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = lasso_best_lambda, standardize = TRUE)

# Predictions on test set
lasso_pred <- predict(lasso_model, s = lasso_best_lambda, newx = X_test)

# Evaluate Lasso
lasso_mse <- mean((test_data$RBC - lasso_pred)^2)
lasso_r2 <- 1 - sum((test_data$RBC - lasso_pred)^2) / sum((test_data$RBC - mean(test_data$RBC))^2)

cat("Lasso Regression MSE on test data:", lasso_mse, "\n")
cat("Lasso Regression R-squared on test data:", lasso_r2, "\n")

# Random Forest
rf_model <- randomForest(RBC ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)

# Predictions on test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate Random Forest
rf_mse <- mean((test_data$RBC - rf_predictions)^2)
rf_r2 <- 1 - sum((test_data$RBC - rf_predictions)^2) / sum((test_data$RBC - mean(test_data$RBC))^2)

cat("Random Forest MSE on test data:", rf_mse, "\n")
cat("Random Forest R-squared on test data:", rf_r2, "\n")

# XGBoost
library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, -which(names(train_data) == "RBC")]), 
                      label = train_data$RBC)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, -which(names(test_data) == "RBC")]), 
                     label = test_data$RBC)

params <- list(
  objective = "reg:squarederror", # For regression problems
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "rmse"
)

# Train the XGBoost model
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 500)

# Predictions on test set
xgb_predictions <- predict(xgb_model, newdata = dtest)

# Evaluate XGBoost
xgb_mse <- mean((test_data$RBC - xgb_predictions)^2)
xgb_r2 <- 1 - sum((test_data$RBC - xgb_predictions)^2) / sum((test_data$RBC - mean(test_data$RBC))^2)

cat("XGBoost MSE on test data:", xgb_mse, "\n")
cat("XGBoost R-squared on test data:", xgb_r2, "\n")

# Comparison of all models
cat("\nModel Comparison:\n")
cat("Linear Regression MSE:", mse_full_model, "R-squared:", lin_r2, "\n")
cat("Ridge Regression MSE:", ridge_mse, ", R-squared:", ridge_r2, "\n")
cat("Lasso Regression MSE:", lasso_mse, ", R-squared:", lasso_r2, "\n")
cat("Random Forest MSE:", rf_mse, ", R-squared:", rf_r2, "\n")
cat("XGBoost MSE:", xgb_mse, ", R-squared:", xgb_r2, "\n")


# Calculate Residuals for each model
ridge_residuals <- test_data$RBC - ridge_pred
lasso_residuals <- test_data$RBC - lasso_pred
rf_residuals <- test_data$RBC - rf_predictions
xgb_residuals <- test_data$RBC - xgb_predictions

# For Ridge Regression
ridge_fitted <- predict(ridge_model, s = ridge_best_lambda, newx = X_test)
ridge_residuals <- test_data$RBC - ridge_fitted
plot(ridge_fitted, ridge_residuals, main = "Ridge Regression: Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals", col = "black")
abline(h = 0, col = "red")

# For Lasso Regression
lasso_fitted <- predict(lasso_model, s = lasso_best_lambda, newx = X_test)
lasso_residuals <- test_data$RBC - lasso_fitted
plot(lasso_fitted, lasso_residuals, main = "Lasso Regression: Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals", col = "black")
abline(h = 0, col = "red")

# For Random Forest
rf_fitted <- rf_predictions
rf_residuals <- test_data$RBC - rf_fitted
plot(rf_fitted, rf_residuals, main = "Random Forest: Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals", col = "black")
abline(h = 0, col = "red")

# For XGBoost
xgb_fitted <- xgb_predictions
xgb_residuals <- test_data$RBC - xgb_fitted
plot(xgb_fitted, xgb_residuals, main = "XGBoost: Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals", col = "black")
abline(h = 0, col = "red")


##In this analysis, several models were evaluated to predict the target variable, with performance measured using Mean Squared Error (MSE) and R-squared. Below is a summary of the results for each model:

##Linear Regression:
##MSE: 125,235.4
##R-squared: 0.618
##The linear regression model demonstrates a moderate level of performance, with a fairly high MSE and an R-squared value that indicates it explains about 61.8% of the variance in the data. Although it provides a baseline model, its predictive power is relatively limited compared to more complex models.

##Ridge Regression:
#3MSE: 130,393
##R-squared: 0.607
##Ridge regression, which includes L2 regularization, results in slightly worse performance compared to linear regression. With a higher MSE and a slightly lower R-squared (60.7%), it appears that the regularization does not significantly improve model accuracy in this case.

##Lasso Regression:
#3MSE: 127,368
##R-squared: 0.617
##Lasso regression, which incorporates L1 regularization, shows performance similar to ridge regression. Its MSE is slightly lower than that of ridge regression, but still relatively high compared to more complex models. The R-squared value (61.7%) is comparable to linear regression.

##Random Forest:
##MSE: 40,938.36
##R-squared: 0.877
##Random Forest significantly outperforms the simpler regression models, with a much lower MSE and an R-squared value of 0.877. This indicates that the Random Forest model is able to capture more of the data's variance and make more accurate predictions.

##XGBoost:
##MSE: 24,433.74
##R-squared: 0.926
##The XGBoost model achieves the best results among all evaluated models. With the lowest MSE and the highest R-squared (92.6%), XGBoost demonstrates the highest predictive accuracy, making it the most effective model for this task.

##Conclusion
##Based on the performance metrics, it is evident that XGBoost is the most effective model in terms of minimizing error and explaining the variance in the data. It consistently outperforms all other models, including Random Forest, which also shows strong performance. In comparison, the Linear Regression, Ridge Regression, and Lasso Regression models fall short in both MSE and R-squared, indicating that they are less capable of accurately modeling the data. Moving forward, XGBoost would be the preferred model for achieving the best predictive performance.

