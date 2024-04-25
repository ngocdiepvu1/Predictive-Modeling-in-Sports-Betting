
library(GGally)


dt <- read.csv("/Users/vudiep/Downloads/CSC542 Group Project/soccermatch.csv")

str(dt)

#Fixing Date Column Name and Type:

dt$Date <- as.Date(dt$Date, format="%m/%d/%Y")

str(dt)


summary(dt)  # Get summary statistics for each column

# new variable 
dt$TotalGoals = dt$FTHG + dt$FTAG


#head(dt)

#summary(dt)

# Histogram of Home Odds
hist(dt$AvgH)

# Find the range of your data
min_odds <- min(dt$AvgH)
max_odds <- max(dt$AvgH)

# Define breaks starting from the minimum and increasing by 5% of the range until the maximum
breaks <- seq(from = min_odds, to = max_odds, by = (max_odds - min_odds) * 0.02)

# Ensure the last break is the max_odds value if it's not already included
if(max_odds > max(breaks)) {
  breaks <- c(breaks, max_odds)
}

# Plot the histogram with these new breaks
hist(dt$AvgH, breaks = breaks, main = "Histogram of Home Odds", xlab = "Home Odds",xaxt="n")

# Generate axis labels rounded to two decimal places
axis_labels <- round(breaks, 2)
# Adding an axis on the bottom (side = 1) with custom labels
axis(side = 1, at = breaks, labels = axis_labels)


# Boxplot Total Score
boxplot(dt$TotalGoals, main="Total Score")  
# Histogram of Total Score
hist(dt$TotalGoals)

# Update the bins and add lables 
max_score <- max(dt$TotalGoals)  # Find the maximum score
breaks <- 0:(max_score + 1)  # Create a sequence of breaks from 0 to max_score + 1
hist(dt$TotalGoals, breaks = breaks, right = FALSE, labels = TRUE)  # Plot the histogram



# Select only numeric columns from dt
dt_numeric <- dt[sapply(dt, is.numeric)]

# Use pairs on the numeric-only subset of dt
pairs(dt_numeric)

# Create a subset of variables for the pairs plot
dt_subset <- dt[, c("HLP", "ALP", "FTHG", "FTAG", "AvgH", "AvgD", "AvgA", "TotalGoals", "FTR")]

# Generating the pairs plot
pairs(dt_subset)

# Generating an enhanced pairs plot with ggpairs
ggpairs(dt_subset)

# Scatter Plot
plot( dt$AvgH, dt$FTHG)

plot(dt$Home.Result, dt$AvgH, yaxt = "n") # Suppress automatic Y-axis
axis(2, at = seq(min(dt$AvgH, na.rm = TRUE), 
                 max(dt$AvgH, na.rm = TRUE), 
                 by = 0.25)) # Add Y-axis with .25 increments

plot(dt$FTR, dt$AvgH, 
     ylab = "Home Odd", xlab = "Home Result", 
     ylim = c(floor(min(dt$AvgH)), ceiling(max(dt$AvgH))))

# Setting breaks at every .25 interval within the range of Home.Odd
breaks <- seq(from = floor(min(dt$AvgH)), to = ceiling(max(dt$AvgH)), by = .2)

# Adding horizontal lines at each break
abline(h = breaks, col = "gray", lty = 2)



# -----Win rate at home
home_win_rate <- mean(dt$FTR == 0)  # Assuming '0' indicates a win for home team based on your coding


# Win rate away
away_win_rate <- mean(dt$FTR == 2)  # Assuming '2' indicates a win for away team based on your coding

home_win_rate
away_win_rate

# Average home score
avg_home_score <- mean(dt$FTHG)

# Average away score
avg_away_score <- mean(dt$FTAG)

avg_home_score
avg_away_score


# Conducting a paired t-test to compare full-time home and away goals
# Assumption: Scores are paired by match, comparing the same matches' home and away goals
t.test(dt$FTHG, dt$FTAG, paired = TRUE)

# Paired t-test results:
# - t-value: 22.929, df: 20900, p-value: < 2.2e-16
# - Confidence Interval: 95% CI of mean difference is [0.2472575, 0.2934822]
# - Mean difference between home and away goals: 0.2703698

# Interpretation:
# The results significantly support the home advantage hypothesis, with home teams scoring more on average.
# The very small p-value (< 2.2e-16) indicates this difference is statistically significant.
# The confidence interval does not include 0, further validating the presence of a meaningful difference.
# Overall, this analysis confirms that home teams tend to score more goals than away teams.



# Performing a Chi-squared test to compare observed match outcomes with expected frequencies
# Assuming equal distribution of Full-Time Results (Win, Draw, Lose) as the expected scenario
observed <- table(dt$FTR)
expected <- rep(sum(observed) / 3, 3)  # Assuming equal expected frequencies for 3 outcomes
chisq.test(x = observed, p = expected / sum(expected))

# Chi-squared test results:
# - X-squared: 911.63, df: 2, p-value: < 2.2e-16

# Interpretation:
# The Chi-squared test result significantly deviates from the expected equal distribution of match outcomes.
# With a p-value < 2.2e-16, the observed frequencies of wins, draws, and losses are not equally distributed.
# The large X-squared value indicates a strong discrepancy between observed and expected frequencies,
# suggesting that match outcomes are not random and likely influenced by various factors.
# This analysis provides statistical evidence against the hypothesis of equal distribution among match outcomes.



# Calculate distribution for Home Results
Full_Time_Result <- table(dt$FTR) / nrow(dt) * 100



# Print the distributions
print(Full_Time_Result)


# Combined bar plot for Home and Away Result distributions
barplot(rbind(Full_Time_Result), beside = TRUE, 
        col = c("blue"), legend = TRUE, 
        names.arg = c("Win", "Draw", "Loss"),
        args.legend = list(title = "Team Type", x = "topright", legend = c("Home", "Away"), cex = 0.4))

# Analyzing the correlation between Average Home Odds (AvgH) and Full-Time Result (FTR) using Spearman's method
cor.test(~ AvgH + FTR, data = dt, method = "spearman")

# Spearman's rank correlation results:
# - rho: 0.3343254, indicating a moderate positive correlation between AvgH and FTR
# - S: 1.013e+12, a measure of the data's deviation from perfect correlation
# - p-value: < 2.2e-16, suggesting the correlation is statistically significant

# Interpretation:
# The positive rho value indicates that as the average home odds increase, there is a tendency for the full-time result
# (expressed in a rankable form, presumably with higher values indicating more favorable outcomes for the home team) to also increase.
# This could suggest that higher odds (implying a lower expectation of winning) are associated with better performance outcomes, which might
# seem counterintuitive but could reflect complexities in the odds-setting process and match dynamics.
# The very low p-value strongly supports the existence of a significant correlation between these variables.

# Transforming Full-Time Result (FTR) into a binary outcome for Home Win (1 for Win, 0 Otherwise)
dt$Home.Win <- ifelse(dt$FTR == 0, 1, 0)

library(caTools)
# Creating a sample diving into the ratio of 80:20 
sample <- sample.split(dt, SplitRatio = 0.80) 

# 80% for training - 16181 examples
training_dt <- subset(dt, sample == TRUE) 
cat("Training Dataset:", nrow(training_dt), "examples")
head(training_dt)

# 20% for testing - 4720 examples
testing_dt <- subset(dt, sample == FALSE) 
cat("Training Dataset:", nrow(testing_dt), "examples")
head(testing_dt)

# Fitting a logistic regression model to predict Home Win based on Average Home Odds (AvgH)
model <- glm(Home.Win ~ ., family = binomial, data = dt)
summary(model)

# Logistic Regression Model Summary:
# - Intercept: 1.27673 (highly significant, p < 2e-16), indicating the log odds of a home win when AvgH is 0
# - Coefficient for AvgH: -0.63776 (highly significant, p < 2e-16), suggesting a negative relationship between AvgH and the likelihood of a home win
# - This means as AvgH increases, the likelihood of the home team winning decreases

# Model Fit:
# - Null deviance: 28557 on 20900 degrees of freedom
# - Residual deviance: 26850 on 20899 degrees of freedom, indicating an improvement in the model fit with AvgH as a predictor
# - AIC: 26854, useful for model comparison

# Interpretation:
# The significant negative coefficient for AvgH in the logistic regression model suggests that higher home odds correlate with a lower probability of home team wins.
# This aligns with expectations, as higher odds typically indicate a lower expected chance of winning.
# The model's diagnostics indicate a decent fit, with a noticeable reduction in deviance when including AvgH as a predictor.


# Extending the logistic regression model to predict Home Win including league positions and odds
# Model predictors: Home League Position (HLP), Away League Position (ALP), Average Home Odds (AvgH), Average Away Odds (AvgA)
model <- glm(FTR ~ HLP + ALP + AvgH + AvgD + AvgA + AvgCH + AvgCD + AvgCA, data = training_dt)
summary(model)
# Get the predicted probabilities
predicted_probabilities <- predict(model, testing_dt, type = "response")
summary(predicted_probabilities)

# Convert probabilities to class predictions
# Convert probabilities to class predictions
predicted_values <- ifelse(predicted_probabilities < 0.5, 0,
                           ifelse(predicted_probabilities > 1.50, 2, 1))
predicted_values

# Calculate the accuracy
accuracy <- sum(predicted_values == testing_dt$FTR) / nrow(testing_dt)
print(paste("Accuracy:", accuracy))


# Model Summary:
# Coefficients:
# - Intercept: Not significant (p = 0.8915), indicating the baseline log odds of a home win when predictors are 0
# - HLP: Slightly negative coefficient (-0.0053) with p = 0.0579, suggesting a trend where higher home league positions (worse standings) slightly decrease the likelihood of a home win
# - ALP: Coefficient near zero (0.00006) with p = 0.9839, indicating no significant effect of away league position on home win probability
# - AvgH: Significant negative coefficient (-0.3403) with p < 2e-16, meaning higher home odds significantly decrease the likelihood of a home win
# - AvgA: Significant positive coefficient (0.1671) with p < 2e-16, indicating higher away odds significantly increase the likelihood of a home win

# Model Fit and Quality:
# - Null deviance: 28557 on 20900 degrees of freedom
# - Residual deviance: 26498 on 20896 degrees of freedom, showing an improvement with the inclusion of predictors
# - AIC: 26508, for comparing model quality

# Interpretation:
# This model demonstrates that the odds (AvgH and AvgA) are significant predictors of home win likelihood, with home odds decreasing and away odds increasing the probability of a home win. The effects of league positions (HLP and ALP) are less pronounced, with home league position showing a marginal negative trend. The significant reduction in deviance and the AIC value indicate a good model fit and an improvement over a model without predictors.


# Extending logistic regression model with interaction terms
# Predicting Home Win based on:
# - Home League Position (HLP)
# - Away League Position (ALP)
# - Average Home Odds (AvgH)
# - Average Away Odds (AvgA)
# Includes interaction between HLP and AvgH, and ALP and AvgA
model_interaction <- glm(Home.Win ~ HLP * AvgH + ALP * AvgA, family = binomial, data = training_dt)
summary(model_interaction)

# Get the predicted probabilities
predicted_probabilities <- predict(model_interaction, newdata = testing_dt, type = "response")

# Convert probabilities to class predictions
predicted_values <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Calculate the accuracy
accuracy <- sum(predicted_values == testing_dt$Home.Win) / nrow(testing_dt)
print(paste("Accuracy:", accuracy))

# Model with Interactions Summary:
# - The interaction between HLP and AvgH is significant, indicating the relationship between home league position and average home odds significantly affects the likelihood of a home win.
# - The coefficient for HLP:AvgH (0.0086597) suggests that as HLP improves or AvgH increases, the effect on Home.Win likelihood also changes, reinforcing the combined influence of league position and betting odds on outcomes.
# - ALP and its interaction with AvgA are not as significant, indicating the away team's league position and its odds do not have a strong interactive effect on the home team's win likelihood.
# - Significant main effects for AvgH and AvgA, with AvgH negatively affecting and AvgA positively affecting the likelihood of a home win, remain consistent.

# Interpretation:
# This model underscores the importance of considering not just the direct effects of league positions and odds but also how these variables interact in predicting match outcomes. The model suggests nuanced dynamics where home team's league position combined with betting odds can significantly influence match outcomes, highlighting the multifaceted nature of football match predictions.

# Model Fit:
# - Reduction in residual deviance compared to the null model suggests improved fit.
# - AIC slightly lower than the non-interaction model, indicating a marginally better fit considering the complexity added by interaction terms.

library(randomForest)
install.packages("caret")
library(caret)

# Fitting a Random Forest classification model to predict home win based on league positions and average odds
rf_model <- randomForest(as.factor(FTR) ~ ALP + HLP + AvgH + AvgD + AvgA + AvgCH + AvgCD + AvgCA, data=training_dt)
print(rf_model)

# Get the predicted classes for the testing set
predicted_values <- predict(rf_model, newdata = testing_dt)
predicted_values
# Convert FTR to a factor with the same levels as the predicted values

# Calculate the accuracy
accuracy <- sum(predicted_values == testing_dt$FTR) / nrow(testing_dt)
accuracy
print(paste("Accuracy:", accuracy))

# Calculate the confusion matrix
confusion_matrix <- table(Predicted = predicted_values, Actual = testing_dt$FTR)
print(confusion_matrix)

# Load the cvTools package
install.packages('cvTools')
library(cvTools)
library(caret)

cv.error.10 <- rep(0, 10)

# Perform 10-fold cross-validation
for (i in 1:10) {
  # Split the data into training and validation sets
  train_indices <- sample(1:nrow(dt), nrow(dt) * 0.9)
  train_set <- dt[train_indices, ]
  validation_set <- dt[-train_indices, ]
  
  # Train the model on the training set
  rf_model <- randomForest(as.factor(FTR) ~ HLP + ALP + AvgH + AvgD + AvgA + AvgCH + AvgCD + AvgCA, data = train_set)
  
  # Get the predicted classes for the validation set
  predicted_values <- predict(rf_model, newdata = validation_set)
  
  # Calculate the accuracy
  cv.error.10[i] <- sum(predicted_values == validation_set$FTR) / nrow(validation_set)
}

# Print the cross-validation error for each fold
print(cv.error.10)

# Print the average cross-validation error
print(mean(cv.error.10))


# Random Forest Model Summary:
# - Type: Classification with home win as the outcome
# - Trees: 500, with 2 variables tried at each split for the best split
# - OOB (Out-of-Bag) estimate of error rate: 39.75%, indicating the model's prediction error on unseen data
# - Confusion Matrix:
#   - For non-wins (0), class error rate of 28.98%
#   - For wins (1), class error rate of 54.07%, indicating challenges in accurately predicting actual home wins

# Interpretation:
# The Random Forest model's higher OOB error rate compared to logistic regression suggests that while it is capable
# of capturing complex relationships without explicit interaction terms, it may not be as effective in this scenario
# for predicting home wins. The considerable class error, especially in predicting wins, points to potential areas
# for model improvement, such as parameter tuning or addressing class imbalance.
# This approach, however, benefits from Random Forest's inherent handling of non-linear relationships and interaction effects.
