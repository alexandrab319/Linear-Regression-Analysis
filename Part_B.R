install.packages("ggplot2")
# Load the required packages
library(ggplot2)

# Load the data
data <- read.csv("C:/Users/alexb/Downloads/AMS315_Proj1_PartB/096730_PartB.csv")

# Rename the columns to match the variable names in your code
colnames(data) <- c("ID", "IV", "DV")

# Plot the data with LOWESS curve
plot(data$IV, data$DV, main = " #1 Relationship between IV and DV")
lines(lowess(data$IV, data$DV), col = "blue")

 correlation

##############

ggplot(data, aes(x = IV, y = DV)) + 
  geom_point() + 
  labs(title = " #2 Scatter Plot of IV vs. DV", x = "IV", y = "DV")

# Perform ANOVA on the model
library(olsrr)
fit <- lm(DV ~ IV, data = data)
ols_regress(fit)


# Perform log transformation on DV variable
data$logDV <- log(data$DV)

# Plot transformed data
plot(data$IV, data$logDV, main = " #3 Relationship btwn IV and log-transformed DV")

# Create the log-transformed variables
ln_IV <- log(data$IV)
ln_DV <- log(data$DV)

## Correlation
cor(ln_IV, ln_DV)

# Plot the transformed variables with LOWESS curve and linear regression line
plot(ln_IV, ln_DV, main = " #4 Relationship btwn Log-transformed IV and DV")
lines(lowess(ln_IV, ln_DV), col = "blue")
abline(lm(ln_DV ~ ln_IV), col = "red")


## RESIDUAL PLOT

  # Fit linear regression model
model <- lm(DV ~ IV, data = data)

  # Create a data frame of residuals
resid_df <- data.frame(IV = data$IV, Residuals = residuals(model))

  # Plot residuals against IV
plot(resid_df$IV, resid_df$Residuals, 
     main = " #5 Residual Plot", xlab = "IV", ylab = "Residuals")
abline(h = 0, lty = 2)

  
  # Transform the data
  data_trans <- data.frame(IV = data$IV, logDV = log(data$DV))
  
  ggplot(data_trans, aes(x = IV, y = logDV)) +
    geom_point(aes(x = IV, y = logDV)) +
    labs(title = " #6 Transformed data: IV and ln(DV)") +
    xlab("IV") +
    ylab("ln(DV)") +
    geom_smooth(method="lm", se=FALSE, color="red")
  
  # Name of data 
  names(data)
  
  # Correlation of ln(DV) transformation
  correlation
  
  # Print the first 10 rows of the transformed data
  head(data_trans, 10)

  ############
  
  # Load the required library
  library(ggplot2)
  
  # Define the model
  model <- nls(DV ~ a * IV^b, data = data, start = list(a = 1, b = 1))
  
  # Check the model summary
  summary(model)
  
  # Plot the model fit
  ggplot(data, aes(x = IV, y = DV)) +
    geom_point() +
    labs(title = " #7 Nonlinear regression model fit: DV ~ IV^b") +
    xlab("IV") +
    ylab("DV") +
    geom_smooth(method = "nls", formula = y ~ a * x^b, se = FALSE, color = "red", method.args = list(start = list(a = 1, b = 1)))
  
  
########### has slight curvature 
  
  # Load the required library
  library(MASS)
  
  # Fit a second-order polynomial to the data
  polyfit <- lm(DV ~ poly(IV, 2), data = data)
  
  # Compute the Pearson correlation coefficient between the IV and DV
  correlation_poly <- cor(data$IV, polyfit$fitted.values)
  
  # Check the correlation coefficient
  correlation_poly
  
  # Plot the regression line and data
  ggplot(data, aes(x = IV, y = DV)) +
    geom_point() +
    labs(title = " #8 Polynomial Regression: Second-Order") +
    xlab("IV") +
    ylab("DV") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red")
  
  ############

  # check for heteroscedasticity
  
  # Fit the linear regression model
  mode_trans <- lm(DV ~ IV, data = data)
  
  # Create a scatter plot of the residuals vs. fitted values
  ggplot(data = data, aes(x = predict(model), y = residuals(model))) +
    geom_point() +
    xlab("Fitted Values") +
    ylab("Residuals") +
    ggtitle(" #9 Residuals vs. Fitted Values")
  
  ############
  
  # Transform the data
  data_trans <- data.frame(xtrans = data$IV^(3/2), ytrans = data$DV)
  
   # Fit linear regression model using transformed data
  fit <- lm(ytrans ~ xtrans, data = data_trans)
  
  # Create residual plot vs. fitted
  
  plot(fit, which = 1)
  
  #ANOVA table
  ols_regress(fit)
  
  model <- lm(ytrans ~ xtrans, data = mydata)
  summary(model)
  
  # Create scatter plot of transformed data with linear regression line
  ggplot(data_trans, aes(x = xtrans, y = ytrans)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE, color="red") +
    labs(title = " #10 Transformed data: IV^(3/2) and DV", x = "IV^(3/2)", y = "DV")
  
  # Calculate the correlation coefficient between transformed IV and DV
  cor(data_trans$xtrans, data_trans$ytrans)
  
  # Bin the IV variable with smaller bin width
  groups <- cut(data_trans$xtrans, breaks = seq(5.0, 20.0, by = 0.3))
  table(groups)

  x <- ave(data_trans$xtrans, groups)
  data_bin <- data.frame(x=x, y=data_trans$ytrans)

  install.packages("olsrr")
  library("olsrr")
  fit_b <- lm(y ~ x, data = data_bin)
  ols_pure_error_anova(fit_b) 

  fit_b_final <- lm(ytrans ~ xtrans, data = data_trans)
  summary(fit_b_final)  
  
  
  ### second transformation
  
  # Transform the data
  data_trans <- data.frame(xtrans = data$IV^(3/2), ytrans = data$DV^(3/2))
  
  # Fit linear regression model using transformed data
  fit <- lm(ytrans ~ xtrans, data = data_trans)
  
  # Create residual plot vs. fitted 
  plot(fit, which = 1)
  
  #ANOVA table
  ols_regress(fit)
  
  # Create scatter plot of transformed data with linear regression line
  ggplot(data_trans, aes(x = xtrans, y = ytrans)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE, color="red") +
    labs(title = " #11 Transformed data: IV^(3/2) and DV^(3/2)", x = "IV^(3/2)", y = "DV^(3/2)")
  
  # Calculate the correlation coefficient between transformed IV and DV
  cor(data_trans$xtrans, data_trans$ytrans)
  
  # Bin the IV variable with smaller bin width
  groups <- cut(data_trans$xtrans, breaks = seq(5.0, 20.0, by = 0.3))
  table(groups)
  
  x <- ave(data_trans$xtrans, groups)
  data_bin <- data.frame(x=x, y=data_trans$ytrans)
  
  install.packages("olsrr")
  library("olsrr")
  fit_b <- lm(y ~ x, data = data_bin)
  ols_pure_error_anova(fit_b) 
  
  fit_b_final <- lm(ytrans ~ xtrans, data = data_trans)
  summary(fit_b_final)  
  