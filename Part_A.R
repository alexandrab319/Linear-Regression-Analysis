## Problem A, Part I 

# Load the datasets into R data frames
df1 <- read.csv("C:/Users/alexb/Downloads/part a ams315/096730_DV.csv")
df2 <- read.csv("C:/Users/alexb/Downloads/part a ams315/096730_IV.csv")

# Check the contents of the data frames
head(df1)
head(df2)


library(dplyr)

# Merge the datasets on the ID column
merged_df <- merge(df1, df2, by = "ID") %>%
  arrange(ID)

# Check the contents of the merged data frame
head(merged_df)


# Count the number of subject IDs that had at least one IV or DV
total_subjects <- length(unique(merged_df$ID))

# Count the number of subject IDs that had an IV
iv_subjects <- length(unique(merged_df$ID[!is.na(merged_df$IV)]))

# Count the number of subject IDs that had a DV
dv_subjects <- length(unique(merged_df$ID[!is.na(merged_df$DV)]))

# Count the number of subject IDs that had both an IV && DV
both_subjects <- length(unique(merged_df$ID[!is.na(merged_df$IV) & !is.na(merged_df$DV)]))

# Print results
cat("Total subjects:", total_subjects, "\n")
cat("Subjects with IV value:", iv_subjects, "\n")
cat("Subjects with DV value:", dv_subjects, "\n")
cat("Subjects with both IV and DV value:", both_subjects, "\n")

##Problem A, Part II

#install the statistical package MICE
install.packages("mice")

#load the MICE package
library(mice)

# inspect the pattern of missing data 
md.pattern(merged_df)

# Impute missing values using MICE
imputed_df <- mice(merged_df, m = 5)

# Extract the completed data from the imputed data frame
completed_df <- complete(imputed_df)

# Check the number of missing values in the completed data frame
cat("Number of missing values in completed data:", sum(is.na(merged_df)), "\n")

#check if any missing data points in the data frame 
any(is.na(merged_df))

# inspect the pattern of missing data
md.pattern(completed_df)


##Part A, Part 3 

# Fit OLS model
model <- lm(DV ~ IV, data = completed_df)

# Print model summary
summary(model)

# Print ANOVA table
anova(model)


# Load the knitr library for creating tables
library(knitr)

# Use the kable function to create a table of the ANOVA results from the linear regression model
# Set the caption for the table to 'ANOVA Table'
kable(anova(model), caption='ANOVA Table')


# Plot the scatterplot
plot(merged_df$DV ~ completed_df$IV, main=' #1 Scatter: DV ~ IV', xlab='IV', ylab='DV', pch=20)

# Add the regression line
M <- lm(DV ~ IV, data = completed_df)
abline(M, col='darkorchid', lty=3, lwd=2)

# Add the legend
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='darkorchid')

# Calculate the confidence intervals for the model coeff.
confint(model, level = 0.95)

# Calculate the confidence intervals for the model coeff.
confint(model, level = 0.99)

