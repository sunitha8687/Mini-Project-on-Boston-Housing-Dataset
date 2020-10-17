#Assignmet DDMO Random Forest - Point 3
#Autor: Daniel Zito
#Date: 25/06/2019 5:10 PM

#Analysis of data
load("RFimportance2.Rdata")
View(df2)
head(df2)
summary(df2)
str(df2)
plot(df2)

#----------Looking for hidden variables existency----------

df2_original <- df2 #Cloning the original dataset for further analysis

#The function below calculates the difference from each row output and the sum of all the variables in input.
#Since the output is the sum of all the input variables, the difference should be 0. Or else, hidden values will be found out by the difference
difference <- function(df2) {
  hidden_values <- numeric() #This will be a vector containing the values of the hidden variables
  for (row in 1:nrow(df2)) {
    diff <- df2[row, ncol(df2)] - sum(df2[row, 1:(ncol(df2) - 1)]) #sum sums al the variables in the dataframe, for each column, other than the last one (out)
    hidden_values <- c(hidden_values, diff)
  }
  return(hidden_values)
}

#Function that checks if there are hidden values based on the differences previously calculated
check <- function(hidden_values) {
  flag <- FALSE #If there are hidden values this flag turns into TRUE
  if (any(round(hidden_values, digits = 10) != 0)) { #We evaluate the difference up to 10 decimal digits to ignore computational approximations
    print("The dataframe contains hidden values")
    return(flag = TRUE)
  } else {
    print("The dataframe DOES NOT contain hidden values")
    return(flag)
  }
}

#Checking whether there are hidden variables or not based on the difference between output and inputs
hidden_values <- difference(df2)
hidden_values
hid_val_pres <- check(hidden_values)

#If and only if there are hidden variables we add them to our dataset.
#Hidden variables means the output is not given only by the sum of the xin2 to xin5 variables.
#We suppose xin1 is missing. We add the new variable xin1 to our dataframe df2 for consistency
if (hid_val_pres == TRUE) {
  df2$xin1 <- hidden_values
  df2 <- df2[c("xin1", "xin2", "xin3", "xin4", "xin5", "out")] #We reorder the dataframe by putting xin1 as first column in the dataframe
}

#Sanity check to make sure the hidden variables have been correctly considered and inserted
hidden_values <- difference(df2)
hidden_values
hid_val_pres <- check(hidden_values)

plot(df2)

#----------Importance of variables in our dataset NO randomForest----------

boxplot(df2)
correlations <- cor(df2, df2$out)
correlations
plot(correlations, main = "Variables correlation")

#We fit a linear model to see the impact of each input variable on the output variable based on the t-statistic value and p value.
#Since we want to see the impact of each input varible independently from each other, we fit for each variable a linear model.
#The function below calculates t-statistic value, p-value and significance for each single variable with respect to the output
tps_table <- function(df) {
  parameters <- NULL
  for (i in 1:(ncol(df) - 1)) {
    print(i)
    xin <- df[, i] #Copying all the values for the ith varible to be processed in the linear model
    fit_lm <- lm(df$out ~ xin, data = df) #xin iterates from xin1 (or xin2) to xin5
    print(summary(fit_lm))
    t_val <- coef(summary(fit_lm))[-1, "t value"] #Extracting t-statistic values for the ith predictor
    p_val <- coef(summary(fit_lm))[-1, "Pr(>|t|)"] #Extracting p-value for the ith predictor
    significance <- sig_code(p_val) #Based on the p-values returns a significant values in stars code (see function below)
    parameters <- rbind(parameters, cbind(t_val, p_val, significance)) #Combining t-statistic values, p-values and significance in a table
  }
  return(parameters)
}

features_df2 <- tps_table(df2)
features_df2_or <- tps_table(df2_original) #The hidden variable was the one with the least impact on the dataframe (xin1)

features_df2
features_df2_or

plot(features_df2[, "t_val"]) #Plotting the t-statistic values for both the dataframe
plot(features_df2_or[, "t_val"])

#The function below is used to show the significance in stars code
sig_code <- function(p_val) {
  code <- " "
  if (p_val >= 0 & p_val <= 0.001)
    return(code = "***")
  else if 
    (p_val > 0.001 & p_val <= 0.01)
    return(code = "**")
  else if
    (p_val > 0.01 & p_val <= 0.05)
    return(code = "*")
  else if
    (p_val > 0.05 & p_val <= 0.1)
    return(code = ".")
  else
    return(code)
}

#----------Importance of variables in our dataset WITH randomForest----------

set.seed(123)

library(randomForest)

#Creating randomForest objects for both the dataset
df2_RF <- randomForest(df2$out ~ ., data = df2, importance = TRUE)
df2_RF

df2_or_RF <- randomForest(df2_original$out ~ ., data = df2_original, importance = TRUE)
df2_or_RF

importance(df2_RF)
importance(df2_or_RF)

#Instead of plot() we use varImpPlot(). It does basically the same
varImpPlot(df2_RF)
varImpPlot(df2_or_RF)

#----------Conclusion----------
#The results shown by the randomForest match with our expectations/observations
