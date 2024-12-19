#Some information about data 
#Sex: male or female(Nominal)
#Age: Age of the patient;(Continuous - Although the recorded ages have been truncated to whole numbers, the concept of age is continuous)
#Behavioral
#Current Smoker: whether or not the patient is a current smoker (Nominal)
#Cigs Per Day: the number of cigarettes that the person smoked on average in one day.(can be considered continuous as one can have any number of cigarettes, even half a cigarette.)
#Medical( history)
#BP Meds: whether or not the patient was on blood pressure medication (Nominal)
#revalent Stroke: whether or not the patient had previously had a stroke (Nominal)
#revalent Hyp: whether or not the patient was hypertensive (Nominal)
#Diabetes: whether or not the patient had diabetes (Nominal)
#Medical(current) 
#tot Chol: total cholesterol level (Continuous)
#Sys BP: systolic blood pressure (Continuous)
#Dia BP: diastolic blood pressure (Continuous)
#BMI: Body Mass Index (Continuous)
# Heart Rate: heart rate (Continuous - In medical research, variables such as heart rate though in fact discrete, yet are considered continuous because of large number of possible values.)
# Glucose: glucose level (Continuous)
# 10 year risk of coronary heart disease CHD (binary: ???1???, means ???Yes???, ???0??? means ???No???)


#Change the name of data
datasmt <- framingham

## Install necessary packages
install.packages("psych")

# Load the packages
library(psych)

# Using psych for more detailed descriptive statistics
describe(datasmt) 

# For even more detailed output, I use the summary() function
summary(datasmt)


#Visualization


# Boxplots for selected numeric variables

numeric_columns <- c("age", "cigsPerDay", "totChol", "sysBP", "male", "glucose")

#Loop through the numeric columns and create boxplots

par(mfrow = c(3, 3))  # # Layout: 3x3 grid for multiple plots

for (col in numeric_columns) {
  boxplot(datasmt[[col]], 
          main = paste("Boxplot of", col),
          col = "skyblue",
          ylab = col) }

# Histograms for numeric columns

par(mfrow = c(3, 3))  # Reset to 3x3 layout

for (col in numeric_columns) {
  hist(datasmt[[col]], 
       main = paste("Histogram of", col), 
       xlab = col,
       col = "lightgreen",
       border = "white")
}

#Comments about boxplots and histograms

# cigsPerDay has extreme outliers (heavy smokers).
# totChol shows a few high cholesterol values.
# sysBP  have notable outliers, indicating hypertension cases.
# glucose has extreme outliers, suggesting potential diabetes cases.
# male and age has no outliers.
# age appears well-distributed , others is not well-distributed.


#Multi Linear Regression 
datasmt <- na.omit(datasmt) #Remove missing values
lm_model <- lm(TenYearCHD ~ age + male + cigsPerDay + totChol + sysBP + glucose, data = datasmt)
summary(lm_model) #summary model

#Logit Model

logit_model <- glm(TenYearCHD ~ age + male + cigsPerDay + totChol + sysBP + glucose, data = datasmt, family = binomial(link="logit"))
summary(logit_model)

#Probit Model

probit_model <- glm(TenYearCHD ~ age + male + cigsPerDay + totChol + sysBP + glucose, data = datasmt, family = binomial(link= "probit"))
summary(probit_model)

#Compare Models
#Linear Regression: The r-squared value is 0.098 and the r-squared model explains %9.8, this value is very low, so we can say that the model is not suitable.
#Logit Model and Probit Model: AIC value of the logit model = 2776.5, AIC value of the probit model = 2777.8, the AIC values of both models are quite close, but since the lower AIC value is more compatible with the model, we say that the logit model is more compatible.
#As a result, the best fitting model is the logit model.
#Logit Model "Pr(>|z|)" shows the p-value for each coefficient. Since all independent variables have p-values < 0.05, they are all statistically significant. This indicates that there is strong evidence that these variables affect the risk of developing coronary heart disease within 10 years.
#age:The coefficient is 0.065896. This means that each unit increase in age (for example, 1 year) increases the log-odds ratio of the probability of developing coronary heart disease within 10 years by 0.065896 units.
#male:The coefficient is 0.561446. Men have a log-odds ratio of 0.561446 units greater chance of developing coronary heart disease within 10 years than women.
#cigsPerDay:The coefficient is 0.019226. It means that each cigarette smoked per day increases the log-odds ratio of the probability of developing coronary heart disease within 10 years by 0.019226 units.
#totChol: The coefficient is 0.002272. It means that each unit increase in total cholesterol increases the log-odds ratio of the probability of developing coronary heart disease within 10 years by 0.002272 units.
#sysBP:The coefficient is 0.017534. It means that each unit increase in systolic blood pressure increases the log-odds ratio of the probability of developing coronary heart disease within 10 years by 0.017534 units.
#Glucose:The coefficient is 0.007280. It means that each unit increase in blood glucose level increases the log-odds ratio of the probability of developing coronary heart disease within 10 years by 0.007280 units.
#This logistic regression model examines the factors that affect the risk of developing coronary heart disease over 10 years. The model shows that age, gender, number of cigarettes per day, total cholesterol, systolic blood pressure, and blood sugar levels significantly affect this risk.

#Discuss the practical application of these results in real-world scenarios.
#I think that even though a life-threatening event like a heart attack may not have much of an effect on gender, it does have a significant effect on age and other factors.Because daily cigarette consumption has important effects on organs such as the heart and lungs, at the same time, high glucose levels, cholesterol and blood pressure levels are important values for the human body and health and should be checked regularly.
















