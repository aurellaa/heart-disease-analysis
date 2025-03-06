rm(list = ls())  # clear environment
set.seed(123)     # ensure reproducibility

# loading the necessary packages
library(dplyr)
library(ggplot2)
library(cowplot)

# loading the data
heart_data <- read.csv("heart_disease_uci.csv")

# first looks at the data
head(heart_data)
str(heart_data)

# recoding the variables to better suit our analyses:
# note that the "num" variable is renamed to "heart_disease" for better
# clarity. it indicates presence and severity of heart disease.
# it has been changed into a binary variable as we are doing logistic
# regression

heart_data <- heart_data |>
  mutate(sex = as.factor(sex),
         dataset = as.factor(dataset),
         cp = as.factor(cp),
         restecg = as.factor(restecg),
         slope = as.factor(slope),
         thal = as.factor(thal),
         ca = as.factor(ca)) |>
  mutate(num = ifelse(num > 0, 1, 0)) |>
  rename(heart_disease = num) 

# converting the heart disease column such that 0 is "healthy" and 1 is "unhealthy"

heart_data$heart_disease <- ifelse(test= heart_data$heart_disease == 0, yes = "Healthy", no = "Unhealthy")
heart_data$heart_disease <- as.factor(heart_data$heart_disease)


# checking if allthat worked out well
str(heart_data)
head(heart_data)

# check if there are missing values
colSums(is.na(heart_data))

# removing the missing values
heart_data <- na.omit(heart_data)

# summary statistics
summary(heart_data)

# verify that the data is not imbalanced...

# do healthy and diseased samples come from each gender
xtabs(~ heart_disease + sex, data = heart_data)

# were all 4 levels of chest pain reported by many patients ?
xtabs(~ heart_disease + cp, data = heart_data)

# were both high and low fasting blood sugar reported by many patients
xtabs(~ heart_disease + fbs, data = heart_data)

#were all levels of restecg reported by many patients
xtabs(~ heart_disease + restecg, data = heart_data)
# st-t abnormality only has 5 patients reported- 2 healthy and 3 unhealthy-- could cause problems

# eda
# age distribution
ggplot(heart_data, aes(x = age)) +
  geom_histogram(binwidth = 5, alpha = 0.8) +
  facet_grid(rows = vars(heart_disease)) +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Count")

# visualizing the data (heart disease proportion by sex)
ggplot(heart_data, aes(x = as.factor(sex), fill = as.factor(heart_disease))) +
  geom_bar(position = "fill", color = "black") +
  labs(x = "Sex", y = "Proportion", title = "Proportion of Heart Disease by Sex",
       fill = "Heart Disease")

# visualizing the data (heart disease proportion by chest pain type)
ggplot(heart_data, aes(x = cp, fill = heart_disease)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Proportion of Heart Disease by Chest Pain Type",
       x = "Chest Pain Type", y = "Proportion", fill = "Heart Disease")

# visualizing the data (heart disease proportion by cholesterol)
ggplot(heart_data, aes(x = heart_disease, y = chol, fill = heart_disease)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Cholesterol Levels by Heart Disease Status",
       x = "Heart Disease", y = "Cholesterol (mg/dL)")

# visualizing the data (heart disease proportion by trestbps)
ggplot(heart_data, aes(x = heart_disease, y = trestbps, fill = heart_disease)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Resting Blood Pressure by Heart Disease Status",
       x = "Heart Disease", y = "Blood Pressure (mmHg)")

# visualizing the data (heart disease proportion by exang)
ggplot(heart_data, aes(x = exang, fill = heart_disease)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Proportion of Heart Disease by Exercise-Induced Angina",
       x = "Exercise-Induced Angina", y = "Proportion", fill = "Heart Disease")


############################################################################################

# single logistic regression: predicting heart disease by sex
logistic1 <- glm(heart_disease ~ sex, data = heart_data, family = "binomial")

#deviance residuals
summary(residuals(logistic1, type = "deviance")) 
# they look good as they are roughly symmetrical and close to being centered on 0

summary(logistic1)
# the coefficients correspond to the following model:
# heart disease = -1.0578 + 1.2919 x the patient is male
# 1.2919 is the log(odds ratio) of the odds that a male will have heart disease over the odds that a female will.
# both p-values are well below 0.05, and therefore the log(odds) and log(odds ratios) are both statistically significant
# the number of fisher scoring iterations tell us how quickly the glm() function 
# converged on the maximum likelihood estimates for the coefficients



#############################################################################################


# multiple logistic regression
logistic2 <- glm(heart_disease ~ ., data = heart_data, family = "binomial")
summary(logistic2)


predicted.data <- data.frame(
  probability.of.hd = logistic2$fitted.values,
  heart_disease = heart_data$heart_disease
)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing = F),]

predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data = predicted.data, aes(x = rank, y = probability.of.hd)) + 
  geom_point(aes(color = heart_disease), alpha = 1, shape = 4, stroke = 2) +
  xlab("index") +
  ylab("predicted probability of getting heart disease")
  

