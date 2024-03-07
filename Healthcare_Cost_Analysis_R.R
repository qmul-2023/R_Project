#Fetching the data
healthcare<-read.csv(file.choose(),header=T)

View(healthcare)
str(healthcare)
summary(healthcare)
names(healthcare)




#To record the patient statistics, the agency wants to find the age category of people who frequently visit the hospital and has the maximum expenditure.
library(dplyr)
# Analyzing age category with maximum expenditure
age_expenditure_analysis <- healthcare %>%
  group_by(AGE) %>%
  summarize(
    TotalExpenditure = sum(TOTCHG),
    VisitCount = n()
  ) %>%
  arrange(desc(TotalExpenditure))

# Displaying the results
print(age_expenditure_analysis)

# Print the information for the age category with the highest total expenditure
print(age_expenditure_analysis[1, ])




#In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.
# Analyzing diagnosis-related group with maximum hospitalization and expenditure
drg_analysis <- healthcare %>%
  group_by(APRDRG) %>%
  summarize(
    TotalHospitalization = sum(LOS),
    TotalExpenditure = sum(TOTCHG),
    VisitCount = n()
  ) %>%
  arrange(desc(TotalHospitalization), desc(TotalExpenditure))

print(drg_analysis)

# Displaying the results
print(drg_analysis[1, ])




#To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
# Analyzing the relationship between race and hospitalization costs
race_analysis <- lm(TOTCHG ~ RACE, data = healthcare)

# Displaying the ANOVA table
anova_results <- anova(race_analysis)
print(anova_results)




#To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.
# Analyzing severity of hospital costs by age and gender
library(ggplot2)

# Summary statistics by age and gender
age_gender_summary <- healthcare %>%
  group_by(AGE, FEMALE) %>%
  summarize(
    MeanCost = mean(TOTCHG),
    MedianCost = median(TOTCHG),
    MaxCost = max(TOTCHG)
  )

# Displaying summary statistics
print(age_gender_summary)

# Creating a boxplot to visualize the distribution of costs by age and gender
ggplot(healthcare, aes(x = factor(AGE), y = TOTCHG, fill = factor(FEMALE))) +
  geom_boxplot() +
  labs(title = "Distribution of Hospital Costs by Age and Gender",
       x = "Age",
       y = "Total Hospitalization Costs") +
  theme_minimal()




#Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
# Fit a linear regression model
LOS_model <- lm(LOS ~ AGE + FEMALE + RACE, data = healthcare)

# Displaying the summary of the regression model
summary(LOS_model)




#To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.
# Fit a linear regression model for hospital costs including APRDRG
cost_model_updated <- lm(TOTCHG ~ AGE + FEMALE + LOS + RACE + APRDRG, data = healthcare)

# Displaying the summary of the updated regression model
summary(cost_model_updated)























