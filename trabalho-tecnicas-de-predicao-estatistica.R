library(data.table)
library(dplyr)
library(ggplot2)
base = fread(
  input = paste0("healthcare-dataset-stroke-data.csv"),
  header = T,
  na.strings = "NA",
  data.table = FALSE,
  dec = ","
)

# Formatando a base
base <- base %>%
  filter(gender != "Other")

base$heart_disease = as.factor(base$heart_disease)
base$hypertension = as.factor(base$hypertension)
base$work_type = as.factor(base$work_type)
base$gender = as.factor(base$gender)
base$Residence_type = as.factor(base$Residence_type)
base$ever_married = as.factor(base$ever_married)
base$smoking_status = as.factor(base$smoking_status)
base$stroke = as.factor(base$stroke)

base$age = as.numeric(base$age)
base$avg_glucose_level = as.numeric(base$avg_glucose_level)
base$bmi = as.numeric(base$bmi)

base = na.omit(base)

summary(base)

