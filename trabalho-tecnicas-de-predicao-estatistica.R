library(data.table)
library(ggplot2)
library(car)
library(dplyr)
base = fread(
  input = paste0("healthcare-dataset-stroke-data.csv"),
  header = T,
  na.strings = "NA",
  data.table = FALSE,
  dec = ","
)

# Formatando a base
# Removendo o genero outro, pois ele possue apenas uma linha de dados
base = base %>%
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

# Gráficos
numeric_vars = c("age", "avg_glucose_level", "bmi")

# Boxplot
for (name in numeric_vars) {
  boxplot_plot = ggplot(base[, c(name, "stroke")], aes(x = stroke, y = !!as.name(name), fill = stroke)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", name, "by stroke"),
         x = "Stroke",
         y = name) +
    theme_minimal()
  
  print(boxplot_plot)
}

# Mosaic plot para ver as qualitativas
colors <- c("#F8766D", "#00BFC4")
mosaicplot(table(base$gender, base$stroke), color = colors, main = "gender/stroke")
mosaicplot(table(base$hypertension, base$stroke), color = colors, main = "hypertension/stroke")
mosaicplot(table(base$heart_disease, base$stroke), color = colors, main = "heart_disease/stroke")
mosaicplot(table(base$ever_married, base$stroke), color = colors, main = "ever_married/stroke")
mosaicplot(table(base$work_type, base$stroke), color = colors, main = "work_type/stroke")
mosaicplot(table(base$Residence_type, base$stroke), color = colors, main = "Residence_type/stroke")
mosaicplot(table(base$smoking_status, base$stroke), color = colors, main = "smoking_status/stroke")

# Criando a base de treinamento e de teste
set.seed(42)
train_proportion = 0.8  
index = sample(nrow(base), size = round(train_proportion * nrow(base)))
train_data = base[index, ]
test_data = base[-index, ]

# Modelo 1
predictor_vars = setdiff(names(train_data), c("id", "stroke"))
m0 = glm(stroke ~ 1, data = train_data, family = binomial())
upper_formula =
  as.formula(paste("~", paste(predictor_vars, collapse = " + ")))
m1 = step(m0,
           scope = list(lower = ~ 1, upper = upper_formula),
           direction = "forward")

# Análise do modelo
Anova(m1)
summary(m1)

# Testando o modelo
m1_predictions = data.frame(
  actual = test_data$stroke,
  predicted = predict(m1, newdata = test_data)
)
m1_predictions$binary_predictions = ifelse(m1_predictions$predicted >= 0.5, 1, 0)
sum(m1_predictions$binary_predictions == test_data$stroke) / nrow(test_data)

# Observando as predições, podemos garantir que esses modelos possuem a mesma precisão, isso acontece graças a genialidade desse modelo
# Esse modelo nega a existencia de derrames, e como a maior parte dos dados são pessoas sem derrame, ela acerta na maioria dos casos

indices_0 = which(base$stroke == 0)
indices_1 = which(base$stroke == 1)

num_rows_to_keep = length(indices_1)

indices_to_remove = sample(indices_0, size = length(indices_0) - num_rows_to_keep)

base_fitted = base[-indices_to_remove[base$stroke[indices_to_remove] == 0], ]

summary(base_fitted)

# Gráficos
numeric_vars = c("age", "avg_glucose_level", "bmi")

# Boxplot
for (name in numeric_vars) {
  boxplot_plot = ggplot(base_fitted[, c(name, "stroke")], aes(x = stroke, y = !!as.name(name), fill = stroke)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", name, "by stroke"),
         x = "Stroke",
         y = name) +
    theme_minimal()
  
  print(boxplot_plot)
}

# Mosaic plot para ver as qualitativas
mosaicplot(table(base_fitted$gender, base_fitted$stroke), color = colors, main = "gender/stroke")
mosaicplot(table(base_fitted$hypertension, base_fitted$stroke), color = colors, main = "hypertension/stroke")
mosaicplot(table(base_fitted$heart_disease, base_fitted$stroke), color = colors, main = "heart_disease/stroke")
mosaicplot(table(base_fitted$ever_married, base_fitted$stroke), color = colors, main = "ever_married/stroke")
mosaicplot(table(base_fitted$work_type, base_fitted$stroke), color = colors, main = "work_type/stroke")
mosaicplot(table(base_fitted$Residence_type, base_fitted$stroke), color = colors, main = "Residence_type/stroke")
mosaicplot(table(base_fitted$smoking_status, base_fitted$stroke), color = colors, main = "smoking_status/stroke")

# Criando a base_fitted de treinamento e de teste
set.seed(42)
train_proportion = 0.8  
index = sample(nrow(base_fitted), size = round(train_proportion * nrow(base_fitted)))
train_data = base_fitted[index, ]
test_data = base_fitted[-index, ]

# Modelo 2
predictor_vars = setdiff(names(train_data), c("id", "stroke"))
m0 = glm(stroke ~ 1, data = train_data, family = binomial())
upper_formula =
  as.formula(paste("~", paste(predictor_vars, collapse = " + ")))
m2 = step(m0,
          scope = list(lower = ~ 1, upper = upper_formula),
          direction = "forward")

# Análise do modelo
Anova(m2)
summary(m2)

# Modelo Final
# Vamos remover colunas que não são significativas
m3 = glm(stroke ~ age + hypertension, data = train_data, family = binomial())

# Análise do modelo
Anova(m3)
summary(m3)

# Testando qual é o melhor modelo
m2_predictions = data.frame(
  actual = test_data$stroke,
  predicted = predict(m2, newdata = test_data)
)
m2_predictions$binary_predictions = ifelse(m2_predictions$predicted >= 0.5, 1, 0)
sum(m2_predictions$binary_predictions == test_data$stroke) / nrow(test_data)

m3_predictions = data.frame(
  actual = test_data$stroke,
  predicted = predict(m3, newdata = test_data)
)
m3_predictions$binary_predictions = ifelse(m3_predictions$predicted >= 0.5, 1, 0)
sum(m3_predictions$binary_predictions == test_data$stroke) / nrow(test_data)

