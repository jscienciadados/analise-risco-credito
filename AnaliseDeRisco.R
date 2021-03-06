# Avaliação de Risco de Crédito

# Analisando uma base de crédito histórica para conceder novo crédito 

# Para esta análise, vamos usar um conjunto de dados German Credit Data,
# já devidamente limpo e organizado para a criação do modelo preditivo.



# Coletando os dados
credit.df <- read.csv("credit_dataset.csv", header = TRUE, sep = ",")

# Normalizando os dados

# Convertendo as variáveis para o tipo fator (categoricas)
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

# Normalização
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center = T, scale = T)
  }
  return(df)
}

# Normalizando as variáveis 

# numéricas
numeric.vars <- c("credit.duration.months", "age", "credit.amount")
credit.df <- scale.features(credit.df, numeric.vars)

# categóricas
categorical.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')

credit.df <- to.factors(df = credit.df, variables = categorical.vars)

# Dividindo os dados em dados de treino e de teste
# Divisão treino e teste - 60:40 ratio
indexes <- sample(1:nrow(credit.df), size = 0.6 * nrow(credit.df))
train.data <- credit.df[indexes,]
test.data <- credit.df[-indexes,]

# Feature Selection
library(caret)
library(randomForest)

# Função para seleção de variaveis
run.feature.selection <- function(num.iters=20, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv", 
                        verbose = FALSE, returnResamp = "all", 
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var, 
                     sizes = variable.sizes, 
                     rfeControl = control)
  return(results.rfe)
}

# Executando a função
rfe.results <- run.feature.selection(feature.vars = train.data[,-1], 
                                     class.var = train.data[,1])

# Visualizando os resultados
# Mostra as variaveis mais relevantes para a criação do modelo
varImp((rfe.results))

# Criando e Avaliando a primeira versao do modelo
install.packages("ROCR")
library(ROCR)

# Biblioteca de utilitario para construção de gráficos
source("plot_utils.R")

# separate feature and class variables
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

# Construindo um modelo de regressão logística
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
lr.model <- glm(formula = formula.init, data = train.data, family = "binomial")

# Visualizando o modelo
summary(lr.model)

# Testando o modelo nos dados de teste
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- round(lr.predictions)

# Avaliando o modelo
# Confusion Matrix
confusionMatrix(table(data = lr.predictions, reference = test.class.var), positive = '1')

# Construindo o modelo com as variáveis selecionadas
formula.new <- "credit.rating ~ account.balance +
credit.purpose + previous.credit.payment.status
+ savings + credit.duration.months"
formula.new <- as.formula(formula.new)
lr.model.new <- glm(formula = formula.new, data = train.data, family = "binomial")

# Visualizando o modelo
summary(lr.model.new)

# Testando o modelo nos dados de teste
lr.predictions.new <- predict(lr.model.new, test.data, type="response") 
lr.predictions.new <- round(lr.predictions.new)

# Avaliando o modelo
confusionMatrix(table(data=lr.predictions.new, reference=test.class.var), positive='1')

# Curva ROC e Avaliação do Modelo Final

# Criando curvas ROC
lr.model.best <- lr.model
lr.prediction.values <- predict(lr.model.best, test.feature.vars, type = "response")
predictions <- prediction(lr.prediction.values, test.class.var)
par(mfrow = c(1,2))
plot.roc.curve(predictions, title.text = "Curva ROC")
plot.pr.curve(predictions, title.text = "Curva Precision/Recall")

# Trocando o Algoritmo - Arvore de decisão com RandomForest

modelo <- randomForest(credit.rating ~ account.balance 
                       + credit.purpose 
                       + previous.credit.payment.status 
                       + savings + credit.duration.months,
                       data = train.data,
                       ntree = 100, 
                       nodesize = 10)
# Imprimindo o resultado
print(modelo)

# Gerando as previsões
avaliacao <- predict(modelo, test.data, type = "response")

# Avaliando o Resultado do Modelo - 72%
confusionMatrix(table(data=avaliacao, reference=test.class.var), positive='1')

# Criando modelo com todas as variaveis 
modelo_v2 <- randomForest(credit.rating ~ ., data = train.data, ntree = 100, nodesize = 10)
print(modelo_v2)

# Previsão
avaliacao2 <- predict(modelo_v2, test.data, type = "response")
View(avaliacao2)

# Matriz de confusão - 75%
confusionMatrix(table(data =avaliacao2, reference = test.class.var), positive = '1')

# Usando um Conjunto de variaveis mais importante
modelo_v3 <- randomForest(credit.rating ~  account.balance
                          + previous.credit.payment.status
                          + credit.duration.months
                          + savings
                          + credit.amount
                          + other.credits,
                          data = train.data,
                          ntree = 100,
                          nodesize = 10)

# Gerando as previsões
avaliacao <- predict(modelo_v3, test.data, type = "response")

# Matriz de confusão - 75,3%
confusionMatrix(table(data =avaliacao, reference = test.class.var), positive = '1')

# Modelo support vector machine
library(e1071)
model_svm <- svm(credit.rating ~ ., data = train.data)
summary(model_svm)

# Teste
teste <- predict(model_svm, test.data)
confusionMatrix(table(data = teste, reference = test.class.var), positive = '1')






