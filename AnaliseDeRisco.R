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







