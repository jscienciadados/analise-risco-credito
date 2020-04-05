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




library(randomForest)
library(caret)
library(readr)

