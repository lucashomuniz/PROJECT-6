# Making Predictions
# Predictions with a ranking model based on randomForest
require(randomForest)

# Generating predictions on test data
previsoes <- data.frame(observado = dados_teste$CreditStatus, previsto = predict(modelo, newdata = dados_teste))

# Viewing the result
View(previsoes)
View(dados_teste)

