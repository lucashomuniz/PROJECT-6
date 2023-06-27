# Creating the Predictive Model in R
# Create a ranking model based on randomForest
library(randomForest)

# Cross Tabulation
table(Credit$CreditStatus)

# Function to generate training data and test data
splitData <- function(dataframe, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}

# Generating training and test data
splits <- splitData(Credit, seed = 808)

# Splitting the data
dados_treino <- splits$trainset
dados_teste <- splits$testset

# Checking the number of lines
nrow(dados_treino)
nrow(dados_teste)
  
# Building the model
modelo <- randomForest( CreditStatus ~ CheckingAcctStat
                       + Duration_f
                       + Purpose
                       + CreditHistory
                       + SavingsBonds
                       + Employment
                       + CreditAmount_f, 
                       data = dados_treino, 
                       ntree = 100, 
                       nodesize = 10)

# Imprimondo o resultado
print(modelo)
