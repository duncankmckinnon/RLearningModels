#Logistic Regression Function
#Duncan McKinnon


LogisticRegression_Model <- function(XTrain, YTrain, alpha = 0.01, num_iters = 10, raw = F,  XTest = NULL, YTest = NULL)
{
  optimize <- function(w, b, XTrain, YTrain, alpha, num_iters)
  {
    costs <- c()
    for(i in 1:num_iters)
    {
      vals <- propogate(w, b, XTrain, YTrain)
      
      w = w - (alpha * vals$dw)
      b = b - (alpha * vals$db)
      costs <- c(costs, vals$cost)
    }
    return(list("w" = w, "b" = b, "dw" = vals$dw, "db" = vals$db,  "costs" = costs))
  }
  
  propogate <- function(w, b, XTrain, YTrain, type = "sigmoid")
  {
    m <- dim(XTrain)[2]
    
    guess <- activation(XTrain %*% w + b, type)
    
    cost <- -(1/m) * sum((t(YTrain) %*% log(guess)) + (1 - t(YTrain)) %*% log(1 - guess))
    
    
    
    dw <- (1/m) * t(XTrain) %*% (guess - YTrain)
    
    db <- (1/m) * sum(guess - YTrain)
    
    return(list("dw" = dw, "db" = db, "cost" = cost))
  }
  
  XTrain <- as.matrix(XTrain)
  YTrain <- as.matrix(YTrain)
  
  w = matrix(0, nrow = dim(XTrain)[2])
  b = 0
  
  vals <- optimize(w, b, XTrain, YTrain, alpha, num_iters)
  pred_Train <- as.matrix(LRMod_predict(vals$w, vals$b, XTrain, raw), nrow = 1)
  accuracy_Train <- 1 - (sum(abs(YTrain - pred_Train)) / length(YTrain))
  
  LRMod <- list("w" = vals$w, "b" = vals$b, "costs" = vals$costs, "is_diff" = raw, "Train_Per" = accuracy_Train, "Train_Vals" = pred_Train)
  
  if(!is.null(XTest) && !is.null(YTest))
  {
    XTest <- as.matrix(XTest)
    YTest <- as.matrix(YTest)  
    pred_Test <- as.matrix(LRMod_predict(vals$w, vals$b, XTest, raw), nrow = 1)
    accuracy_Test <- 1 - (sum(abs(YTest - pred_Test)) / length(YTest))
    LRMod[["Test_Per"]] = accuracy_Test
    LRMod[["Test_Vals"]] = pred_Test
  }
  
  return(LRMod)
}

Predict <- function(LRMod, XTest, YTest, raw = F)
{
  pred_Test <- as.matrix(LRMod_predict(LRMod$w, LRMod$b, XTest, raw), nrow = 1)
  accuracy_Test <- 1 - (sum(abs(YTest - pred_Test)) / length(YTest))
  
  return(list("values" = pred_Test, "Accuracy" = accuracy_Test))
}

LRMod_predict <- function(w, b, XTest, raw = F)
{
  if(!raw)
  {
    return(ifelse(activation(XTest %*% w + b, "sigmoid") > 0.5, 1, 0))
  }
  return(activation(XTest %*% w + b, "sigmoid"))
}

activation <- function(z, type = c("sigmoid", "tanH", "ReLU"), deriv = F, n = 1)
{
  if(!deriv)
  {
    if(type[n] == "sigmoid"){return(1 / (1 + exp(-z)))}
    
    if(type[n] == "tanH"){return(tanh(z))}
    
    if(type[n] == "ReLU"){return(max(0.01*z, z))}
    return(ifelse(z >= 0, 1, 0))
  }else
  {
    if(type[n] == "sigmoid"){return(activation(z, type) * (1 - activation(z, type)))}
    
    if(type[n] == "tanH"){return(1 - tanh(z)^2)}
    
    if(type[n] == "ReLU"){return(max(0.01*z, z)/ifelse(z == 0, 1e-6, z))}
    return(0)
  }
}

#type = c("setosa", "versicolor", "virginica")
LR_Sample <- function(type = "virginica")
{
  train <- sample(150, 100)
  test <- 1:150
  test <- test[!(test %in% train)]
  xTrain <- as.matrix(iris[train, 1:4])
  yTrain <- as.matrix(ifelse(iris[train, 5] %in% type, 1, 0))
  xTest <- as.matrix(iris[test, 1:4])
  yTest <- as.matrix(ifelse(iris[test, 5] %in% type, 1, 0))
  LRMod <- LogisticRegression_Model(XTrain = xTrain, YTrain = yTrain, XTest = xTest, YTest = yTest)
  
  return(list("XTrain" = xTrain, "YTrain" = yTrain, "XTest" = xTest, "YTest" = yTest, "LRModel" = LRMod))
}
