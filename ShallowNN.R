#Neural Network Model
#Duncan McKinnonx


NeuralNetwork_Model <- function(XTrain, YTrain, n_h = 4, alpha = 0.01, num_iters = 10, type = "tanH", raw_diff = T, XTest = NULL, YTest = NULL)
{
#internal model function to perform gradient descent optimization on weights and offset
  optimize <- function(w, b, XTrain, YTrain, alpha, num_iters, type)
  {
    costs <- c()
    for(i in 1:num_iters)
    {
      vals <- propogate(w, b, XTrain, YTrain, type)
      w[[1]] = w[[1]] - (alpha * vals$dw[1])
      b[[1]] = b[[1]] - (alpha * vals$db[1])
      w[[2]] = w[[2]] - (alpha * vals$dw[2])
      b[[2]] = b[[2]] - (alpha * vals$db[2])
      
      costs <- c(costs, vals$cost)
    }
    return(list("w" = w, "b" = b, "dw" = vals$dw,  "db" = vals$db,  "costs" = costs))
  }

#internal model function to perform forward propogation to get estimates based on current weights and offset
# and back propogation for next optimization step  
  propogate <- function(w, b, XTrain, YTrain, type)
  {
    m <- dim(XTrain)[2]
    
    z1 <- (w[[1]] %*% XTrain) %+% b[[1]]
    
    a1 <- activation(z1, type)
    
    z2 <- (w[[2]] %*% a1) %+% b[[2]]
    
    a2 <- activation(z2, type)
    
    cost <- -(1/m) * sum((YTrain - t(a2))^2)
    
    dz2 <- a2 - t(YTrain)
    
    dw2 <- (1/m) * dz2 %*% t(a1)
    
    db2 <- (1/m) * colSums(t(dz2))
    
    dz1 <- (t(w[[2]]) %*% dz2) * activation(z1, type, T)
    
    dw1 <- (1/m) * dz1 %*% t(XTrain)
    
    db1 <- (1/m) * colSums(t(dz1))
    
    return(list("dw" = c(dw1, dw2), "db" = c(db1, db2), "cost" = cost))
  }
  
#initialization of variables for training phase
  XTrain <- t(as.matrix(XTrain))
  YTrain <- as.matrix(YTrain)

 #number of inner layer dimensions  
  n <- c(dim(XTrain)[1], n_h, dim(YTrain)[2])
  
  
  w <- list()
  b <- list()
  
  #initialize 2 levels of weights and offsets
  for(i in 2:length(n))
  {
    w[[i-1]] <- matrix((sample(100, n[i-1] * n[i], T) - 50) * 0.01 , n[i], n[i-1])
    b[[i-1]] <- matrix((sample(100, n[i], T) - 50) * 0.01, n[i], 1)
  }
  
  
#run gradient descent optimization  
  vals <- optimize(w, b, XTrain, YTrain, alpha, num_iters, type)
  
#get predictions and accuracy for training examples
  pred_Train <- as.matrix(NNModel_predict(vals$w, vals$b, XTrain, type, raw_diff), nrow = 1)
  accuracy_Train <- sum(t(YTrain) - pred_Train) / length(YTrain)
    
  NNModel <- list("w" = vals$w, "b" = vals$b, "costs" = vals$costs, "activation" = type, "is_diff" = raw_diff, "Train_Per" = accuracy_Train, "Train_Vals" = pred_Train)
  
#get predictions and accuracy for testing examples
  if(!is.null(XTest) && !is.null(YTest))
  {
    XTest <- t(as.matrix(XTest))
    YTest <- as.matrix(YTest)
    pred_Test <- as.matrix(NNModel_predict(vals$w, vals$b, XTest, type, raw_diff), nrow = 1)
    accuracy_Test <- sum(t(YTest) - pred_Test) / length(YTest)
    NNModel[["Test_Per"]] = accuracy_Test
    NNModel[["Test_Vals"]] = pred_Test 
  }
  return(NNModel)
}

#Run existing model against a new dataset
Predict <- function(NNModel, XTest, YTest)
{
  pred <- NNModel_predict(NNModel$w, NNModel$b, XTest, YTest, NNModel$activation, NNModel$is_diff)
  accuracy_Test <- sum(t(YTest) - pred_Test) / length(YTest)
  predModel <- list("Values" = pred, "Accuracy" = accuracy_Test)
}

#Get prediction results for a set of parameters and data
NNModel_predict <- function(w, b, XTest, type, raw_diff = F)
{
  z1 <- (w[[1]] %*% XTest)  %+% b[[1]]
  
  a1 <- activation(z1 , type)
  
  z2 <- (w[[2]] %*% a1) %+% b[[2]]
  
  a2 <- activation(z2, type)
  
  if(raw_diff)
  {
    a2 <- ifelse(a2 > 0.5, 1, 0)
  }
  
  return(a2)
}

#Non-linear activation functions for determining classifications based on input
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


#Generate a sample model trained to recognize the type of flower in the iris sample set.
#type = c("setosa", "versicolor", "virginica")
NN_Sample <- function(type = "virginica")
{
  train <- sample(150, 100)
  test <- 1:150
  test <- test[!(test %in% train)]
  xTrain <- as.matrix(iris[train, 1:4])
  yTrain <- as.matrix(ifelse(iris[train, 5] %in% type, 1, 0))
  xTest <- as.matrix(iris[test, 1:4])
  yTest <- as.matrix(ifelse(iris[test, 5] %in% type, 1, 0))
  NNMod <- NeuralNetwork_Model(XTrain = xTrain, YTrain = yTrain, XTest = xTest, YTest = yTest, raw_diff = T, type = "sigmoid")
  
  return(list("XTrain" = xTrain, "YTrain" = yTrain, "XTest" = xTest, "YTest" = yTest, "NN_Sample" = NNMod))
}
