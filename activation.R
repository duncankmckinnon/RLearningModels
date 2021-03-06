
#Non-linear activation functions for determining classifications based on input
activation <- function(z, type = c("sigmoid", "tanh", "relu", "l_relu", "t"), deriv = F, n = 1)
{
  if(!deriv)
  {
    if(type[n] == "sigmoid"){return(g_sigmoid(z))}
    
    if(type[n] == "tanh"){return(g_tanh(z))}
    
    if(type[n] == "relu"){return(g_relu(z))}
    
    if(type[n] == "l_relu"){return(g_relu(z, leaky = T))}
    
    if(type[n] == "t"){return(g_t(z))}
    return(ifelse(z >= 0, 1, 0))
  }else
  {
    if(type[n] == "sigmoid"){return(g_sigmoid(z, deriv = T))}
    
    if(type[n] == "tanh"){return(g_tanh(z, deriv = T))}
    
    if(type[n] == "relu"){return(g_relu(z, deriv = T))}
    
    if(type[n] == "l_relu"){return(g_relu(z, leaky = T))}
    return(0)
  }
}

g_sigmoid <- function(z, deriv = F)
{
  if(deriv)
  {
    return(g_sigmoid(z) * (1 - g_sigmoid(z)))
  }
  return(1 / (1 + exp(-z)))
}

g_tanh <- function(z, deriv = F)
{
  if(deriv)
  {
    return(1 - tanh(z)^2)
  }
  return(tanh(z))
}

g_relu <- function(z, deriv = F, thresh = 0, leaky = T)
{
  if(deriv)
  {
    return(g_relu(z, thresh = thresh, leaky = leaky)/ifelse(z == 0, 1e-6, z))
  }
  return(ifelse(z >= thresh, z, ifelse(leaky, 0.01*z, 0)))
}

g_t <- function(z, deriv = F)
{
  return(exp(z)/rowSums(exp(z)))
}

softmax <- function(z)
{
  z_t <- sum(g_sigmoid(z))
  z_i <- vector(length = length(z))
  for(i in 1:length(z))
  {
    z_i[i] <- g_sigmoid(z[i])/z_t
  }
  return(z_i)
}