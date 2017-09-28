

#NumPy style matrix addition with broadcasting
`%+%` <- function(m1, m2)
{
  require('assertthat')
  assertthat::assert_that(is.matrix(m1), 
                          is.matrix(m2),
                          dim(m1)[1] == dim(m2)[1] | dim(m2)[2] == dim(m2)[2])
  
 
  len1 <- dim(m1)
  len2 <- dim(m2)
  if(all(len1 == len2))
  {
    return(m1 + m2)
  }
  else if(all(len1 %in% len2) && all(len2 %in% len1))
  {
    return(m1 + t(m2))
  }
  
  iters <- 1
  dimV <- 1
  longer <- m1
  shorter <- m2
  if(dim(m1)[1] == dim(m2)[1])
  {
    if(len1[2] > len2[2])
    {
      longer <- m1
      shorter <- m2
    }
    else
    {
      longer <- m2
      shorter <- m1
    }
    assertthat::assert_that(dim(shorter)[2] == 1)
    
    iters <- dim(longer)[2]
    dimV <- 2
  }
  else
  {
    if(len1[1] > len2[1])
    {
      longer <- m1
      shorter <- m2
    }
    else
    {
      longer <- m2
      shorter <- m1
    }
    assertthat::assert_that(dim(shorter)[1] == 1)
    
    iters <- dim(longer)[1]
    dimV <- 1
    
  }
  
  retVal <- matrix(0, dim(longer)[1], dim(longer)[2])
  
  for(i in 1:iters)
  {
    if(dimV == 2)
    {
      retVal[,i] <- longer[,i] + shorter
    }
    else
    {
      retVal[i,] <- longer[i,] + shorter
    }
  }
  return(retVal)
}