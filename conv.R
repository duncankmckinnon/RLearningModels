#Convolutional NN Tools
#Duncan McKinnon

#Convolutional NN tools using R Arrays

conv1d <- function(imray, filter, padding = 0, stride = 1, sameConv = FALSE)
{
  if(sameConv)
  {
      padding <- ceiling((stride * (dim(imray)[1] - 1) + dim(filter)[1] - dim(imray)[1])/2)
  }
  if(padding[1] > 0)
  {
    imray <- addPad(imray, padding)
  }
  nf <- dim(filter)[1]
  nx <- dim(imray)[1]
  len <- nx - nf + 1
  z <- c()
  for(i in seq(1, len, stride))
  {
    if((i+nf-stride < nx)) 
    {
      z <- c(z, sum(imray[i:(i+nf-1)] * filter))
    }
  }
  suppressWarnings(
    {
      mz <- array(z, dim = length(z))
    }
  )
  return(mz)
}

conv2d <- function(imray, filter, padding = 0, stride = 1, sameConv = FALSE)
{
  if(sameConv)
  {
    if(dim(imray)[1] == dim(imray)[2] | length(padding) == 1)
    {
      padding <- ceiling((stride * (dim(imray)[1] - 1) + dim(filter)[1] - dim(imray)[1])/2)
    }
    else
    {
      padding <- c(ceiling((stride * (dim(imray)[1] - 1) + dim(filter)[1] - dim(imray)[1])/2), ceiling((stride * (dim(imray)[2] - 1) + dim(filter)[2] - dim(imray)[2])/2))
    }
  }
  if(padding[1] > 0)
  {
    imray <- addPad(imray, padding)
  }
  nf <- dim(filter)
  nx <- dim(imray)
  z <- c()
  for(i in seq(1, (nx[2]-nf[2] + 1), stride))
  {
    for(j in seq(1, (nx[1]-nf[1] + 1), stride))
    {
      if((j+nf[1]-stride < nx[1]) && (i+nf[2]-stride < nx[2])) 
      {
        z <- c(z, sum(imray[j:(j+nf[1]-1), i:(i+nf[2]-1)] * filter))
      }
    }
  }
  suppressWarnings(
    {
      mz <- matrix(z, nrow = floor((nx[1]-nf[1])/stride + 1), ncol = floor((nx[2]-nf[2])/stride+ 1))
    }
  )
  return(mz)
}


conv3d <- function(imray, filter, padding = 0, stride = 1, sameConv = FALSE)
{
  if(stride < 1)
  {
    print("stride length must be > 1")
    stride = 1
  }
  
  nf <- dim(filter)
  nx <- dim(imray)
  
  stopifnot(nx[3] == nf[3])

  if(sameConv)
  {
    if(nx[1] == nx[2])
    {
      padding <- ceiling((stride * (nx[1] - 1) + nf[1] - nx[1])/2)
    }
    else
    {
      padding <- c(ceiling((stride * (nx[1] - 1) + nf[1] - nx[1])/2), ceiling((stride * (nx[2] - 1) + nf[2] - nx[2])/2))
    }
  }
  if(padding[1] > 0)
  {
    imray <- addPad(imray, padding)
    nx <- dim(imray)
  }
  
  z <- c()
  
  if(length(nf) == 3)
  {
    nf <- c(nf, 1)
  }
  for(i in 1:nf[4])
  {
    for(j in seq(1, (nx[2]-nf[2] + 1), stride))
    {
      for(k in seq(1, (nx[1]-nf[1] + 1), stride))
      {
        if((k+nf[1]-stride < nx[1]) && (j+nf[2]-stride < nx[2])) 
        {
          z <- c(z, sum(imray[k:(k+nf[1]-1), j:(j+nf[2]-1), 1:nx[3]] * filter[1:nf[1], 1:nf[2],1:nx[3], i]))
        }
      }
    }
  }
  suppressWarnings(
    {
      mz <- array(z, dim = c(floor((nx[1]-nf[1])/stride + 1), floor((nx[2]-nf[2])/stride+ 1), nf[4]))
    }
  )
  return(mz)
}


conv <- function(imray, filters, padding = 0, stride = 1)
{
  z <- list()
  df <- dim(filters)
  nf <- length(df)
  ni <- length(dim(imray))
  n <- df[nf]
  for(i in 1:n)
  {
    if(nf == 2)
    {
      z[[i]] <- conv1d(imray, filters[,i], padding, stride)
    }
    if(nf == 3)
    {
      z[[i]] <- conv2d(imray, filters[,,i], padding, stride)
    }
    else if(nf == 4)
    {
      z[[i]] <- conv3d(imray, filters[,,,i], padding, stride)
    }
  }
  if(nf > 2)
  {
    return(array(unlist(z), dim = c(dim(z[[1]])[1], dim(z[[1]])[2], n)))
  }
  else
  {
    return(array(unlist(z), dim = c(dim(z[[1]])[1], n)))
  }
}


pooling <- function(imray, type = "max", filterSize = c(3,3), stride = 1)
{
  if(stride < 1)
  {
    print("stride length must be > 1")
    stride = 1
  }
  nx <- dim(imray)
  nf <- filterSize
  
  stopifnot(nx[3] == nf[3])
  
  if(length(nf) == length(nx))
  {
    filterSize <- c(filterSize, 1)
    nf <- filterSize
  }
  
  z <- c()
  for(h in 1:nf[4])
  {
    for(i in seq(1, (nx[2]-nf[2] + 1), stride))
    {
      for(j in seq(1, (nx[1]-nf[1] + 1), stride))
      {
        if((j+nf[1]-stride < nx[1]) && (i+nf[2]-stride < nx[2])) 
        {
          m = 0
          if(length(nx) == 3)
          {
            if(type == "max")
            {
              m <- max(imray[j:(j+nf[1]-1), i:(i+nf[2]-1), 1:nx[3]])
            }
            else if(type == "avg")
            {
              m <- mean(imray[j:(j+nf[1]-1), i:(i+nf[2]-1), 1:nx[3]])
            }
            z <- c(z, m)
          }
          else if(length(nx) == 2)
          {
            if(type == "max")
            {
              m <- max(imray[j:(j+nf[1]-1), i:(i+nf[2]-1), 1])
            }
            else if(type == "avg")
            {
              m <- mean(imray[j:(j+nf[1]-1), i:(i+nf[2]-1), 1])
            }
            z <- c(z, m)
          }
        }
      }
    }
  }
  suppressWarnings(
    {
      if(filterSize[3] == 1)
      {
        mz <- matrix(z, nrow = floor((nx[1]-nf[1])/stride + 1), ncol = floor((nx[2]-nf[2])/stride+ 1))
      }
      else
      {
        mz <- array(z, dim = c(floor((nx[1]-nf[1])/stride + 1), floor((nx[2]-nf[2])/stride+ 1), filterSize[4]))
      }
    }
  )
  return(mz)
}


maxMask <- function(X)
{
  if(is.array(X))
  {
    m <- dim(X)
  }
  else
  {
    m <- length(X)
  }
  return(array(as.numeric(X == max(X)), dim = c(m)))
}


addPad <- function(data, pad)
{
  if(is.array(data))
  {
    dims = dim(data)
    dlen = length(dims)
    if(dlen == 1)
    {
      n <- array(0, dim = c(dims[1] + 2 * pad))
      n[(pad+1):(pad+dims[1])] <- data
      data <- n
    }
    else if(dlen == 2)
    {
      if(dims[1] == dims[2] | length(pad) == 1)
      {
        padA <- pad
        padB <- pad
      }
      else
      {
        padA <- pad[1]
        padB <- pad[2]
      }
      n <- array(0, dim = c(dims[1] + 2 * padA, dims[2] + 2 * padB))
      n[(padA+1):(padA+dims[1]), (padB+1):(padB+dims[2])] <- data
      data <- n
    }
    else if(dlen == 3)
    {
      if(dims[1] == dims[2] | length(pad) == 1)
      {
        padA <- pad
        padB <- pad
      }
      else
      {
        padA <- pad[1]
        padB <- pad[2]
      }
      n <- array(0, dim = c(dims[1] + 2 * padA, dims[2] + 2 * padB, dims[3]))
      n[(padA+1):(padA+dims[1]), (padB+1):(padB+dims[2]), 1:dims[3]] <- data
      data <- n
    }
  }
  return(data)
}


flatten <- function(imray)
{
  return(array(imray, dim = length(imray)))
}

