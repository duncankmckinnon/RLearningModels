conv2d <- function(imray, filter, padding = 0, stride = 1)
{
  if(padding > 0)
  {
    n <- matrix(0, nrow = (dim(imray)[1]+2*padding), ncol = (dim(imray)[2]+2*padding))
    n[(padding+1):(padding+dim(imray)[1]), (padding+1):(padding+dim(imray)[2])] <- imray
    imray <- n
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

conv3d <- function(imray, filter, padding = 0, stride = 1)
{
  if(stride < 1)
  {
    print("stride length must be > 1")
    stride = 1
  }
  if(padding > 0)
  {
    n <- array(0, dim = c((dim(imray)[1]+2*padding),(dim(imray)[2]+2*padding),dim(imray)[3]))
    n[(padding+1):(padding+dim(imray)[1]), (padding+1):(padding+dim(imray)[2]),1:dim(imray)[3]] <- imray
    imray <- n
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
        z <- c(z, sum(imray[j:(j+nf[1]-1), i:(i+nf[2]-1), 1:dim(imray)[3]] * filter))
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