conv2d <- function(img, filter, padding = 0, stride = 1)
{
  if(padding > 0)
  {
    n <- matrix(0, nrow = (dim(img)[1]+2*padding), ncol = (dim(img)[2]+2*padding))
    n[(padding+1):(padding+dim(img)[1]), (padding+1):(padding+dim(img)[2])] <- img
    img <- n
  }
  nx <- dim(filter)
  ny <- dim(img)
  n <- 1
  m <- 1
  z <- c()
  for(i in seq(1, (ny[2]-nx[2] + 1), stride))
  {
    for(j in seq(1, (ny[1]-nx[1] + 1), stride))
    {
      if((j+nx[1]-stride < ny[1]) && (i+nx[2]-stride < ny[2])) 
      {
        z <- c(z, sum(img[j:(j+nx[1]-1), i:(i+nx[2]-1)] * filter))
      }
    }
  }
  
  suppressWarnings(
    {
      mz <- matrix(z, nrow = floor((ny[1]-nx[1])/stride + 1), ncol = floor((ny[2]-nx[2])/stride+ 1))
    }
  )
  return(mz)
}

conv3d <- function(img, filter, padding = 0, stride = 1)
{
  if(stride < 1)
  {
    print("stride length must be > 1")
    stride = 1
  }
  if(padding > 0)
  {
    n <- array(0, dim = c((dim(img)[1]+2*padding),(dim(img)[2]+2*padding),dim(img)[3]))
    n[(padding+1):(padding+dim(img)[1]), (padding+1):(padding+dim(img)[2]),1:dim(img)[3]] <- img
    img <- n
  }
  nx <- dim(filter)
  ny <- dim(img)
  n <- 1
  m <- 1
  z <- c()
  for(i in seq(1, (ny[2]-nx[2] + 1), stride))
  {
    for(j in seq(1, (ny[1]-nx[1] + 1), stride))
    {
      if((j+nx[1]-stride < ny[1]) && (i+nx[2]-stride < ny[2])) 
      {
        z <- c(z, sum(img[j:(j+nx[1]-1), i:(i+nx[2]-1), 1:dim(img)[3]] * filter))
      }
    }
  }
  
  suppressWarnings(
    {
      mz <- matrix(z, nrow = floor((ny[1]-nx[1])/stride + 1), ncol = floor((ny[2]-nx[2])/stride+ 1))
    }
  )
  return(mz)
}