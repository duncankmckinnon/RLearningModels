

SequenceProb <- function(p)
{
  vals <- vector(length = length(p))
  vals[1] <- p[1]
  for(i in 2:length(p))
  {
    vals[i] <- (vals[i-1] * p[i]) / p[i]
  }
  return(prod(vals))
}