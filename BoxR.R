makeBox <- function(x1, y1, x2, y2)
{
  return(list('type' = 'Box', 'P1' = list('x' = x1, 'y' = y1), 'P2' = list('x' = x2, 'y' = y2)))
}

areaBox <- function(box)
{
  return(abs((box$P2$x - box$P1$x) * (box$P2$y - box$P1$y)))
}

interBoxes <- function(boxA, boxB)
{
  x1 <- max(boxA$P1$x, boxB$P1$x)
  y1 <- max(boxA$P1$y, boxB$P1$y)
  x2 <- min(boxA$P2$x, boxB$P2$x)
  y2 <- min(boxA$P2$y, boxB$P2$y)
  
  boxC <- makeBox(x1,y1,x2,y2)
  return(areaBox(boxC))
}

unionBoxes <- function(boxA, boxB)
{
  areaC <- interBoxes(boxA, boxB)
  areaA <- areaBox(boxA)
  areaB <- areaBox(boxB)
  return((areaA + areaB - areaC))
}

IoU <- function(boxA, boxB)
{
  intr <- interBoxes(boxA, boxB)
  uni <- unionBoxes(boxA, boxB)
  return(intr / uni)
}

plotBoxes <- function(boxList)
{
  Xpoints <- c()
  Ypoints <- c()
  n <- 0
  for(i in boxList)
  {
    box <- i
    n <- n+1
    Xpoints <- c(Xpoints,box$P1$x, box$P1$x, box$P2$x, box$P2$x, box$P1$x)
    Ypoints <- c(Ypoints, box$P1$y, box$P2$y, box$P2$y, box$P1$y, box$P1$y)
  }
  yMax <- max(Ypoints)
  xMax <- max(Xpoints)
  yMin <- min(Ypoints)
  xMin <- min(Xpoints)
  matplot(0,0, ylim = c(yMin-1, yMax+1), xlim = c(xMin-1, xMax+1))
  Xs <- matrix(Xpoints, ncol = n)
  Ys <- matrix(Ypoints, ncol = n)
  matlines(Xs, Ys)
}