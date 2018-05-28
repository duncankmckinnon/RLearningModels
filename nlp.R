#Recurrent NN Tools
#Duncan McKinnon

require('stringr')
require('hash')

vocab2Dict <- function(vocab)
{
  v <- str_to_lower(vocab)
  v <- sort(union(v, v))
  i <- 1:length(v)
  return(hash(v, i))
}

chars2Dict <- function()
{
  i <- 1:36
  v <- str_split('abcdefghijklmnopqrstuvwxyz0123456789', boundary('character'))[[1]]
  return(hash(v, i))
}

sentence2Words <- function(sentence)
{
  return(str_split(str_to_lower(sentence), boundary('word'))[[1]])
}

word2chars <- function(word)
{
  return(str_split(str_to_lower(word), boundary('character'))[[1]])
}

updateDict <- function(dict, items)
{
  v <- sort(union(keys(dict), str_to_lower(items)))
  i <- 1:length(v)
  return(hash(v, i))
}

words2Matrix <- function(words, dict = hash(), updateDict = TRUE)
{
  if(updateDict)
  {
    dict <- updateDict(dict, words)
  }
  nw <- length(words)
  nd <- length(dict)
  m <- matrix(0, nrow = nd, ncol = nw)
  for(i in 1:nw)
  {
    word <- words[i]
    m[dict[[word]], i] <- 1
  }
  return(list('words_matrix' = m, 'dictionary' = dict))
}

chars2Matrix <- function(words)
{
  chars <- c()
  if(length(words) == 1){
    words <- sentence2Words(words)
  }
  for(i in words){
    chars <- c(chars, word2chars(i))
  }
  dict <- chars2Dict()
  nc <- length(chars)
  nd <- length(dict)
  m <- matrix(0, nrow = nd, ncol = nc)
  for(i in 1:nc)
  {
    char <- chars[i]
    m[dict[[char]], i] <- 1
  }
  return(list('character_matrix' = m, 'dictionary' = dict))
}

sentence2Matrix <- function(sentence, dict = hash(), updateDict = TRUE)
{
  words <- sentence2Words(sentence)
  return(words2Matrix(words, dict, updateDict))
}

documentEmbedding <- function(document)
{
  sentences <- str_split(document, boundary('sentence'))
  docStripped <- str_remove_all(document, '[^a-zA-z0-9 ]')
  vocab <- sentence2Words(docStripped)
  docDict <- vocab2Dict(vocab)
  doc <- list()
  for(i in 1:length(sentences[[1]]))
  {
    doc[[i]] <- sentence2Matrix(sentences[[1]][i], docDict, FALSE)
  }
  return(doc)
}


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

SimilarityF <- function(u,v, type='cos')
{
  if(length(v) == length(u))
  {
    n <- -1
    if(type == 'cos')
    {
      n <- as.vector(t(u)%*%v)/(norm(u,"2")*norm(v,"2"))
    }
    return(sum(n)/length(n))
  }
  return(-1)
}

plot_one_hot <- function(one_hot, type="p")
{
  n <- 1:dim(one_hot)[2]
  m <- which(one_hot==1, arr.ind = TRUE)[,1]
  plot(n, m, type = type)
}

