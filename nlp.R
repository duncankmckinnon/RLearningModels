#Recurrent NN Tools
#Duncan McKinnon

require(stringr)
require(hash)

vocab2Dict <- function(vocab)
{
  v <- str_to_lower(vocab)
  v <- sort(union(v, v))
  i <- 1:length(v)
  return(hash(v, i))
}

sentence2Words <- function(sentence)
{
  return(str_split(str_to_lower(sentence), boundary('word'))[[1]])
}

updateDict <- function(dict, items)
{
  v <- sort(union(keys(dict), str_to_lower(items)))
  i <- 1:length(v)
  return(hash(v, i))
}

words2Matrix <- function(words, dict)
{
  dict <- updateDict(dict, words)
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

