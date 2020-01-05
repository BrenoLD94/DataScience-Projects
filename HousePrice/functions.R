# FUNCTIONS
deleteColumns <- function(x, columns){
  for(col in columns){
    x[col] <- NULL
  }
  return(x)
}

imputationByMeanMedian <- function(x, Mean, Median, seed = 100){
  set.seed(seed)
  for(i in 1:length(x))
  {
    if(is.na(x[i]))
    {
      if(runif(1, 0, 1) <= 0.5)
        x[i] <- Median
      else
        x[i] <- Mean
    }
  }
  
  return(x)
}

imputationByMedian <- function(x, Median){
  if(is.na(x))
    return(Median)
  else
    return(x)
}

fillNA <- function(x, column, c, p){
  tot <- dim(x)[1]
  
  for(index in 1:tot){
    if(is.na(x[index, column])){
      x[index, column] <- as.factor(sample(c, size=1, replace=F, prob=p))
    }
  }
  return(x[, column])
}

CategoricalGraphsByPrice <- function(dataFrame, X = "SalePrice", y, xlab = "Price(10^3)", size=5, bin=5){
  ggplot(dataFrame, aes(x = (dataFrame[, X]/1000)))  +
    geom_histogram(alpha = 0.9, binwidth = bin) + 
    xlab("Price(10^3)") +
    labs(title=paste(y, " by Price")) + 
    facet_wrap(~ dataFrame[, y], ncol = size)
}

factorToNumeric <- function(x){
  if(is.factor(x)){
    Names <- levels(x)  
  }
  else if(is.array(x)){
    Names <- names(x)
  }
  size <- length(x)
  num <- seq(0, size-1)
  names(num) <- Names
  i = 1
  new_x = c()
  for(val in x){
    new_x[i] <- num[val]
    i = i + 1
  }
  return(new_x)
}

normalization <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}


infoNormality <- function(x, col, rows=1, cols=2){
  par(mfrow = c(rows, cols))
  hist(x[, col], xlab = col, main=col)
  qqnorm(x[, col]) 
  qqline(x[, col])
  print(paste(paste(col, "has skewness rate = "), skewness(x[, col])))
  print(paste(paste(col, "has kurtosis rate = "), kurtosis(x[, col])))
}
