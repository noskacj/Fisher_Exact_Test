fisher_exact <- function(iter, y1, y2, test = "two sided"){
  actual_t <- mean(y1) - mean(y2)
  
  n1 <- length(y1)
  n2 <- length(y2)
  
  y <- c(y1, y2)
  
  n <- n1 + n2
  
  t_rep <- vector(mode = "double", length = iter)
  
  for (i in 1:iter){
    y1_new_index <- base::sample(1:n, size = n1, replace = FALSE)
    
    y1_new <- y[y1_new_index]
    y2_new <- y[-y1_new_index]
    
    t_rep[i] <- mean(y1_new) - mean(y2_new)
  }
  
  if (test == "two sided"){
    return(sum(abs(t_rep) > abs(actual_t)) / iter)
  }
  
  else if (test == "greater"){
    return(sum(t_rep > actual_t) / iter)
  }
  
  else if (test == "less"){
    return(sum(t_rep < actual_t) / iter)
  }
  
  else {
    print("Test type specified incorrectly")
  }
}


x1 <- rnorm(1000, sd = 2) + 0.1
x2 <- rnorm(1000, sd = 2)

fisher_exact(100000, x1, x2, test = "greater")

t.test(x1, x2, alternative = "greater")
