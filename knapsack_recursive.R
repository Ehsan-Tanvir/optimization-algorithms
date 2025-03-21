# Code for Dynamic Programming KnapSack problem [without items]

knapSack <- function(W, wt, val, n) {
  if (n == 0 | W == 0) {
    return(0)
  }

  if (wt[n] > W) {
    return(knapSack(W, wt, val, n-1))
  } else {
    return(max(
      val[n] + knapSack(W-wt[n], wt, val, n-1),
      knapSack(W, wt, val, n-1)
    ))
  }
}

profit <- c(60, 100, 120)
weight <- c(10, 20, 30)
W <- 50
n <- length(profit)

result <- knapSack(W, weight, profit, n)
cat("Maximum Profit KS:", result, "\n")
