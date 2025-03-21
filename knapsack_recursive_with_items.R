# Code for Dynamic Programming KnapSack problem [with items]

knapSack <- function(W, wt, val, n) {
  if (n == 0 | W == 0) {
    return(list(profit = 0, weights = integer(0)))
  }

  if (wt[n] > W) {
    return(knapSack(W, wt, val, n-1))
  } else {
    without_current_item <- knapSack(W, wt, val, n-1)

    with_current_item <- knapSack(W - wt[n], wt, val, n-1)
    with_current_item$profit <- with_current_item$profit + val[n]
    with_current_item$weights <- c(with_current_item$weights, wt[n])

    if (with_current_item$profit > without_current_item$profit) {
      return(with_current_item)
    } else {
      return(without_current_item)
    }
  }
}

profit <- c(2,3,1,4)
weight <- c(3,4,6,5)
W <- 8
n <- length(profit)

result <- knapSack(W, weight, profit, n)
cat("Maximum Profit KS:", result$profit, "\n")
cat("Selected Weights:", result$weights, "\n")
