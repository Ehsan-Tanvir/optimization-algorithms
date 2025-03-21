# Code for Dynamic Programming TravelSalesMan problem [with path]

MAX <- 999999

TSM <- function(mask, pos, graph, dp, n, visited, path) {
  if (mask == visited) {
    return(list(cost = graph[pos + 1, 1], path = c(path, 1)))
  }
  
  if (dp[mask + 1, pos + 1] != -1) {
    return(list(cost = dp[mask + 1, pos + 1], path = path))
  }
  
  ans <- MAX
  best_path <- NULL

  for (city in 1:n) {
    if ((bitwAnd(mask, 2^(city - 1))) == 0) {
      result <- TSM(bitwOr(mask, 2^(city - 1)), city - 1, graph, dp, n, visited, c(path, city))
      new <- graph[pos + 1, city] + result$cost

      if (new < ans) {
        ans <- new
        best_path <- result$path
      }
    }
  }
  
  dp[mask + 1, pos + 1] <- ans
  return(list(cost = ans, path = best_path))
}

graph <- matrix(c(0, 10, 15, 20,
                  10, 0, 35, 25,
                  15, 35, 0, 30,
                  20, 25, 30, 0), nrow = 4, byrow = TRUE)

n <- 4
visited <- (2^n) - 1
r <- 16
c <- 4

dp <- matrix(-1, nrow = r, ncol = c)

for (i in 0:(2^n - 1)) {
  for (j in 1:n) {
    dp[i + 1, j] <- -1
  }
}

result <- TSM(1, 0, graph, dp, n, visited, numeric(0))
cat("Total Minimum cost TSM:", result$cost, "\n")
cat("Path:", result$path, "\n")
