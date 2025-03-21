# Code for Dynamic Programming TravelSalesMan problem [without path]

MAX <- 999999
TSM <- function(mask, pos, graph, dp, n, visited) {
  if (mask == visited) {
    return(graph[pos + 1, 1])
  }
  
  if (dp[mask + 1, pos + 1] != -1) {
    return(dp[mask + 1, pos + 1])
  }
  
  ans <- MAX
  for (city in 1:n) {
    if ((bitwAnd(mask, 2^(city - 1))) == 0) {
      new <- graph[pos + 1, city] + TSM(bitwOr(mask, 2^(city - 1)), city - 1, graph, dp, n, visited)
      ans <- min(ans, new)
    }
  }
  
  dp[mask + 1, pos + 1] <- ans
  return(ans)
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

cat("Total Minimum cost TSM:", TSM(1, 0, graph, dp, n, visited))
