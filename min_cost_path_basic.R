# Code for Dynamic Programming MinCostPath problem [without path]

MinCostPath <- function(A) {
  rows <- nrow(A)
  cols <- ncol(A)
  solution <- matrix(0, nrow = rows, ncol = cols)
  solution[1, 1] <- A[1, 1]
  for (i in 2:cols) {
    solution[1, i] <- A[1, i] + solution[1, i - 1]
  }
  for (i in 2:rows) {
    solution[i, 1] <- A[i, 1] + solution[i - 1, 1]
  }
  for (i in 2:rows) {
    for (j in 2:cols) {
      solution[i, j] <- A[i, j] + min(solution[i - 1, j], solution[i, j - 1])
    }
  }
  return(solution[rows, cols])
}

A <-    matrix(c(9, 4, 9, 9,
                 6, 7, 6, 4,
                 8, 3, 3, 7,
                 7, 4, 9, 10), nrow = 4, ncol = 4, byrow = TRUE)
                 
cat("Total Minimum Cost MCP:", MinCostPath(A) , "\n")
